open Core_kernel.Std
open Bap.Std
open Bap_traces.Std
open Trace

module Dis = Disasm_expert.Basic
module Report = Veri_report
module Diff = Report.Diff

module type V = sig
  type t
  val create : Trace.t -> t
  val until_mismatch: t -> t option
  val execute: Trace.t -> t
  val find: Trace.t -> string -> Report.Record.t option
  val report: t -> Report.t
end

module Verification(T : Veri_types.T) = struct

  module Context = Veri_context.Make(T) 

  (** type compare_point describes a piece of program trace
      as raw code and list of side effects that this code
      should perform. *)
  type compare_point = {
    code : Chunk.t;
    side : event list;
  }

  type events_reader =
    | Started of event * (event Seq.t)
    | Finished

  type t = {
    events  : events_reader;
    report  : Report.t;
    context : Context.t;
  }

  let endian = Arch.endian T.arch

  let lift_insn (mem,insn) = match T.lift mem insn with
    | Ok b -> Some b
    | Error _ -> None 

  let create trace =
    let events = match Seq.next (Trace.events trace) with
      | Some (ev, evs) -> Started (ev, evs)
      | None -> Finished in
    let context = Context.create () in
    let report  = Report.create () in
    {events; context; report;}

  let report t = t.report

  let insns_of_mem dis mem = 
    let open Or_error in
    let rec loop insns mem =
      Dis.insn_of_mem dis mem >>= (fun (imem, insn, left) ->
          let insns' = match insn with 
            | Some insn -> (imem, insn) :: insns 
            | None -> insns in
          match left with
          | `left mem -> loop insns' mem 
          | `finished -> Ok (List.rev insns')) in 
    loop [] mem

  let insns_of_chunk chunk =
    let open Or_error in
    Dis.with_disasm ~backend:"llvm" (Arch.to_string T.arch)
      ~f:(fun dis ->
          let dis = Dis.store_kinds dis |> Dis.store_asm in
          let mems = Bigstring.of_string (Chunk.data chunk) in
          Memory.create endian (Chunk.addr chunk) mems >>=
          fun mem -> insns_of_mem dis mem)

  let move_cell ev = Move.cell ev
  let move_data ev = Move.data ev

  let update_reg ctxt reg_event = 
    Context.update_var ctxt (move_cell reg_event) (move_data
    reg_event)

  let update_mem ctxt mem_event =
    Context.update_mem ctxt (move_cell mem_event) (move_data
    mem_event)

  let eval_event ctxt event =
    let open Trace in
    Value.Match.(begin
        select @@
        case Event.register_write (fun m -> update_reg ctxt m) @@
        case Event.register_read (fun m -> update_reg ctxt m) @@
        case Event.memory_load (fun m -> update_mem ctxt m) @@
        case Event.memory_store (fun m -> update_mem ctxt m) @@
        default (fun () -> ())
      end) event

  let update_histo t record = function 
    | [] -> ()
    | insns -> 
      let (_, insn) = List.hd_exn (List.rev insns) in      
      let name = Insn.(name (of_basic insn)) in
      Report.succ_wrong t.report name record

  let is_init_event context ev = 
    Value.Match.(
      select @@
      case Event.register_read 
        (fun m -> 
           let var = Move.(cell m) in
           not (Context.exists_var context var)) @@
      case Event.memory_load 
        (fun m -> 
           let addr = Move.(cell m) in
           not (Context.exists_mem context addr)) @@
      default (fun () -> false)) ev
    
  let init_stage t point = 
    let evs = List.filter ~f:(is_init_event t.context) point.side in
    List.iter evs ~f:(eval_event t.context)

  let perform_compare t ~verbose point =
    match insns_of_chunk point.code with
    | Error _ -> {t with report = Report.succ_undef t.report}
    | Ok insns -> 
      let bil = List.filter_map ~f:lift_insn insns |> List.concat in
      let () = init_stage t point in
      let exec_ctxt = Context.to_bili_context t.context in
      let exec_ctxt' = Stmt.eval bil exec_ctxt in
      let () = List.iter ~f:(eval_event t.context) point.side in
      if Context.is_different t.context exec_ctxt' then       
        let record =
          if verbose then
            let diff = Context.diff t.context exec_ctxt' in
            Some (Report.Record.create point.code exec_ctxt diff)
          else None in
        update_histo t record insns;
        t
      else {t with report = Report.succ_right t.report}

  let next_compare_point reader = 
    let is_code = Value.is Event.code_exec in
    let get_code_exn = Value.get_exn Event.code_exec in
    let make_point code side = match code with
      | Some code -> Some {code = get_code_exn code; side;} 
      | None -> None in
    let rec run code side events = match Seq.next events with 
      | None -> make_point code (List.rev side), Finished
      | Some (event, events') -> 
        if is_code event then
          match code with 
          | None -> run (Some event) side events'
          | Some code as code' -> 
            let comp_point = make_point code' (List.rev side) in
            comp_point, Started (event, events')
        else run code (event::side) events' in
    match reader with 
    | Finished -> None, reader
    | Started (ev, evs) -> 
      if is_code ev then run (Some ev) [] evs
      else run None [ev] evs

  (** [step t] - processes such number events from trace, that are needed
      to get next compare result. And returns None if number events in a 
      trace is not enough to do it. *)
  let step t = 
    let p,events = next_compare_point t.events in
    let t' = {t with events} in
    match p with
    | Some p -> Some (perform_compare t' ~verbose:false p) 
    | None -> None

  let until_mismatch t = 
    let start = Report.wrong t.report in
    let rec run t =
      let p, events = next_compare_point t.events in
      let t' = {t with events} in
      match p with
      | None -> None 
      | Some p -> 
        let t' = perform_compare t' ~verbose:true p  in
        if start <> Report.wrong t'.report then Some t'
        else run t' in
    run t

  let execute trace = 
    let t = create trace in
    let rec run t = match step t with 
      | None -> t
      | Some t' -> run t' in
    run t 

  let find trace insn_name = 
    let t = create trace in
    let rec run t =
      let p, events = next_compare_point t.events in
      let t' = {t with events} in
      match p with
      | None -> None
      | Some p -> 
        let t' = perform_compare t' ~verbose:true p in
        match Report.find t'.report insn_name with
        | [] -> run t'
        | r::_ -> r in
    run t

end

let create arch = 
  let module T = (val (Veri_types.t_of_arch arch)) in
  (module Verification(T) : V)
