open Core_kernel.Std
open Bap.Std
open Bap_traces.Std
open Trace

module Dis = Disasm_expert.Basic

module type V = sig
  type t
  val create : Trace.t -> t
  val execute: Trace.t -> t
  val until_mismatch: t -> Diff.t list * t option
  val stat: t -> Stat.t
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
    result  : Stat.t;
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
    let result  = Stat.create () in
    {events; context; result;}

  let stat t = t.result

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

  (** TODO: remove it *)
  let update_reg ctxt reg_event = 
    let var, data = Move.cell reg_event, Move.data reg_event in
    let var' = Var.create (Var.name var) (Type.Imm 32) in
    Context.update_var ctxt var' data

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

  let update_histo t = function 
    | [] -> t
    | insns -> 
      let (_, insn) = List.hd_exn (List.rev insns) in
      let name = Insn.(name (of_basic insn)) in
      let () = Printf.printf "insn is %s\n" name in
      {t with result = Stat.succ_histo t.result name}

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

  let perform_compare t point =
    match insns_of_chunk point.code with
    | Error _ -> [], {t with result = Stat.succ_undef t.result}
    | Ok insns -> 
      let bil = List.filter_map ~f:lift_insn insns |> List.concat in
      let () = init_stage t point in
      let exec_ctxt = Context.to_bili_context t.context in
      let exec_ctxt' = Stmt.eval bil exec_ctxt in
      let () = List.iter ~f:(eval_event t.context) point.side in
      match Context.diff t.context exec_ctxt' with
      | [] -> [], t
      | diff -> diff, update_histo t insns

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
    | Some p -> 
      let diff, t' = perform_compare t' p in
      Some t'
    | None -> None

  let until_mismatch t = 
    let rec run t =
      let p, events = next_compare_point t.events in
      let t' = {t with events} in
      match p with
      | None -> [], None 
      | Some p -> 
        let diff, t' = perform_compare t' p in
        match diff with 
        | [] -> run t' 
        | diff -> diff, Some t' in
    run t

  let execute trace = 
    let t = create trace in
    let rec run t = match step t with 
      | None -> t
      | Some t' -> run t' in
    run t 

end

let create arch = 
  let module T = (val (Veri_types.t_of_arch arch)) in
  (module Verification(T) : V)
