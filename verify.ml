open Core_kernel.Std
open Bap.Std
open Bap_traces.Std
open Trace
open Veri_types

module Dis = Disasm_expert.Basic
module Report = Veri_report

module type V = sig
  type t
  val create : Trace.t -> t
  val execute: Trace.t -> Report.t
  val step: t -> (Record.t * t) option
  val count: Trace.t -> string -> int option
  val find: Trace.t -> string -> Record.t option
  val find_all: Trace.t -> string -> Record.t list
  val fold: t -> init:'a -> f:(Record.t -> 'a -> 'a) -> 'a
  val iter: t -> f:(Record.t -> unit) -> unit
  val report: t -> Report.t
end

(** type compare_point describes a piece of program trace
    as raw code and list of side effects that this code
    should perform. *)
type compare_point = {
  code : Chunk.t;
  side : event list;
}

(** type executed describes a point after execution *)
type executed = {
  insns: (mem * (Dis.asm, Dis.kinds) Dis.insn) list;
  ctxt : Bili.context;
}

type events_reader =
  | Started of event * (event Seq.t)
  | Finished

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

let move_cell ev = Move.cell ev
let move_data ev = Move.data ev

(** [name_of_insns insns] - returns a name of last instruction in [insns]  *)
let name_of_insns insns = 
  Option.(List.hd (List.rev insns) >>|
          fun (_,insn) -> Insn.(name (of_basic insn)))


module Verification(T : Veri_types.T) = struct

  module Context = Veri_context.Make(T) 

  type t = {
    events  : events_reader;
    report  : Report.t ;
    context : Context.t;
  }

  let endian = Arch.endian T.arch

  let create trace =
    let events = match Seq.next (Trace.events trace) with
      | Some (ev, evs) -> Started (ev, evs)
      | None -> Finished in
    let context = Context.create () in
    let report = Report.create () in
    {events; context; report;}

  let lift_insn (mem,insn) = match T.lift mem insn with
    | Ok b -> Some b
    | Error _ -> None 

  let report t = t.report

  let succ t what = {t with report = Report.succ t.report what} 

  let insns_of_chunk chunk =
    let open Or_error in
    Dis.with_disasm ~backend:"llvm" (Arch.to_string T.arch)
      ~f:(fun dis ->
          let dis = Dis.store_kinds dis |> Dis.store_asm in
          let mems = Bigstring.of_string (Chunk.data chunk) in
          Memory.create endian (Chunk.addr chunk) mems >>=
          fun mem -> insns_of_mem dis mem)

  let update_reg ctxt reg_event = 
    Context.update_var ctxt (move_cell reg_event) (move_data reg_event)

  let update_mem ctxt mem_event =
    Context.update_mem ctxt (move_cell mem_event) (move_data mem_event)

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

  let is_init_event context ev = 
    Value.Match.(
      select @@
      case Event.register_read 
        (fun m -> not (Context.exists_var context (move_cell m))) @@
      case Event.memory_load 
        (fun m -> not (Context.exists_mem context (move_cell m))) @@
      default (fun () -> false)) ev

  let init_stage context point = 
    let evs = List.filter ~f:(is_init_event context) point.side in
    List.iter evs ~f:(eval_event context)

  let next_compare_point t = 
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
    match t.events with 
    | Finished -> None, t
    | Started (ev, evs) -> 
      let p, events = 
        if is_code ev then run (Some ev) [] evs
        else run None [ev] evs in
      p, {t with events}

  let eval_base context point = List.iter ~f:(eval_event context) point.side 

  let eval_insns context insns =
    let bil = List.filter_map ~f:lift_insn insns |> List.concat in
    Context.to_bili_context context |>
    Stmt.eval bil

  let eval context point insns =
    let () = init_stage context point in
    let ctxt = eval_insns context insns in
    let () = eval_base context point in
    ctxt 

  let prepare_compare context point =
    Or_error.(insns_of_chunk point.code >>| 
              fun insns -> {insns; ctxt = eval context point insns})
  
  let brief_compare t point = 
    match prepare_compare t.context point with
    | Error _ -> succ t `Undef
    | Ok {insns; ctxt} -> 
      if Context.is_different t.context ctxt then
        match name_of_insns insns with
        | Some name -> succ t (`Wrong name)
        | None -> t
      else succ t `Right

  let single_compare t point insn_name = 
    match insns_of_chunk point.code with
    | Error _ -> t, None
    | Ok insns ->
      match name_of_insns insns with
      | None -> t, None
      | Some name when name <> insn_name -> 
        eval_base t.context point;
        t, None
      | Some _ ->
        let ctxt = eval t.context point insns in
        match Context.diff t.context ctxt with
        | [] -> succ t `Right, None
        | diff -> 
          match name_of_insns insns with
          | None -> t, None
          | Some name ->
            let record = Record.create name point.code ctxt diff in
            succ t (`Wrong name), Some record

  let execute trace = 
    let rec run t = 
      let p, t' = next_compare_point t in
      match p with 
      | None -> t'.report
      | Some p -> run (brief_compare t' p) in
    run (create trace)

  let count trace insn_name = 
    let rec run t = match next_compare_point t with 
      | None, t' -> Some (Report.wrong t'.report)
      | Some point, t' -> 
        let t', _ = single_compare t' point insn_name in
        run t' in
    run (create trace)

  let perform_compare t point = 
    match prepare_compare t.context point with
    | Error _ -> succ t `Undef, None
    | Ok {insns; ctxt} -> 
      match Context.diff t.context ctxt with
      | [] -> succ t `Right, None
      | diff -> 
        match name_of_insns insns with
        | None -> t, None
        | Some name -> 
          let record = Record.create name point.code ctxt diff in
          succ t (`Wrong name), Some record

  let step t = 
    let rec run t =
      let p, t' = next_compare_point t in
      match p with
      | None -> None 
      | Some p -> match perform_compare t' p with
        | t', None -> run t'
        | t', Some record -> Some (record, t') in
    run t

  let fold t ~init ~f =
    let rec run t acc =
      match step t with
      | None -> acc
      | Some (r, t') -> 
        let acc' = f r acc in
        run t' acc' in
    run t init

  let iter t ~f = 
    let f' r () = f r in
    fold t ~init:() ~f:f'

  let find trace insn_name = 
    let rec run t =
      let p, t' = next_compare_point t in
      match p with
      | None -> None
      | Some p -> match single_compare t p insn_name with
        | t', None -> run t'
        | t', Some r -> Some r in
    run (create trace)

  let find_all trace insn_name = 
    let rec run t recs =
      let p, t' = next_compare_point t in
      match p with
      | None -> recs
      | Some p -> 
        let t', r = single_compare t' p insn_name in
        match r with
        | None -> run t' recs
        | Some r -> run t' (r::recs) in
    run (create trace) []

end

let create arch = 
  let module T = (val (Veri_types.t_of_arch arch)) in
  (module Verification(T) : V)
