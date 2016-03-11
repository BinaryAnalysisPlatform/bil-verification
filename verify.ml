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

module type A = sig
  val arch: arch
end

module type T = sig
  include Target
  include A
end

module Make(A:A)(Target:Target) : T = struct
  include Target
  include A
end

module Verification(T : T) = struct

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
    context : Veri_context.t;
  }

  let endian = Arch.endian T.arch

  let lift_insn (mem,insn) = match T.lift mem insn with
    | Ok b -> Some b
    | Error _ -> None 

  let create trace =
    let events = match Seq.next (Trace.events trace) with
      | Some (ev, evs) -> Started (ev, evs)
      | None -> Finished in
    let context = Veri_context.of_memory T.CPU.mem in
    let result  = Stat.create () in
    {events; context; result;}

  let stat t = t.result

  (** TODO: remove it later  *)
  let print_name insn = 
    let insn' = Insn.of_basic insn in
    Printf.printf "insn: %s\n" (Insn.name insn')

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

  (** TODO: remove it later  *)
  let string_of_bil b = 
    Format.fprintf Format.str_formatter "%a" Bil.pp b;
    Format.flush_str_formatter ()

  (** TODO: remove it later  *)
  let string_of_event e = 
    Format.fprintf Format.str_formatter "%a" Value.pp e;
    Format.flush_str_formatter ()
  
  let eval_event context event =
    let open Trace in
    let content m = Move.(cell m, data m) in
    Value.Match.(begin
        select @@
        case Event.register_write 
          (fun m -> 
             let cell,data = content m in
             Veri_context.update_var context cell data) @@
        case Event.memory_store
          (fun m -> 
             let addr,data = content m in
             Veri_context.update_mem context addr data) @@
        default (fun () -> ())
      end) event

  (** TODO : remove it  *)
  let string_of_bindings binds = 
    let pp (v,r) =
      let value = Bil.Result.value r in
      let ppv = Bil.Result.Value.pp in
      Format.fprintf Format.str_formatter "%a = %a\n" Var.pp v ppv value in
    Seq.iter binds ~f:pp;
    Format.flush_str_formatter ()

  (** TODO : remove it  *)
  let print_bindings binds = 
    Printf.printf "binds: %s\n" (string_of_bindings binds)

  (** [name_of_insns insns] returns a name of last
      instruction in [insns] list *)
  let name_of_insns insns = match (List.rev insns) with 
    | [] -> None
    | (_,insn)::_ -> Some Insn.(name (of_basic insn))

  let update_histo t = function 
    | [] -> ()
    | insns -> 
      let (_, insn) = List.hd_exn (List.rev insns) in
      let name = Insn.(name (of_basic insn)) in
      Stat.succ_histo t.result name
  
  let perform_compare t point =
    match insns_of_chunk point.code with
    | Error _ -> 
      let result = Stat.succ_undef t.result in
      [], {t with result}
    | Ok insns -> 
      let bil = List.filter_map ~f:lift_insn insns |> List.concat in     
      let exec_ctxt = Veri_context.to_bili_context t.context in
      let exec_ctxt' = Stmt.eval bil exec_ctxt in
      let () = List.iter ~f:(eval_event t.context) point.side in
      match Veri_context.diff t.context exec_ctxt' with
      | [] -> [], t
      | diff -> 
        update_histo t insns;
        diff,t

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
      let _, t' = perform_compare t' p in
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
  let module Target = (val target_of_arch arch) in
  let module A = struct let arch = arch end in
  let module T = Make(A)(Target) in  
  (module Verification(T) : V)
