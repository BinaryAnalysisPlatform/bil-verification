open Core_kernel.Std
open Bap.Std
open Bap_traces.Std
open Trace

module Dis = Disasm_expert.Basic

module Diff = struct
  
  type 'a diff = {
    src : 'a;
    ok  : word;
    er  : word option
  }

  type t = Imm of var diff | Mem of addr diff

  let pp fmt t =
    let ppo fmt = function
      | Some v -> Format.fprintf fmt "%a" Word.pp v
      | None -> Format.fprintf fmt "none" in
    let pp pp_src src ok er = 
      Format.fprintf fmt "%a: error %a (%a)\n"
        pp_src src ppo er Word.pp ok in 
    match t with 
    | Imm t -> pp Var.pp t.src t.ok t.er
    | Mem t -> pp Addr.pp t.src t.ok t.er

end

module type V = sig
  type t
  val create : Trace.t -> t
  val execute: Trace.t -> t
  val step : t -> t option
  val right: t -> int
  val wrong: t -> int
  val until_mismatch: t -> t option
  val diverged: t -> Diff.t list
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

module Context(Target:Target)= struct

  type vars = word Var.Table.t
  type mems = word Addr.Table.t

  type t = {
    vars : vars;
    mems : mems;
  }

  let create () = { 
    vars = Var.Table.create (); 
    mems = Addr.Table.create ();
  }
 
  let to_bili_context t = 
    let ctxt = Var.Table.fold t.vars ~init:(new Bili.context)
        ~f:(fun ~key ~data ctxt ->
            let ctxt,r = ctxt#create_word data in
            ctxt#update key r) in
    let s = Addr.Table.fold t.mems ~init:(new Bil.Storage.sparse)
        ~f:(fun ~key ~data s -> s#save key data) in
    let ctxt, r = ctxt#create_storage s in
    ctxt#update Target.CPU.mem r

  let update_var t var word = Var.Table.set t.vars ~key:var ~data:word
  let update_mem t addr word = Addr.Table.set t.mems ~key:addr ~data:word

  let storage_of_result v = 
    let open Bil in
    match Result.value v with 
    | Mem s -> Some s
    | _ -> None
      
  let word_of_result v = 
    let open Bil in
    match Result.value v with
    | Imm w -> Some w
    | _ -> None

  let vars_diff t ctxt = 
    let open Diff in
    let add diff src ok er = (Imm {src; ok; er})::diff in
    Var.Table.fold t.vars ~init:[] ~f:(fun ~key ~data diff ->
        match ctxt#lookup key with
        | None -> add diff key data None
        | Some r -> match word_of_result r with 
          | Some w -> 
            if data = w then diff 
            else add diff key data (Some w)
          | _ -> add diff key data None) 
      
  let mems_diff t ctxt = 
    let open Diff in
    let add diff src ok er = (Mem {src; ok; er})::diff in
    let all () = 
      Addr.Table.fold t.mems ~init:[]
        ~f:(fun ~key ~data diff -> add diff key data None) in
    match ctxt#lookup Target.CPU.mem with
    | None -> all ()
    | Some r -> match storage_of_result r with
      | None -> all ()
      | Some s ->
        Addr.Table.fold t.mems ~init:[] ~f:(fun ~key ~data diff ->
            match s#load key with
            | None -> add diff key data None
            | Some w -> 
              if w = data then diff
              else add diff key data (Some w))

  let diff t ctxt = vars_diff t ctxt @ mems_diff t ctxt

end

module Verification(T : T) = struct

  module Context = Context(T)

  (** type compare_point describes a piece of program trace
      as raw code and list of side effects that this code
      should perofrm. *)
  type compare_point = {
    code : Chunk.t;
    side : event list;
  }

  (** TODO: refine it. It's just a stub now and it is not informable *)
  type result = {
    right : int;
    wrong : int;
  } 

  type events_reader =
    | Started of event * (event Seq.t)
    | Finished

  type t = {
    events  : events_reader;
    result  : result;
    context : Context.t;
    diff    : Diff.t list;
  }

  let arch = T.arch
  let endian = Arch.endian arch
  let right {result} = result.right
  let wrong {result} = result.wrong
  let diverged t = t.diff

  let lift_insn (mem,insn) = match T.lift mem insn with
    | Ok b -> Some b
    | Error _ -> None 

  let create trace =
    let events = match Seq.next (Trace.events trace) with
      | Some (ev, evs) -> Started (ev, evs)
      | None -> Finished in
    let context = Context.create () in
    let result  = {right = 0; wrong = 0;} in
    {events; context; result; diff = []; }

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

  let bil_of_chunk chunk =
    let open Or_error in
    Dis.with_disasm ~backend:"llvm" (Arch.to_string arch)
      ~f:(fun dis ->
          let dis = Dis.store_kinds dis |> Dis.store_asm in
          let mems = Bigstring.of_string (Chunk.data chunk) in
          Memory.create endian (Chunk.addr chunk) mems >>=
          fun mem -> insns_of_mem dis mem >>|
          List.filter_map ~f:lift_insn >>|
          List.concat)

  (** TODO: remove it later  *)
  let string_of_bil b = 
    Format.fprintf Format.str_formatter "%a" Bil.pp b;
    Format.flush_str_formatter ()

  (** TODO: remove it later  *)
  let string_of_event e = 
    Format.fprintf Format.str_formatter "%a" Value.pp e;
    Format.flush_str_formatter ()

  let eval_code ctxt chunk =
    match bil_of_chunk chunk with
    | Ok bil ->
      Printf.printf "code: ";
      Printf.printf "%s\n" (string_of_bil bil);
      Stmt.eval bil ctxt
    | Error er -> 
      Printf.printf "error during chunk exec : %s\n"
        (Info.to_string_hum (Error.to_info er));
      flush stdout;
      ctxt

  let eval_event context event =
    let open Trace in
    let content m = Move.(cell m, data m) in
    Value.Match.(begin
        select @@
        case Event.register_write 
          (fun m -> 
             Printf.printf "register_write: %s\n" (string_of_event event);
             let cell,data = content m in
             Context.update_var context cell data) @@
        case Event.memory_store
          (fun m -> 
             Printf.printf "memory store: %s\n" (string_of_event event);
             let addr,data = content m in
             Context.update_mem context addr data) @@
        default (fun () -> ())
      end) event
  
  let perform_compare t point =
    let exec_ctxt = Context.to_bili_context t.context in
    let exec_ctxt' = eval_code exec_ctxt point.code in
    let () = List.iter ~f:(eval_event t.context) point.side in
    let diff = Context.diff t.context exec_ctxt' in
    {t with diff }

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

  let step t = 
    let p,events = next_compare_point t.events in
    let t' = {t with events} in
    match p with
    | Some p -> Some (perform_compare t' p)
    | None -> None

  let until_mismatch t = 
    let r = t.diff in
    let rec run t =
      let p, events = next_compare_point t.events in
      let t' = {t with events} in
      match p with
      | Some p -> 
        let t' = perform_compare t' p in
        if t'.diff = r then run t'
        else Some t'
      | _ -> None in
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
