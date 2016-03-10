open Core_kernel.Std
open Bap.Std
open Bap_traces.Std
open Trace

module Dis = Disasm_expert.Basic

module type V = sig
  type t
  val create : Trace.t -> t
  val execute: Trace.t -> t
  val step : t -> t option
  val right: t -> int
  val wrong: t -> int
  val until_mismatch: t -> t option
  val context: t -> Bili.context * Bili.context
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

  (** type context consists of two Bili.context. First one
      represents evaluation of events like register_write,
      memory_store and so on, i.e. events that explicitly
      describes some side effects. Second context represents
      evaluation of code_exec events, after lifting them with
      appropriative lifter for current target *)
  type context = {
    base : Bili.context;
    exec : Bili.context; 
  } 

  (** type compare_unit describes a piece of program trace
      as raw code and list of side effects that this code
      (in best case) perofrms. *)
  type compare_unit = {
    code : Chunk.t;
    side : event list;
  }

  (** TODO: think about returning this type  *)
  type diverged = {
    var   : var;
    right : Bil.result;
    wrong : Bil.result option;
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
    context : context;
  }

  let arch = T.arch
  let endian = Arch.endian arch

  let lift_insn (mem,insn) = match T.lift mem insn with
    | Ok b -> Some b
    | Error _ -> None 

  let create trace =
    let events = match Seq.next (Trace.events trace) with
      | Some (ev, evs) -> Started (ev, evs)
      | None -> Finished in
    let ctxt = new Bili.context in
    let context = {base = ctxt; exec = ctxt;} in
    let result  = {right = 0; wrong = 0;} in
    {events; context; result;}

  let right {result} = result.right
  let wrong {result} = result.wrong
  let context {context} = context.base, context.exec

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
      if List.length bil <> 0 then
        Printf.printf "eval chunk %d\n" (List.length bil) ;
      Printf.printf "%s\n" (string_of_bil bil);
      Stmt.eval bil ctxt
    | Error er -> 
      Printf.printf "error during chunk exec : %s\n"
        (Info.to_string_hum (Error.to_info er));
      flush stdout;
      ctxt

  let eval_event ctxt event =
    let open Trace in
    Value.Match.(begin
        select @@
        case Event.register_write 
          (fun m -> 
             let cell,data = Move.(cell m, data m) in
             let b = Bil.(cell := int data) in
             Stmt.eval (b::[]) ctxt) @@
        case Event.memory_store
          (fun m ->              
             Printf.printf "memory store: %s\n" (string_of_event event);
             ctxt) @@
        default (fun () -> ctxt)
      end) event

  (** [sync t] - replaces bindings from exec context with same 
      variables from base context. *)
  let sync ({context} as t) =     
    let exec = Seq.fold ~init:context.exec
        ~f:(fun ctxt (var,res) -> ctxt#update var res) context.base#bindings in    
    let context = { context with exec } in 
    { t with context }

  let compare_binds base executed =
    Seq.fold base ~init:(0,0) ~f:(fun (cor,incor) (b, r) ->
        match Seq.find executed ~f:(fun (b',_) -> Var.name b' = Var.name b) with
        | None -> cor, incor + 1
        | Some (_, r') -> 
          if Bil.Result.value r = Bil.Result.value r' then cor + 1, incor
          else cor, incor + 1)    

  let update_result t = 
    let binds  = t.context.base#bindings in
    let binds' = t.context.exec#bindings in
    let cor, incor = compare_binds binds binds' in
    let r = t.result in
    let result = {right = r.right + cor; wrong = r.wrong + incor} in
    {t with result}

  let compare_stage t = update_result t |> sync

  (** TODO: remove it later  *)
  let string_of_event ev = 
    let open Event in
    Value.Match.(begin
        select @@
        case register_write (fun m -> "register_write") @@
        case register_read (fun _ -> "register_read") @@
        case memory_load (fun _ -> "memory_load") @@
        case memory_store (fun _ -> "memory_store") @@
        case timestamp (fun _ -> "timestamp") @@
        case pc_update (fun _ -> "pc_update") @@
        case context_switch (fun _ -> "context_switch") @@
        case exn (fun _ -> "exn") @@
        case call (fun _ -> "call") @@
        case return (fun _ -> "return") @@
        case modload (fun _ -> "modload") @@
        case code_exec (fun c -> "code_exec") @@
        default (fun () -> "unknown event")
      end) ev 

  let perform_compare t point =
    Printf.printf "events count %d\n" (List.length point.side + 1);
    let base = List.fold_left ~init:t.context.base ~f:eval_event point.side in
    let exec = eval_code t.context.exec point.code in
    let context = {base; exec} in
    let t' = {t with context } in
    update_result t'
  (* compare_stage t' *)

  let next_compare_unit reader = 
    let is_code = Value.is Event.code_exec in
    let get_code_exn = Value.get_exn Event.code_exec in
    let make_unit code side = match code with
      | Some code -> Some {code = get_code_exn code; side;} 
      | None -> None in
    let rec run code side events = match Seq.next events with 
      | None -> make_unit code (List.rev side), Finished
      | Some (event, events') -> 
        if is_code event then
          match code with 
          | None -> run (Some event) side events'
          | Some code as code' -> 
            let comp_unit = make_unit code' (List.rev side) in
            comp_unit, Started (event, events')
        else run code (event::side) events' in
    match reader with 
    | Finished -> None, reader
    | Started (ev, evs) -> 
      if is_code ev then run (Some ev) [] evs
      else run None [ev] evs

  let step t = 
    let p,events = next_compare_unit t.events in
    let t' = {t with events} in
    match p with
    | Some p -> Some (perform_compare t' p)
    | None -> None

  (** TODO: same as previous,  *)
  let until_mismatch t = 
    let r = t.result in
    let rec run t =
      let p, events = next_compare_unit t.events in
      let t' = {t with events} in
      match p with
      | Some p -> 
        let t' = perform_compare t' p in
        if t'.result = r then run t'
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
