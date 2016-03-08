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
  val correct: t -> int
  val incorrect: t -> int
  val until_compare: t -> t option
  val until_mismatch: t -> (Bili.context * Bili.context) option
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

  type context = {
    base : Bili.context;
    exec : Bili.context; 
  } 

  (** TODO: refine it. It's just a stub now, because is not-informable *)
  type result = {
    correct   : int;
    incorrect : int;
  } 
  
  type t = {
    events  : Trace.event Seq.t;
    result  : result;
    context : context;
  }

  let arch = T.arch
  let endian = Arch.endian arch

  let lift_insn (mem,insn) = match T.lift mem insn with
    | Ok b -> Some b
    | Error _ -> None 

  let create trace =
    let events = Trace.events trace in
    let ctxt = new Bili.context in
    let context = {base = ctxt; exec = ctxt;} in
    let result = { correct = 0; incorrect = 0; } in
    { events; context; result; }

  let correct {result} = result.correct
  let incorrect {result} = result.incorrect

  let bil_of_chunk chunk =
    let open Or_error in
    let rec disasm dis insns mem =
      Dis.insn_of_mem dis mem >>= (fun (imem, insn, left) ->
          let insns' = match insn with 
            | Some insn -> (imem, insn) :: insns 
            | None -> insns in
          match left with
          | `left mem -> disasm dis insns' mem 
          | `finished -> Ok (List.rev insns')) in 
    Dis.with_disasm ~backend:"llvm" (Arch.to_string arch)
      ~f:(fun dis -> 
        let dis = Dis.store_kinds dis |> Dis.store_asm in
        let mems = Bigstring.of_string (Chunk.data chunk) in
        Memory.create endian (Chunk.addr chunk) mems >>=
        fun mem -> disasm dis [] mem >>| List.filter_map ~f:lift_insn)

  module type R = module type of Result

  let print_bindings binds = 
    Seq.iter binds ~f:(fun (v,r) -> 
      Var.pp Format.std_formatter v;
      print_newline();
      flush stdout)

  let eval_chunk ctxt chunk =
    match bil_of_chunk chunk with
    | Ok bil ->
      List.fold_left ~init:ctxt
        ~f:(fun ctxt bil -> Stmt.eval bil ctxt) bil
    | Error er -> 
      Printf.printf "error during chunk exec : %s\n" 
      (Info.to_string_hum (Error.to_info er));
      flush stdout;
      ctxt

  (** [sync t] - replaces bindings from exec context for same 
      variables from base context. *)
  let sync ({context} as t) = 
    let exec = Seq.fold ~init:context.exec 
      ~f:(fun ctxt (var,_) -> 
        match context.base#lookup var with 
        | Some res -> ctxt#update var res
        | None -> ctxt) context.exec#bindings in    
    let context = { context with exec} in 
    { t with context}

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
    let result = {correct = r.correct + cor; incorrect = r.incorrect + incor} in
    {t with result}

  let compare_stage t = update_result t |> sync

  let eval_event t event =
    let open Trace in
    let base = t.context.base in
    let exec = t.context.base in
    Value.Match.(begin
        select @@
        case Event.register_write 
          (fun m -> 
             let cell,data = Move.(cell m, data m) in
             let b = Bil.(cell := int data) in
             let base = Stmt.eval (b::[]) base in
             let context = {t.context with base} in
             {t with context}) @@
        case Event.code_exec (fun c -> 
            let exec = eval_chunk exec c in
            let () = print_bindings exec#bindings in
            let context = {t.context with exec} in
            {t with context}) @@
        default (fun () -> t)
      end) event

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

  let until_compare t = 
    let rec run t events delayed =
      match Seq.next events with
      | None -> None
      | Some (ev, events) ->
        (* let () = Printf.printf "%s\n" (string_of_event ev) in *)
        (* let () = flush stdout in *)
        if Value.is Event.code_exec ev then 
          match delayed with
          | [] -> run (eval_event t ev) events []
          | delayed -> 
            let t' = List.fold_left ~init:t
              ~f:eval_event (List.rev delayed) in
            let t' = eval_event t' ev in
            Some (compare_stage t')
        else run t events (ev :: delayed) in       
    run t t.events []

  let until_mismatch t =
    let rec run t = 
      match until_compare t with
      | None -> None
      | Some t' -> 
        if incorrect t = incorrect t' then run t' 
        else Some (t'.context.base, t'.context.exec) in
    run t
    
  (** TODO: [step] function relies on assumption
      that events stored in trace in the following 
      order: code_exec, move, move .. next code_exec ... 
      So all comparies should be permormed before next
      code_exec. This is the issue that should be 
      discussed and refined.  *)
  let step' t = 
    match Seq.next t.events with 
    | None -> None
    | Some (ev, events) ->
      let t' = 
        if Value.is Event.code_exec ev then 
          compare_stage t 
        else t in     
      let t' = eval_event t' ev in
      let t' = {t' with events } in
      Some t'

  let step t = 
    let r = t.result in
    let rec run t =
      match step' t with 
      | None -> None
      | Some t -> 
        if r = t.result then run t
        else Some t in
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
