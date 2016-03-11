open Core_kernel.Std
open Bap.Std
open Bap_traces.Std
open Trace

module Dis = Disasm_expert.Basic

module Diverged = struct
  type t = {
    var : var;
    ok : Bil.result;
    er : Bil.result option;
  } [@@deriving fields]

  let pp fmt t = 
    let to_value = Bil.Result.value in
    let ppv = Bil.Result.Value.pp in
    let ppo fmt = function 
      | Some v -> Format.fprintf fmt "%a" ppv (to_value v) 
      | None -> Format.fprintf fmt "none" in
    Format.fprintf fmt "%a: error %a (%a)\n"
      Var.pp t.var ppo t.er ppv (to_value t.ok)

end

module type V = sig
  type t
  val create : Trace.t -> t
  val execute: Trace.t -> t
  val step : t -> t option
  val right: t -> int
  val wrong: t -> int
  val until_mismatch: t -> t option
  val diverged: t -> Diverged.t list
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

  (** type compare_point describes a piece of program trace
      as raw code and list of side effects that this code
      (in best case) perofrms. *)
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
    context : context;
    diverged: Diverged.t list;
  }

  let arch = T.arch
  let endian = Arch.endian arch
  let right {result} = result.right
  let wrong {result} = result.wrong
  let diverged t = t.diverged
  let context t = t.context.base, t.context.exec

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
    {events; context; result; diverged = []; }

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

  let eval_code' chunk = 
    let bili = new Bili.t in
    match bil_of_chunk chunk with
    | Ok bil -> bili#eval bil
    | Error _ -> failwith "TODO: describe error"

  let var_of_addr addr = 
    let name = Bitvector.string_of_value ~hex:true addr in
    let size = Size.of_int_exn (Bitvector.bitwidth addr) in
    let typ  = Type.(Mem (Arch.addr_size arch, size)) in
    Var.create name typ

  (** TODO: rewrite memory_store  *)
  let eval_event ctxt event =
    let open Trace in
    let content m = Move.(cell m, data m) in
    Value.Match.(begin
        select @@
        case Event.register_write 
          (fun m -> 
             Printf.printf "register_write: %s\n" (string_of_event event);
             let cell,data = content m in
             let b = Bil.(cell := int data) in
             Stmt.eval (b::[]) ctxt) @@
        case Event.memory_store
          (fun m -> 
             Printf.printf "memory store: %s\n" (string_of_event event);
             let addr,data = content m in
             let data = Bil.int data in
             let b = Bil.move (var_of_addr addr) data in
             Stmt.eval (b::[]) ctxt) @@
        default (fun () -> ctxt)
      end) event

  (** [sync t] - replaces bindings from exec context with same 
      variables from base context. *)
  let sync ({context} as t) =     
    let exec = Seq.fold ~init:context.exec
        ~f:(fun ctxt (var,res) -> ctxt#update var res) context.base#bindings in    
    let context = { context with exec } in 
    { t with context }

  let eval_result {base; exec} = 
    let open Diverged in
    let find_in_exec v = 
      Seq.find exec#bindings ~f:(fun (v',_) -> Var.name v = Var.name v') in
    let f (diverged, correct) (var, res) = 
      match find_in_exec var with
      | None -> {var; ok=res; er=None} :: diverged, correct
      | Some (_, res') -> 
        if Bil.Result.value res = Bil.Result.value res' then 
          diverged, correct + 1
        else 
          {var; ok = res; er = Some res'} :: diverged, correct
    in 
    let diverged, correct = Seq.fold ~f ~init:([],0) base#bindings in
    diverged, correct, List.length diverged
    
  let update_result t = 
    let diverged, cor, incor = eval_result t.context in
    let r = t.result in
    let result = {right = r.right + cor; wrong = r.wrong + incor} in
    let t' = {t with result; diverged} in
    sync t'

  let perform_compare t point =
    let base = List.fold_left ~init:t.context.base ~f:eval_event point.side in
    let exec = eval_code t.context.exec point.code in
    let context = {base; exec} in
    let t' = {t with context } in
    update_result t'

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
    let r = t.result in
    let rec run t =
      let p, events = next_compare_point t.events in
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
