open Core_kernel.Std
open Bap.Std
open Bap_traces.Std
open Monad.State 

type 'a u = 'a Bil.Result.u
type event = Trace.event

let stub = fun _ -> return () 

let mem_of_arch arch = 
  let (module T : Target) = target_of_arch arch in
  T.CPU.mem

class context =
  object(self:'s)
  inherit Bili.context
  val current : string option = None
  method set_current s = {< current = s >}
  method current = current
end

let mem_operands mv = 
  let width = Word.bitwidth (Move.data mv) in
  Size.of_int_opt width |> function
  | None -> 
    let er = Printf.sprintf "wrong size for memory operand: %d" width in
    failwith er
  | Some sz ->
    let addr = Bil.int (Move.cell mv) in
    let data = Bil.int (Move.data mv) in
    addr, data, sz

class ['a] t arch = 
  let mem = mem_of_arch arch in 
  let mem' = Bil.var mem in
  object(self)
  constraint 'a = #context
  inherit ['a] Bili.t

  method eval_memory_store mv = 
    let addr, data, size = mem_operands mv in
    get () >>= fun ctxt ->
    let endian = Arch.endian arch in
    self#eval_store ~mem:mem' ~addr data endian size >>= fun r ->
    get () >>= fun ctxt -> self#update mem r

  method eval_register_write mv = 
    self#eval_move (Move.cell mv) (Bil.int (Move.data mv))

  method eval_exec: chunk -> 'a u = fun chunk ->    
    match Veri_helpers.insns_of_chunk arch chunk with
    | Error _ -> update (fun ctxt -> ctxt#set_current None)
    | Ok (name, insns) ->
      update (fun ctxt -> ctxt#set_current (Some name)) >>= fun () ->
      match Veri_helpers.bil_of_insns arch insns with
      | Error _ -> return ()
      | Ok bil -> self#eval bil 

  method eval_register_read: var move -> 'a u = stub
  method eval_memory_load: addr move -> 'a u = stub
  method eval_pc_update: addr -> 'a u = stub
  method eval_context_switch: int -> 'a u = stub
  method eval_syscall: syscall -> 'a u = stub
  method eval_exn: exn -> 'a u = stub
  method eval_call: call -> 'a u = stub
  method eval_return: return -> 'a u = stub
  method eval_modload: modload -> 'a u = stub

  method eval_event ev =
    Value.Match.(
      select @@
      case Event.memory_store self#eval_memory_store @@
      case Event.memory_load self#eval_memory_load @@
      case Event.register_read self#eval_register_read @@
      case Event.register_write self#eval_register_write @@
      case Event.code_exec self#eval_exec @@
      case Event.pc_update self#eval_pc_update @@
      case Event.context_switch self#eval_context_switch @@
      case Event.syscall self#eval_syscall @@
      case Event.exn self#eval_exn @@
      case Event.call self#eval_call @@
      case Event.return self#eval_return @@
      case Event.modload self#eval_modload @@
      default return) ev

  method eval_trace: event Seq.t -> 'a u = fun evs ->
    Seq.fold ~init:(return ()) ~f:(fun sm  ev ->
        sm >>= fun () -> self#eval_event ev) evs

end
