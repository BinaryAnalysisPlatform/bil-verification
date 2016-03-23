open Bap.Std
module Bap_value = Value
open Bap_traces.Std
open Monad.State
open Bil.Result

type event = Trace.event

let mem_of_arch arch = 
  let (module T : Target) = target_of_arch arch in
  T.CPU.mem

(** TODO: what is a behaviour of update_event method?  *)
class context arch = object(self:'s)
  inherit Bili.context
  val mem  : var = mem_of_arch arch
  val arch : arch = arch
  method arch = arch
  method mem = mem
  method update_event: event -> 's = fun ev -> failwith "unimplemented"
end

let mem_operands mv = 
  Size.of_int_opt (Bitvector.bitwidth (Move.data mv)) |> function
  | None -> None
  | Some sz ->
    let addr = Bil.int (Move.cell mv) in
    let data = Bil.int (Move.data mv) in
    Some (addr, data, sz)

class ['a] t = object(self)
  constraint 'a = #context
  inherit ['a] Bili.t

  method private eval_reg: var move -> 'a u = fun mv ->  
    self#eval_move (Move.cell mv) (Bil.int (Move.data mv))

  method private eval_mem_op op mv : 'a r =
    match mem_operands mv with 
    | None -> 
      get () >>= fun ctxt -> 
      let (c,r) = ctxt#create_undefined in
      put c >>= fun () -> return r
    | Some (addr, data, size) ->
      get () >>= fun ctxt ->
      let endian = Arch.endian ctxt#arch in
      match op with
      | `Store ->
        self#eval_store ~mem:(Bil.var ctxt#mem) ~addr data endian size
      | `Load ->
        self#eval_load ~mem:(Bil.var ctxt#mem) ~addr endian size

  method eval_memory_store mv = 
    self#eval_mem_op `Store mv >>= fun r ->
    get () >>= fun ctxt -> self#update ctxt#mem r

  method eval_memory_load: addr move -> 'a u = fun mv -> 
    self#eval_memory_store mv >>= fun () ->
    self#eval_mem_op `Load mv >>= fun r ->
    get () >>= fun ctxt -> self#update ctxt#mem r

  method eval_register_write mv = self#eval_reg mv
  method eval_register_read mv = self#eval_reg mv
  method eval_exec: chunk -> 'a u = fun _ -> return ()
  method eval_pc_update: addr -> 'a u = fun _ -> return ()
  method eval_context_switch: int -> 'a u = fun _ -> return ()
  method eval_syscall: syscall -> 'a u = fun _ -> return ()
  method eval_exn: exn -> 'a u = fun _ -> return ()
  method eval_call: call -> 'a u = fun _ -> return ()
  method eval_return: return -> 'a u = fun _ -> return ()
  method eval_modload: modload -> 'a u = fun _ -> return ()

  method eval_event ev =
    Bap_value.Match.(
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
end
