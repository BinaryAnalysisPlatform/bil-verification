open Core_kernel.Std
open Bap.Std
open Bap_traces.Std
open Monad.State
open Trace

type 'a r = 'a Bil.Result.r
type 'a u = 'a Bil.Result.u

type point = {
  code : Chunk.t;
  side : event list
}

let add_side p ev = {p with side = ev :: p.side }
let make_point code = {code; side = [];}
let do_nothing () = return ()
let value = Bil.Result.value
let is_equal s s' = Seq.to_list s = Seq.to_list s'
let has_side_effects c c' = not (phys_equal c c')

let is_equal_ctxts c c' = 
  Seq.to_list c#bindings = Seq.to_list c'#bindings &&
  c#pc = c'#pc

class context report = object(self:'s)
  inherit Veri_traci.context
  val report : Veri_report.t = report
  method report = report
  method update_report s = {<report = Veri_report.succ report s >}
end

class ['a] t arch = object(self)
  constraint 'a = #context
  inherit ['a] Veri_traci.t arch as super

  method private step point = 
    get () >>= fun src -> 
    List.fold ~init:(return ())
      ~f:(fun sm ev -> sm >>= fun () -> self#eval_event ev)
      point.side >>= fun () -> 
    get () >>= fun trs -> put src >>= fun () ->
    self#eval_exec point.code >>= fun () -> get () >>= fun exe ->
    let s = match exe#current with
      | None -> `Undsm
      | Some insn ->
        if has_side_effects src exe then
          if is_equal_ctxts trs exe then `Right
          else `Wrong insn
        else `Unlif insn in
    put (trs#update_report s)

  method! eval_trace events =
    let map v ~d ~f = Option.value_map v ~default:d ~f in
    let rec loop point evs =
      match Seq.next evs with
      | None -> map point ~d:(do_nothing ()) ~f:(fun p -> self#step p)
      | Some (ev, evs) ->
        match Value.get Event.code_exec ev with
        | Some code ->
          let next = Some (make_point code) in
          map point ~d:(do_nothing ())
            ~f:(fun p -> self#step p) >>= fun () -> loop next evs
        | None ->
          let next = map point ~d:None ~f:(fun p -> Some (add_side p ev)) in
          loop next evs in
    loop None events

end
