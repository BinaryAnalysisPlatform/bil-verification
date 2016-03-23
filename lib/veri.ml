open Core_kernel.Std
open Bap.Std
open Bap_traces.Std
module Bap_value = Value
open Bil.Result
open Monad.State
open Trace

let do_nothing () = return ()

type point = {
  code : Chunk.t;
  side : event list
}
let is_first_read ctxt ev =
  match Bap_value.get Event.register_read ev with
  | None -> false
  | Some mv -> Option.is_none (ctxt#lookup (Move.cell mv))

let is_first_load ctxt ev = 
  match Bap_value.get Event.memory_load ev with
  | None -> false
  | Some mv ->
    match Seq.find ~f:(fun (v,_) -> v = ctxt#mem) ctxt#bindings with
    | None -> true
    | Some (_,s) ->
      match value s with
      | Bil.Mem storage -> Option.is_none (storage#load (Move.cell mv))
      | _ -> true 

let is_init_event ctxt ev =
  is_first_read ctxt ev || is_first_load ctxt ev

class context report arch = object(self:'s)
  inherit Veri_traci.context arch
  val report : Veri_report.t = report
  val events : event list = []
  val selves = None
  val current = `Fst
  val descr : string option = None

  method split = 
    let self = {< events = []; descr = None >} in
    {< selves = Some (self, self); current = `Fst >}
       
  method register_event ev = match selves with
    | None -> {< events = ev :: events >}
    | Some (main, shadow) ->
      match current with
      | `Fst ->
        let main = main#register_event ev in
        {<selves = Some (main, shadow) >}
      | `Snd ->
        let shadow' = shadow#register_event ev in
        {<selves = Some (main,shadow') >}

  method events = events
  method main = fst (Option.value_exn selves)
  method shadow = snd (Option.value_exn selves)
  method replay = {<current = `Snd; >}

  method merge: 's = 
    let is_exists_event ev evs = 
      List.exists ~f:(fun ev' -> ev = ev') evs in   
    let events = self#main#events in
    let events' = self#shadow#events in
    let init = List.filter ~f:(is_init_event self#main) events' in
    let events' = init @ events' in
    let wrong = List.fold_left ~init:0 
        ~f:(fun wrong ev -> 
            if is_exists_event ev events' then wrong
            else wrong + 1) events in    
    let report = 
      if wrong = 0 then Veri_report.succ report `Right
      else match descr with
        | Some descr -> Veri_report.succ report (`Wrong descr)
        | None -> report in    
    {<report = report; selves = None; >}

  method report = report
  method set_description s = {<descr = Some s >}
end

let create_move_event tag cell' data' =  
  Bap_value.create tag Move.({cell = cell'; data = data';})   

let create_mem_store = create_move_event Event.memory_store
let create_mem_load  = create_move_event Event.memory_load
let create_reg_read  = create_move_event Event.register_read
let create_reg_write = create_move_event Event.register_write

let is_imm_var var = 
  let open Type in
  match Var.typ var with
  | Imm _ -> true
  | _ -> false

let is_mem_var var = 
  let open Type in
  match Var.typ var with
  | Mem _ -> true
  | _ -> false

let imm_result v = 
  let open Bil in
  match v with
  | Imm x -> Some x
  | _ -> None

let mem_result v = 
  let open Bil in
  match v with
  | Mem x -> Some x
  | _ -> None

type 'a e = (event option, 'a) Monad.State.t

class ['a] t is_interesting = object(self)
  constraint 'a = #context
  inherit ['a] Veri_traci.t as super

  method private update_event ev =
    if is_interesting ev then
      get () >>= fun s -> put @@ s#register_event ev
    else do_nothing ()
        
  method! lookup var : 'a r =
    super#lookup var >>= fun r -> 
    if is_imm_var var then 
      match imm_result (value r) with 
      | Some data -> 
        self#update_event (create_reg_read var data) >>= fun () ->
        return r
      | None -> return r
    else return r

  method! update var result : 'a u = 
    super#update var result >>= fun () -> 
    if is_imm_var var then 
      match imm_result (value result) with 
      | Some data -> self#update_event (create_reg_write var data)
      | None -> do_nothing ()
    else do_nothing ()

  method private eval_mem_event tag addr data : 'a e = 
    match value addr, value data with
    | Bil.Imm addr, Bil.Imm data ->
      let ev = create_move_event tag addr data in
      return (Some ev)
    | _ -> return None

  method! eval_store: mem:exp -> addr:exp -> exp -> endian -> size -> 'a r =
    fun ~mem ~addr data endian size ->
      super#eval_store ~mem ~addr data endian size >>= fun r ->
      self#eval_exp addr >>= fun addr ->
      self#eval_exp data >>= fun data ->
      self#eval_mem_event Event.memory_store addr data >>=
      fun ev -> match ev with
      | None -> return r
      | Some ev -> self#update_event ev >>= fun () -> return r

  method! eval_load: mem:exp -> addr:exp -> endian -> size -> 'a r =
    fun ~mem ~addr endian size ->
      super#eval_load ~mem ~addr endian size >>= fun r ->
      self#eval_exp addr >>= fun addr ->
      self#eval_mem_event Event.memory_load addr r >>=
      fun ev -> match ev with
      | None -> return r
      | Some ev -> self#update_event ev >>= fun () -> return r

  method private eval_chunk arch chunk = 
    match Veri_helpers.bil_of_chunk arch chunk with
    | Error _ -> do_nothing ()
    | Ok (name, bil) ->
      self#eval bil >>= fun () -> get () >>= fun ctxt ->      
      put (ctxt#set_description name)

  method private step point = 
    get () >>= fun ctxt -> put ctxt#split >>= fun () ->
    List.fold ~init:(return ()) ~f:(fun sm ev -> 
        sm >>= fun () -> self#eval_event ev) point.side >>= fun () ->
    get () >>= fun ctxt -> put (ctxt#replay) >>= fun () ->
    self#eval_chunk ctxt#arch point.code >>= fun () ->
    get () >>= fun ctxt -> put ctxt#merge 

  method eval_trace trace =
    let is_code  = Bap_value.is Event.code_exec in
    let code_exn = Bap_value.get_exn Event.code_exec in      
    let rec loop point evs =
      match Seq.next evs with
      | None -> do_nothing ()
      | Some (ev, evs) ->
        if is_code ev then
          let next_point = Some ({code = code_exn ev; side = [];}) in
          match point with 
          | None -> loop next_point evs
          | Some point ->           
            self#step point >>= fun () -> loop next_point evs
        else
          match point with
          | None -> loop None evs
          | Some point -> 
            let next_point = {point with side = ev :: point.side} in
            loop (Some next_point) evs in
    loop None (Trace.events trace)

end
