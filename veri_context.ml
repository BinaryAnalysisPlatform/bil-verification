open Bap.Std

type vars = word Var.Table.t
type mems = word Addr.Table.t

type t = {
  vars : vars;
  mems : mems;
  mem  : var;
}

let of_memory mem = { 
  vars = Var.Table.create (); 
  mems = Addr.Table.create ();
  mem;
}

let to_bili_context t = 
  let ctxt = Var.Table.fold t.vars ~init:(new Bili.context)
      ~f:(fun ~key ~data ctxt ->
          let ctxt,r = ctxt#create_word data in
          ctxt#update key r) in
  let s = Addr.Table.fold t.mems ~init:(new Bil.Storage.sparse)
      ~f:(fun ~key ~data s -> s#save key data) in
  let ctxt, r = ctxt#create_storage s in
  ctxt#update t.mem r

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
  let add diff src ok er = Diff.of_var src ok er :: diff in
  Var.Table.fold t.vars ~init:[] ~f:(fun ~key ~data diff ->
      match ctxt#lookup key with
      | None -> add diff key data None
      | Some r -> match word_of_result r with 
        | Some w -> 
          if data = w then diff 
          else add diff key data (Some r)
        | None -> add diff key data (Some r)) 

let mems_diff t ctxt = 
  let add diff src ok er = Diff.of_mem src ok er :: diff in
  let all () = 
    Addr.Table.fold t.mems ~init:[]
      ~f:(fun ~key ~data diff -> add diff key data None) in
  match ctxt#lookup t.mem with
  | None -> all ()
  | Some r -> match storage_of_result r with
    | None -> all ()
    | Some s ->
      Addr.Table.fold t.mems ~init:[] ~f:(fun ~key ~data diff ->
          match s#load key with
          | None -> add diff key data None
          | Some w -> 
            if w = data then diff
            else add diff key data (Some r))

let diff t ctxt = vars_diff t ctxt @ mems_diff t ctxt
