open Core_kernel.Std
open Bap.Std
open Bap_traces.Std 

module Diff = struct

  type 'a diff = {
    src : 'a;
    ok  : word;
    er  : Bil.result option
  }

  type t = Imm of var diff | Mem of addr diff

  let of_var src ok er = Imm {src; ok; er}
  let of_mem src ok er = Mem {src; ok; er}

  let pp fmt t =
    let open Bil.Result in
    let ppo fmt = function
      | Some r -> Format.fprintf fmt "%a" Value.pp (value r)
      | None -> Format.fprintf fmt "none!" in
    let pp pp_src src ok er = 
      Format.fprintf fmt "%a: error %a (%a)\n"
        pp_src src ppo er Word.pp ok in 
    match t with 
    | Imm t -> pp Var.pp t.src t.ok t.er
    | Mem t -> pp Addr.pp t.src t.ok t.er

end

module Record = struct

  type t = {
    code : Chunk.t;
    ctxt : Bili.context;
    diff : Diff.t list;
  }
  
  let create code ctxt diff = {code; ctxt; diff}

end

type record = Record.t

module Map = String.Map

type t = {
  wrong : (record list) Map.t;
  right : int;
  undef : int;
}

let empty = {
  wrong = Map.empty;
  right = 0;
  undef = 0;
}

let add t insn_name record = 
  let wrong = Map.change t.wrong insn_name
      ~f:(function 
          | None -> Some [record]
          | Some rcs -> Some (record::rcs)) in
  {t with wrong } 

let succ_right t = {t with right = Int.succ t.right}
let succ_undef t = {t with undef = Int.succ t.undef}

let records t =
  Map.fold t.wrong ~init:[] 
    ~f:(fun ~key ~data acc ->
        let recs = List.map ~f:(fun r -> key,r) data in
        acc @ recs)

let right t = t.right
let wrong t = Map.length t.wrong
let undef t = t.undef
let histo t = 
  Map.fold t.wrong ~init:[]
    ~f:(fun ~key ~data acc -> (key, List.length data) :: acc)

let pp fmt t = 
  let ppr fmt (name, times) = 
    Format.fprintf fmt "%s mis-executed %d times\n" name times in
  List.iter ~f:(ppr fmt) (histo t);
  Format.fprintf fmt "Total:\ncorrect: %d; wrong: %d; undefined: %d\n"
    t.right (wrong t) t.undef
