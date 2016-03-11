open Bap.Std

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
    | None -> Format.fprintf fmt "none" in
  let pp pp_src src ok er = 
    Format.fprintf fmt "%a: error %a (%a)\n"
      pp_src src ppo er Word.pp ok in 
  match t with 
  | Imm t -> pp Var.pp t.src t.ok t.er
  | Mem t -> pp Addr.pp t.src t.ok t.er
