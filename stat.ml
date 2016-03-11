open Core_kernel.Std

module Map = String.Map

module type M = module type of Map

type t = {
  right : int;   (** right lifted instructions count  *)
  wrong : int;   (** wrong lifted instructions count  *)
  undef : int;   (** undefined instructions count     *)
  histo : int Map.t; (** histogram of wrong           *)
} [@@deriving bin_io, fields, sexp]

let histo t = Map.to_alist t.histo
let create () = {right=0; wrong=0; undef=0; histo = Map.empty;}
let succ_wrong t = {t with wrong = t.wrong + 1}
let succ_right t = {t with right = t.undef + 1}
let succ_undef t = {t with right = t.undef + 1}
let succ_histo t name = 
  let histo = 
    match Map.find t.histo name with
    | Some count -> Map.update t.histo name (fun _ -> (count + 1))
    | None -> Map.add t.histo name 1 in
  {t with histo}
