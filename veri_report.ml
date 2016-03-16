open Core_kernel.Std
open Bap.Std
open Bap_traces.Std 

module Tab = String.Table

type histo = (string * int) list [@@deriving bin_io, sexp]

type succ = [ 
  | `Right (** stands for correct compare results *)
  | `Wrong of string      (** errors in comparisons     *)
  | `Undef (** instructions that weren't recognized by disassm *)
  | `Unlif (** instructions that weren't lifted *)
]

module Stats = struct
  type t = {
    right : int; (** correct compare results *)
    undef : int; (** instructions that weren't recognized by disassm *)
    unlif : int; (** instructions that weren't lifted *)
  } [@@deriving bin_io, sexp]

  let empty = {right = 0; undef = 0; unlif = 0;}

  let succ t = function 
    | `Right -> {t with right = Int.succ t.right}
    | `Undef -> {t with undef = Int.succ t.undef}
    | `Unlif -> {t with unlif = Int.succ t.unlif}

  let access {right; undef; unlif} = function 
    | `Right -> right
    | `Undef -> undef
    | `Unlif -> unlif
end

type t = {
  wrong : int Tab.t;
  right : int;
  undef : int;
  unlif : int;
} [@@deriving bin_io, fields, sexp]

let create () = {
  wrong = Tab.create ();
  right = 0;
  undef = 0;
  unlif = 0;
}  

let succ t = function 
  | `Right -> {t with right = Int.succ t.right}
  | `Undef -> {t with undef = Int.succ t.undef}
  | `Unlif -> {t with unlif = Int.succ t.unlif}
  | `Wrong name ->
    Tab.change t.wrong name
      ~f:(function 
          | None -> Some 1
          | Some cnt -> Some (cnt + 1));
    t

let histo t = Tab.to_alist t.wrong
let wrong t = Tab.fold t.wrong ~init:0 ~f:(fun ~key ~data acc -> acc + data)

let pp fmt t = 
  let ppr fmt (name, times) = 
    Format.fprintf fmt "%s mis-executed %d times\n" name times in
  List.iter ~f:(ppr fmt) (histo t);
  Format.fprintf fmt "correct: %d; wrong: %d; undefined: %d; unlifted %d\n"
    (right t) (wrong t) (undef t) (unlif t)
