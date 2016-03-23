open Core_kernel.Std
open Bap.Std
open Bap_traces.Std 

module Tab = String.Table

type histo = (string * int) list [@@deriving bin_io, sexp]

type succ = [ 
  | `Right (** stands for correct compare results *)
  | `Wrong of string      (** errors in comparisons     *)
  | `Undef (** instructions that weren't recognized by disassm *)
]

type t = {
  wrong : int Tab.t;
  right : int;
  undef : int;
} [@@deriving bin_io]

let create () = {
  wrong = Tab.create ();
  right = 0;
  undef = 0;
}  

let right t = t.right
let undef t = t.undef

let succ t = function 
  | `Right -> {t with right = Int.succ t.right}
  | `Undef -> {t with undef = Int.succ t.undef}
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
  Format.fprintf fmt "correct: %d; wrong: %d; undefined: %d;\n"
    (right t) (wrong t) (undef t)
