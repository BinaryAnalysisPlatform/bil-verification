open Core_kernel.Std
open Bap.Std
open Bap_traces.Std 

module Map = String.Map

type hist = (string * int) list [@@deriving bin_io, sexp]

type s = [ 
  | `Right (** stands for correct compare results *)
  | `Undsm (** stands for disasm fails            *)
  | `Wrong of string (** errors in comparisons    *)
  | `Unlif of string (** stands for lifter fails  *)
]

type t = {
  wrong : int Map.t;
  unlif : int Map.t;
  right : int;
  undsm : int;
} [@@deriving bin_io, fields, sexp]

let create () = {
  wrong = Map.empty;
  unlif = Map.empty;
  right = 0;
  undsm = 0;
}  

let update m s = 
  Map.change m s
    ~f:(function 
        | None -> Some 1
        | Some cnt -> Some (cnt + 1))

let count_of_map m = 
  Map.fold m ~init:0 ~f:(fun ~key ~data acc -> acc + data)

let succ t = function 
  | `Right -> {t with right = Int.succ t.right}
  | `Undsm -> {t with undsm = Int.succ t.undsm}
  | `Unlif name -> {t with unlif = update t.unlif name}
  | `Wrong name -> {t with wrong = update t.wrong name}

let hist_wrong t = Map.to_alist t.wrong
let hist_unlif t = Map.to_alist t.unlif
let wrong t = count_of_map t.wrong
let unlif t = count_of_map t.unlif

let pp fmt t = 
  let ppr fmt what (name, times) = 
    Format.fprintf fmt "%s %s %d times\n" name what times in
  List.iter ~f:(ppr fmt "mis-executed") (hist_wrong t);
  List.iter ~f:(ppr fmt "mis-lifted") (hist_unlif t);
  Format.fprintf fmt 
    "correct: %d; wrong: %d; undisasmed: %d; unlifted: %d\n"
    (right t) (wrong t) (undsm t) (unlif t)
