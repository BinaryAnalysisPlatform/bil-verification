open Core_kernel.Std

(**  Report module.

     Report is intended to get some quantitive results: 
     - number of successful comparisons;
     - number of unsuccessful comparisons;
     - number of instructions, that weren't recognized by disasm;
     - number of instructions, that weren't lifted by BIL;
     - histogram  as mapping from instruction name to number of 
       unsuccessful comparison casses.
*)

type hist = (string * int) list [@@deriving bin_io, sexp]

type s = [ 
  | `Right 
  | `Undsm 
  | `Unlif of string
  | `Wrong of string 
]

type t [@@deriving bin_io, sexp]

val create: unit -> t
val right: t -> int
val wrong: t -> int
val undsm: t -> int
val unlif: t -> int
val succ : t -> s -> t
val hist_wrong: t -> hist
val hist_unlif: t -> hist
val pp: Format.formatter -> t -> unit


