open Bap.Std

(**  Report module.

     Report is intended to get some quantitive results: 
     - number of successful comparisons
     - number of unsuccessful comparisons
     - number of instructions, that either weren't recognized by disasm 
       either weren't lifted by BIL
     - histogram  as mapping from instruction name to number of 
       unsuccessful comparison casses.
*)

type histo = (string * int) list [@@deriving bin_io, sexp]
type succ = [ 
  | `Right 
  | `Undef 
  | `Wrong of string 
]

type t [@@deriving bin_io, sexp]

val create: unit -> t
val histo: t -> histo
val right: t -> int
val wrong: t -> int
val undef: t -> int
val succ : t -> succ -> t
val pp: Format.formatter -> t -> unit


