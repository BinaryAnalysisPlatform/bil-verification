open Bap.Std
open Bap_traces.Std 

(**  Report module.

     Report is intended to get some quantitive results: 
     - number of successful comparisons
     - number of unsuccessful comparisons
     - number of instructions, that weren't recognized by disasm 
     - number of instructions, that weren't lifted.
     - histogram  as mapping from instruction name to number of 
       unsuccessful comparison casses.
*)

type histo = (string * int) list [@@deriving bin_io, sexp]
type succ = [ `Right | `Wrong of string | `Undef | `Unlif ]

type t [@@deriving bin_io, sexp]

val create: unit -> t
val histo: t -> histo
val right: t -> int
val wrong: t -> int
val undef: t -> int
val unlif: t -> int
val succ : t -> succ -> t
val pp: Format.formatter -> t -> unit


