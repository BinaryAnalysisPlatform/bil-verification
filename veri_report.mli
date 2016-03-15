open Bap.Std
open Bap_traces.Std 

(**  Report module.

     Provides mechanism for report creation.
     There are three possible ways, and each next is more detail
     then previous one. 

     Brief report is intended to get some quantitive results: 
     - number of successful comparisons
     - number of unsuccessful comparisons
     - number of instructions, that weren't recognized by disasm 
     - number of instructions, that weren't lifted.
     - histogram  as mapping from instruction name to number of 
       unsuccessful comparison casses.

     Extended report holds everything above and also information about 
     difference in results of executed instruction and information in trace. 
     
     Debug report holds everything above and adds information about initial 
     context and raw code, that contains instruction. So one could peroform 
     instruction lifting and execution on given context.

     Diff module.
     Describes a diffrence between trace information and executed 
     instruction.

     Record module.
     Describes full information about diverging between trace and
     executed instruction.

*)

module Diff : sig
  type er = 
    | Other of word (** result is differ from expected               *)
    | Unprovided    (** result is not provided at all                  *)
    | Undefined     (** result is undefined (read: has Bil.Bot type) *)
    | Type_error    (** result has other type                        *)
    [@@deriving bin_io, sexp]

  type r = {
    ok  : word;
    er  : er;
  } [@@deriving bin_io, sexp]

  type t = 
    | Imm of var * r 
    | Mem of addr * r
    [@@deriving bin_io, sexp]

  val pp: Format.formatter -> t -> unit 
end

module Diffs : sig
  type t = Diff.t list [@@deriving bin_io, sexp]
  val pp: Format.formatter -> t -> unit 
end

type diff = Diff.t [@@deriving bin_io, sexp]

(** diff list represents a list of diverged variables and
    memory content for one instruction *)
type diffs = Diffs.t [@@deriving bin_io, sexp]

module Record : sig
  type t 
  val create: Chunk.t -> Bili.context -> diff list -> t
  val code: t -> Chunk.t
  val ctxt: t -> Bili.context
  val diff: t -> diff list
end

type record = Record.t
type histo = (string * int) list [@@deriving bin_io, sexp]
type stat = [ `Right | `Undef | `Unlif ]

module type Common = sig
  type t 
  val create: unit -> t
  val histo: t -> histo
  val right: t -> int
  val wrong: t -> int
  val undef: t -> int
  val unlif: t -> int
  val succ : t -> stat -> t
  val pp: Format.formatter -> t -> unit
end

module Brief : sig
  type t [@@deriving bin_io, sexp] 
  include Common with type t := t
  val succ_wrong: t -> string -> unit
end

type brief = Brief.t [@@deriving bin_io, sexp] 

module Extended : sig
  type t [@@deriving bin_io, sexp] 
  include Common with type t := t
  val succ_wrong: t -> string -> diff list -> unit
  val find_all: t -> string -> diffs list
  val to_brief: t -> brief
end

type extended = Extended.t [@@deriving bin_io, sexp] 

module Debug : sig
  include Common
  val succ_wrong: t -> string -> record -> unit
  val find: t -> string -> record option
  val find_all: t -> string -> record list
  val records: t -> (string * record) list
  val to_brief: t -> extended
  val to_extended: t -> extended
end

type debug = Debug.t

val create_brief: unit -> brief
val create_debug: unit -> debug
val create_extended: unit -> extended


