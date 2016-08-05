
open Regular.Std
type t [@@deriving bin_io, sexp]
type stat = t [@@deriving bin_io, sexp]
type summary  [@@deriving bin_io, sexp]

val create : unit -> t
val notify : t -> Veri_error.t -> t
val failbil : t -> string -> t
val success : t -> string -> t

include Regular with type t := t

(** Terms:
    successed       - instructions, that were successfuly lifted and evaluted
                      without any devergence from trace at least once (or more);
    misexecuted     - instructions, that were successfuly lifted and evaluted
                      with devergence from trace at least once (or more);
    abs_successed   - the same as successed but haven't any devergences from
                      trace at all;
    abs_misexecuted - the same as misexecuted but haven't any successfully
                      matches with trace at all;
    mislifted       - instructions, that weren't recognized by lifter;
    overloaded      - chunks that contains more that one instruction;
    damaged         - chunks that failed to be represented as memory;
    undisasmed      - chunks that were represented as memory, but failed to
                      be disasmed;
    total           - whole count of cases above *)

module Abs : sig
  type t = stat -> int
  val successed       : t
  val abs_successed   : t
  val misexecuted     : t
  val abs_misexecuted : t
  val overloaded      : t
  val damaged         : t
  val undisasmed      : t 
  val mislifted       : t
  val total           : t
end

module Rel : sig
  type t = stat -> float
  val successed       : t
  val abs_successed   : t
  val misexecuted     : t
  val abs_misexecuted : t
  val overloaded      : t
  val damaged         : t
  val undisasmed      : t
  val mislifted       : t
end

module Names : sig
  type t = stat -> string list
  val successed       : t
  val abs_successed   : t
  val misexecuted     : t
  val abs_misexecuted : t
  val mislifted       : t
end

module Summary : sig
  type t = summary
  val empty : t
  val pp_stat : Format.formatter -> t -> unit
  val stats : t -> (string * stat) list
  val add : t -> string -> stat -> t
  include Regular with type t := t
end
