open Bap.Std
open Bap_traces.Std

module type T = sig
  include Target
  val arch: arch
end

val t_of_arch: arch -> (module T)

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

  (** [create insn_name code ctxt diff]  *)
  val create: string -> Chunk.t -> Bili.context -> diff list -> t

  val code: t -> Chunk.t
  val ctxt: t -> Bili.context
  val diff: t -> diff list
end

type record = Record.t
