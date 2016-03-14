open Bap.Std
open Bap_traces.Std 

module Diff : sig

  type 'a diff = {
    src : 'a;
    ok  : word;
    er  : Bil.result option
  }

  type t = 
    | Imm of var diff
    | Mem of addr diff

  val of_var: var -> word -> Bil.result option -> t 
  val of_mem: addr -> word -> Bil.result option -> t 
  val pp: Format.formatter -> t -> unit
end

type diff = Diff.t

module Record : sig
  type t 
  val create: Chunk.t -> Bili.context -> diff list -> t
  val code: t -> Chunk.t
  val ctxt: t -> Bili.context
  val diff: t -> diff list
end

type record = Record.t

module type Common = sig
  type t 
  val create: unit -> t
  val histo: t -> (string * int) list
  val right: t -> int
  val wrong: t -> int
  val undef: t -> int
  val succ_right: t -> t
  val succ_undef: t -> t
  val pp: Format.formatter -> t -> unit
end

module Light : sig
  type t [@@deriving bin_io, sexp] 
  include Common with type t := t
  val succ_wrong: t -> string -> unit
end

module Debug : sig
  include Common
  val succ_wrong: t -> string -> record -> unit
  val find: t -> string -> record list
  val records: t -> (string * record) list
end

type light = Light.t [@@deriving bin_io, sexp] 
type debug = Debug.t

val create_light: unit -> light
val create_debug: unit -> debug
