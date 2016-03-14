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

module Record : sig
  type t 
  val create: Chunk.t -> Bili.context -> Diff.t list -> t
  val code: t -> Chunk.t
  val ctxt: t -> Bili.context
  val diff: t -> Diff.t list
end

type t

val create: unit -> t

(** [succ_wrong t insn_name record] - adds an corrupted instruction
    description *)
val succ_wrong: t -> string -> Record.t option -> unit

(** [succ_right t] - increments a counter of correctly lifted and
    compared instructions *)
val succ_right: t -> t

(** [succ_right t] - increments a counter of times when instruction lifting
    was failed, i.e. it wasn't even any BIL code to analize. *)
val succ_undef: t -> t 

val records: t -> (string * Record.t) list
val right: t -> int
val wrong: t -> int
val undef: t -> int

(** [histo t] - return a list of instruction names coupled with number of 
    theirs problem cases *)
val histo: t -> (string * int) list

val find: t -> string -> Record.t option list

val pp: Format.formatter -> t -> unit
