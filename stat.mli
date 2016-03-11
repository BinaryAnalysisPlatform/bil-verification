type t [@@deriving bin_io, sexp]

val create: unit -> t

(** [right t] - returns a number of right lifted instructions *)
val right: t -> int

(** [wrong t] - returns a number of wrong lifted instructions *)
val wrong: t -> int

(** [undef t] - returns a number of unlifted instructions     *)
val undef: t -> int

(** [histo] - returns a list of instructions paired with number
    of wrong lifting cases. *)
val histo: t -> (string * int) list

val succ_right: t -> t
val succ_wrong: t -> t
val succ_undef: t -> t
val succ_histo: t -> string -> t
