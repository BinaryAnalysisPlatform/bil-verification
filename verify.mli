open Bap.Std
open Bap_traces.Std

module type V = sig

  type t

  (** [create trace] - returns a fresh t type *)
  val create: Trace.t -> t

  (** [execute trace] - executes whole trace *)
  val execute: Trace.t -> t  

  (** [until_mismatch t] execute trace until first mismatch *)
  val until_mismatch: t -> Diff.t list * t option

  val stat: t -> Stat.t

end

val create: arch -> (module V)

