open Bap.Std
open Bap_traces.Std

module type V = sig

  type t

  (** [create trace] - returns a fresh t type *)
  val create: Trace.t -> t

  (** [execute trace] - executes whole trace *)
  val execute: Trace.t -> t  

  (** [step t] - processes such number events from trace, that are needed
      to get next compare result. And returns None if number events in a 
      trace is not enough to do it. *)
  val step: t -> t option

  (** [correct t] - returns a number of correct lifted instructions *)
  val correct: t -> int

  (** [incorrect t] - returns a number of incorrect lifted instructions *)
  val incorrect: t -> int

  val until_compare : t -> t option
    
  val until_mismatch: t -> (Bili.context * Bili.context) option

end

val create: arch -> (module V)

