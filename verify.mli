open Bap.Std
open Bap_traces.Std

module Diff : sig
  type 'a diff = {
    src : 'a;
    ok  : word;
    er  : word option
  }

  type t = Imm of var diff | Mem of addr diff
  val pp: Format.formatter -> t -> unit
end

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

  (** [right t] - returns a number of right lifted instructions *)
  val right: t -> int

  (** [wrong t] - returns a number of wrong lifted instructions *)
  val wrong: t -> int
 
  (** [until_mismatch t] execute trace until first mismatch *)
  val until_mismatch: t -> t option

  (** [diverged t] - returns a list of diverged variables after last
      {step} call, i.e. such variables that were evaluated to wrong 
      result or weren't come to evaluation context at all. *)
  val diverged: t -> Diff.t list 

end

val create: arch -> (module V)

