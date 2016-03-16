open Bap.Std
open Bap_traces.Std
open Veri_types

module type V = sig

  type t

  (** [create trace] - returns a fresh t type. *)
  val create: Trace.t -> t

  (** [execute trace] - executes whole trace and
      return brief description of execution process *)
  val execute: Trace.t -> Veri_report.t

  (** [step t] - executes trace until first mis-match *)
  val step: t -> (Record.t * t) option

  (** [find trace insn_name] - returns a count of
      mis-matches for instruction [insn_name] *)
  val count: Trace.t -> string -> int option

  (** [find trace insn_name] - executes whole trace
      until first mis-matching instruction [insn_name]. *)
  val find: Trace.t -> string -> Record.t option

  (** [find_all trace insn_name] - returns all records for
      given instruction [insn_name] from trace. *)
  val find_all: Trace.t -> string -> Record.t list

  (** [fold t ~init ~f] - applies [f] for each record *)
  val fold: t -> init:'a -> f:(Record.t -> 'a -> 'a) -> 'a

  (** [iter t ~f] - iteration over records *)
  val iter: t -> f:(Record.t -> unit) -> unit

  (** [report t] - returns a report of trace execution. *)
  val report: t -> Veri_report.t

end

val create: arch -> (module V)


