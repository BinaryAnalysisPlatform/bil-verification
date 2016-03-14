open Bap.Std
open Bap_traces.Std
open Veri_report


module type V = sig

  type t

  (** [create trace] - returns a fresh t type. *)
  val create: Trace.t -> t

  (** [execute trace] - executes whole trace *)
  val execute: Trace.t -> t  

  (** [report t] - returns a report of trace execution. *)
  val report: t -> Veri_report.light

end

module type D = sig

  type t

  (** [create trace] - returns a fresh t type. *)
  val create: Trace.t -> t

  (** [until_mismatch t] executes trace until first mismatch. *)
  val until_mismatch: t -> t option

  (** [find trace insn_name] - executes whole trace
      until first mis-matching instruction [insn_name]. *)
  val find: Trace.t -> string -> Record.t option

  (** [report t] - returns a report of trace execution. *)
  val report: t -> Veri_report.debug

end

val create: arch -> (module V)
val create_debug: arch -> (module D)

