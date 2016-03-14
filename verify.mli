open Bap.Std
open Bap_traces.Std
open Veri_report

module type V = sig

  type t

  (** [create trace] - returns a fresh t type *)
  val create: Trace.t -> t

  (** [until_mismatch t] execute trace until first mismatch *)
  val until_mismatch: t -> t option

  (** [execute trace] - executes whole trace *)
  val execute: Trace.t -> t  

  (** [find trace insn_name] - execute whole trace
      until first mis-matching instruction [insn_name] *)
  val find: Trace.t -> string -> Record.t option

  val report: t -> Veri_report.t

end

val create: arch -> (module V)

