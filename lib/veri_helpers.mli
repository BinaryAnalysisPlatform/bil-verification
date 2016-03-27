open Core_kernel.Std
open Bap.Std
open Bap_traces.Std

type full_insn = Disasm_expert.Basic.full_insn

(** [insns_of_chunk arch chunk] - returns an intruction list 
    from code [chunk] *)
val insns_of_chunk: arch -> Chunk.t -> 
  (string * (mem * full_insn) list) Or_error.t

(** [bil_of_insns arch insns] - returns bil from instruction
    list [insns]*)
val bil_of_insns: arch -> (mem * full_insn) list -> bil Or_error.t

