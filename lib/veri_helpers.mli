open Core_kernel.Std
open Bap.Std
open Bap_traces.Std

val bil_of_chunk: arch -> Chunk.t -> (string * bil) Or_error.t
