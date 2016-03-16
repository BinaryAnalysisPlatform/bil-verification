open Bap.Std

open Veri_report
open Veri_types

module type T = sig

  type t

  val create: unit -> t
  val to_bili_context : t -> Bili.context
  val update_var : t -> var -> word -> unit
  val update_mem : t -> addr -> word -> unit 
  val exists_var : t -> var -> bool
  val exists_mem : t -> addr -> bool
  val diff : t -> Bili.context -> diffs
  val is_different: t -> Bili.context -> bool
  val pp: Format.formatter -> t -> unit
end

module Make(T : Veri_types.T) : T
