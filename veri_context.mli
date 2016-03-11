open Bap.Std

type t 

val of_memory : var -> t
val to_bili_context : t -> Bili.context
val update_var : t -> var -> word -> unit
val update_mem : t -> addr -> word -> unit 
val diff : t -> Bili.context -> Diff.t list
val is_different: t -> Bili.context -> bool

