open Bap.Std

type 'a diff = {
  src : 'a;
  ok  : word;
  er  : Bil.result option
}

type t = Imm of var diff | Mem of addr diff

val of_var: var -> word -> Bil.result option -> t 
val of_mem: addr -> word -> Bil.result option -> t 

val pp: Format.formatter -> t -> unit
