open Bap.Std

module type T = sig
  include Target
  val arch: arch
end


val t_of_arch: arch -> (module T)
