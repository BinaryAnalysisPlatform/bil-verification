open Bap.Std

module type A = sig
  val arch: arch
end

module type T = sig
  include Target
  include A
end

module Make(A:A)(Target:Target) : T = struct
  include Target
  include A
end

let t_of_arch arch = 
  let module Target = (val target_of_arch arch) in
  let module A = struct let arch = arch end in
  (module Make(A)(Target) : T)  



