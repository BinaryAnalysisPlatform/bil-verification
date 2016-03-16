open Core_kernel.Std
open Bap.Std
open Bap_traces.Std

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

module Diff = struct
  
  type er = 
    | Other of word (** result is differ from expected               *)
    | Unprovided    (** result is not provided at all                  *)
    | Undefined     (** result is undefined (read: has Bil.Bot type) *)
    | Type_error    (** result has other type                        *)
    [@@deriving bin_io, sexp]

  type r = {
    ok  : word;
    er  : er;
  } [@@deriving bin_io, sexp]

  type t = 
    | Imm of var * r 
    | Mem of addr * r
    [@@deriving bin_io, sexp]

  let string_of_er = function 
    | Other w -> "other " ^  Word.pps () w
    | Unprovided -> "unprovided"
    | Undefined -> "undefined"
    | Type_error -> "type_error"

  let ppr fmt t = 
    Format.fprintf fmt "%s (expexted: %a)" (string_of_er t.er) Word.pp t.ok

  let pp fmt t = match t with
    | Imm (var,r) ->
      Format.fprintf fmt "variable %a: %a"  Var.pp var ppr r
    | Mem (addr, r) -> 
      Format.fprintf fmt "memory %a: %a"  Addr.pp addr ppr r
end

module Diffs = struct
  type t = Diff.t list [@@deriving bin_io, sexp]
  let pp fmt t = 
    List.iter ~f:(fun d -> Format.fprintf fmt "%a\n" Diff.pp d) t
end

type diff = Diff.t [@@deriving bin_io, sexp]
type diffs = Diffs.t [@@deriving bin_io, sexp]

module Record = struct

  type t = {
    name : string;
    code : Chunk.t;
    ctxt : Bili.context;
    diff : diff list;
  } [@@deriving fields]
  
  let create name code ctxt diff = {name; code; ctxt; diff}
end

type record = Record.t
