open Core_kernel.Std
open Bap.Std
open Bap_traces.Std 

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
    | Other w -> 
      let s = Bitvector.string_of_value ~hex:true w in
      Printf.sprintf "other %s" s
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
    code : Chunk.t;
    ctxt : Bili.context;
    diff : diff list;
  } [@@deriving fields]
  
  let create code ctxt diff = {code; ctxt; diff}
end

type record = Record.t
type histo = (string * int) list [@@deriving bin_io, sexp]
type stat = [ 
  | `Right (** stands for correct compare results *)
  | `Undef (** instructions that weren't recognized by disassm *)
  | `Unlif (** instructions that weren't lifted *)
]

module type Common = sig
  type t 
  val create: unit -> t
  val histo: t -> histo
  val right: t -> int
  val wrong: t -> int
  val undef: t -> int
  val unlif: t -> int
  val succ: t -> stat -> t
  val pp: Format.formatter -> t -> unit
end

module Stats = struct
  type t = {
    right : int; (** correct compare results *)
    undef : int; (** instructions that weren't recognized by disassm *)
    unlif : int; (** instructions that weren't lifted *)
  } [@@deriving bin_io, sexp]

  let empty = {right = 0; undef = 0; unlif = 0;}

  let succ t = function 
    | `Right -> {t with right = Int.succ t.right}
    | `Undef -> {t with undef = Int.succ t.undef}
    | `Unlif -> {t with unlif = Int.succ t.unlif}

  let access {right; undef; unlif} = function 
    | `Right -> right
    | `Undef -> undef
    | `Unlif -> unlif

end

module Tab = String.Table

type tab = 
  | Cases of int Tab.t
  | Diffs of diffs list Tab.t
  [@@deriving bin_io, sexp]

module Base = struct

  type t = {
    wrong : tab;
    stats : Stats.t;
  } [@@deriving bin_io, sexp]

  let create_base wrong = {wrong; stats = Stats.empty;}

  let succ t f = {t with stats = Stats.succ t.stats f}
  let right t = Stats.access t.stats `Right
  let undef t = Stats.access t.stats `Undef
  let unlif t = Stats.access t.stats `Unlif
  let histo t = match t.wrong with
    | Cases tab -> Tab.to_alist tab
    | Diffs tab -> 
      Tab.fold ~init:[] tab ~f:(fun ~key ~data acc ->
          (key, List.length data) :: acc)

  let wrong t = match t.wrong with 
    | Cases tab -> 
      Tab.fold tab ~init:0 ~f:(fun ~key ~data acc -> data + acc)
    | Diffs tab -> 
      Tab.fold tab ~init:0 ~f:(fun ~key ~data acc -> List.length data + acc)

  let succ_wrong t insn_name diff = match t.wrong with
    | Cases tab -> 
      Tab.change tab insn_name
        ~f:(function 
            | None -> Some 1
            | Some cnt -> Some (cnt + 1))
    | Diffs tab -> 
      Tab.change tab insn_name
        ~f:(function 
            | None -> Some (diff :: [])
            | Some diffs -> Some (diff :: diffs))

  let to_brief t = match t.wrong with
    | Cases _ -> t
    | Diffs tab -> 
      let wrong = Cases (Tab.map tab ~f:(fun data -> List.length data)) in
      {wrong; stats = t.stats}

  let pp fmt t = 
    let ppr fmt (name, times) = 
      Format.fprintf fmt "%s mis-executed %d times\n" name times in
    List.iter ~f:(ppr fmt) (histo t);
    Format.fprintf fmt "correct: %d; wrong: %d; undefined: %d; unlifted %d\n"
      (right t) (wrong t) (undef t) (unlif t)
end

module Brief = struct
  include Base
  let create () = Cases (Tab.create ()) |> create_base 
  let succ_wrong t insn_name = succ_wrong t insn_name []
end

module type T = module type of String.Table


module Extended = struct
  include Base
  let create () = Diffs (Tab.create ()) |> create_base

  let find_all t insn_name = match t.wrong with
    | Cases _ -> []
    | Diffs tab -> 
      match Tab.find tab insn_name with
      | None -> []
      | Some diffs -> diffs

  let succ_wrong t insn_name diff = 
    succ_wrong t insn_name diff;
end


module Debug = struct

  type t = {
    wrong : record list Tab.t;
    stats : Stats.t;
  }

  let create () = {
    wrong = Tab.create ();
    stats = Stats.empty;
  }

  let succ t f = {t with stats = Stats.succ t.stats f}
  let right t = Stats.access t.stats `Right
  let undef t = Stats.access t.stats `Undef
  let unlif t = Stats.access t.stats `Unlif

  let succ_wrong t insn_name record =
    Tab.change t.wrong insn_name
      ~f:(function 
          | None -> Some [record]
          | Some rcs -> Some (record::rcs))

  let records t =
    Tab.fold t.wrong ~init:[] 
      ~f:(fun ~key ~data acc -> 
          let data' = List.rev_map ~f:(fun r -> key,r) data in
          acc @ data')

  let wrong t =
    Tab.fold t.wrong ~init:0
      ~f:(fun ~key ~data acc -> acc + List.length data)

  let histo t =
    Tab.fold t.wrong ~init:[]
      ~f:(fun ~key ~data acc -> (key, List.length data) :: acc)

  let find t insn_name =
    match Tab.find t.wrong insn_name with
    | None -> None
    | Some rcs -> List.hd rcs

  let find_all t insn_name =
    match Tab.find t.wrong insn_name with
    | None -> []
    | Some rcs -> rcs

  let pp fmt t = 
    let ppr fmt (name, times) = 
      Format.fprintf fmt "%s mis-executed %d times\n" name times in
    List.iter ~f:(ppr fmt) (histo t);
    Format.fprintf fmt "Total:\ncorrect: %d; wrong: %d; undefined: %d; unlifted: %d\n"
      (right t) (wrong t) (undef t) (unlif t)

  let to_brief t = 
    let tab = Cases (Tab.map t.wrong ~f:(fun data -> List.length data)) in
    Base.({wrong = tab; stats = t.stats})

  let to_extended t = 
    let tab = Tab.map t.wrong ~f:(fun data -> List.map data ~f:Record.diff) in
    Base.({wrong = Diffs tab; stats = t.stats})

end

type brief = Brief.t [@@deriving bin_io, sexp]
type extended = Extended.t [@@deriving bin_io, sexp]
type debug = Debug.t

let create_brief = Brief.create
let create_debug = Debug.create
let create_extended = Extended.create
