open Core_kernel.Std
open Bap.Std
open Bap_traces.Std 

module Diff = struct

  type 'a diff = {
    src : 'a;
    ok  : word;
    er  : Bil.result option
  } 

  type t = Imm of var diff | Mem of addr diff

  let of_var src ok er = Imm {src; ok; er}
  let of_mem src ok er = Mem {src; ok; er}

  let pp fmt t =
    let open Bil.Result in
    let ppo fmt = function
      | Some r -> Format.fprintf fmt "%a" Value.pp (value r)
      | None -> Format.fprintf fmt "none" in
    let pp pp_src src ok er = 
      Format.fprintf fmt "%a: %a (expected: %a)\n"
        pp_src src ppo er Word.pp ok in 
    match t with 
    | Imm t -> pp Var.pp t.src t.ok t.er
    | Mem t -> pp Addr.pp t.src t.ok t.er

end

type diff = Diff.t

module Record = struct

  type t = {
    code : Chunk.t;
    ctxt : Bili.context;
    diff : diff list;
  } [@@deriving fields]
  
  let create code ctxt diff = {code; ctxt; diff}
end

type record = Record.t
type histo = (string * int) list

module type Common = sig
  type t 
  val create: unit -> t
  val histo: t -> histo
  val right: t -> int
  val wrong: t -> int
  val undef: t -> int
  val succ_right: t -> t
  val succ_undef: t -> t
  val pp: Format.formatter -> t -> unit
end

module Tab = String.Table

module Light = struct

  type t = {
    wrong : int Tab.t;
    right : int;
    undef : int;
  } [@@deriving bin_io, sexp]

  let create () = {
    wrong = Tab.create ();
    right = 0;
    undef = 0;
  }
  
  let succ_right t = {t with right = Int.succ t.right}
  let succ_undef t = {t with undef = Int.succ t.undef}
  let right t = t.right
  let undef t = t.undef

  let succ_wrong: t -> string -> unit = fun t insn_name ->
    Tab.change t.wrong insn_name
      ~f:(function 
          | None -> Some 1
          | Some cnt -> Some (cnt + 1))

  let wrong: t -> int = fun t ->
    Tab.fold t.wrong ~init:0 ~f:(fun ~key ~data acc -> data + acc)

  let histo: t -> histo = fun t -> Tab.to_alist t.wrong

  let pp fmt t = 
    let ppr fmt (name, times) = 
      Format.fprintf fmt "%s mis-executed %d times\n" name times in
    List.iter ~f:(ppr fmt) (histo t);
    Format.fprintf fmt "Total:\ncorrect: %d; wrong: %d; undefined: %d\n"
      t.right (wrong t) t.undef

end 

module Debug = struct

  type t = {
    wrong : record list Tab.t;
    right : int;
    undef : int;
  }

  let create () = {
    wrong = Tab.create ();
    right = 0;
    undef = 0;
  }
  
  let succ_right t = {t with right = Int.succ t.right}
  let succ_undef t = {t with undef = Int.succ t.undef}
  let right t = t.right
  let undef t = t.undef

  let succ_wrong: t -> string -> record -> unit = fun t insn_name record ->
    Tab.change t.wrong insn_name
      ~f:(function 
          | None -> Some [record]
          | Some rcs -> Some (record::rcs))

  let records: t -> (string * record) list = fun t ->
    Tab.fold t.wrong ~init:[] 
      ~f:(fun ~key ~data acc -> 
          let data' = List.rev_map ~f:(fun r -> key,r) data in
          acc @ data')

  let wrong: t -> int = fun t ->
    Tab.fold t.wrong ~init:0
      ~f:(fun ~key ~data acc -> acc + List.length data)

  let histo: t -> histo = fun t -> 
    Tab.fold t.wrong ~init:[]
      ~f:(fun ~key ~data acc -> (key, List.length data) :: acc)

  let find: t -> string -> record list = fun t insn_name ->
    match Tab.find t.wrong insn_name with
    | None -> []
    | Some rcs -> rcs

  let pp fmt t = 
    let ppr fmt (name, times) = 
      Format.fprintf fmt "%s mis-executed %d times\n" name times in
    List.iter ~f:(ppr fmt) (histo t);
    Format.fprintf fmt "Total:\ncorrect: %d; wrong: %d; undefined: %d\n"
      t.right (wrong t) t.undef

end

type light = Light.t [@@deriving bin_io, sexp]
type debug = Debug.t

let create_light = Light.create
let create_debug = Debug.create
