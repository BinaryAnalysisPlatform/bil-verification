open Core_kernel.Std
open Bap.Std

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

module Make (Types : Veri_types.T) : T = struct
  open Types

  type vars = word Var.Table.t
  type mems = word Addr.Table.t

  type t = {
    vars : vars;
    mems : mems;
  }

  let pp fmt {vars; mems} = 
    let ppv (var, word) = 
      Format.fprintf fmt "%a %a\n" Var.pp var Word.pp word in
    let ppm (addr, word) = 
      Format.fprintf fmt "%a %a\n" Word.pp addr Word.pp word in
    List.iter ~f:ppv (Var.Table.to_alist vars);
    List.iter ~f:ppm (Addr.Table.to_alist mems)

  let update_var t var word = Var.Table.set t.vars ~key:var ~data:word
  let update_mem t addr word = Addr.Table.set t.mems ~key:addr ~data:word

  let init t =
    let open CPU in
    let reg_size reg = match Var.typ reg with
      | Type.Imm sz -> Bitvector.of_int ~width:sz 0 
      | _ -> invalid_arg "Veri_context.init: must be Imm" in
    let init_reg reg = 
      update_var t reg (reg_size reg) in
    update_var t zf Bitvector.b0;
    update_var t cf Bitvector.b0;
    update_var t vf Bitvector.b0;
    update_var t nf Bitvector.b0;
    Var.Set.iter gpr init_reg

  let create () = 
    let vars = Var.Table.create () in
    let mems = Addr.Table.create () in
    let t = {vars; mems;} in
    init t;
    t

  let endian = Arch.endian arch

  let storage_of_result v = 
    let open Bil in
    match Result.value v with 
    | Mem s -> Some s
    | _ -> None

  let save s addr data = 
    Bitvector.enum_bytes data endian |>
    Seq.fold ~init:(s, addr) 
      ~f:(fun (s, addr) w -> s#save addr w, Bitvector.succ addr) |>
    fst

  let load s addr width = 
    let load_word addr data = 
      match s#load addr with 
      | None -> None
      | Some word -> 
        let w,w' = match endian with
          | LittleEndian -> word, data
          | BigEndian -> data, word in
        Some (Bitvector.concat w w') in
    let rec loop addr data = 
      if Bitvector.bitwidth data = width then Some data
      else
        match load_word addr data with
        | None -> None
        | Some w -> loop (Bitvector.succ addr) w in
    let data = Bitvector.of_int ~width:0 0 in
    loop addr data

  let to_bili_context t = 
    let ctxt = Var.Table.fold t.vars ~init:(new Bili.context)
        ~f:(fun ~key ~data ctxt ->
            let ctxt,r = ctxt#create_word data in
            ctxt#update key r) in
    let s = Addr.Table.fold t.mems ~init:(new Bil.Storage.sparse)
        ~f:(fun ~key ~data s -> save s key data) in
    let ctxt, r = ctxt#create_storage s in
    ctxt#update CPU.mem r

  let is_mem = function 
    | Bil.Mem _ -> true
    | _ -> false

  let word_of_result v = 
    let open Bil in
    match Result.value v with
    | Imm w -> Some w
    | _ -> None

  let diff_imm var ok er = Diff.(Imm (var, {ok; er})) 
  let diff_mem addr ok er = Diff.(Mem (addr, {ok; er})) 

  let vars_diff t ctxt = 
    Var.Table.fold t.vars ~init:[] ~f:(fun ~key ~data diffs ->
        match ctxt#lookup key with
        | None -> diff_imm key data Diff.Unprovided :: diffs
        | Some r -> match word_of_result r with 
          | Some w -> 
            if Bitvector.equal data w then diffs 
            else diff_imm key data (Diff.Other w) :: diffs
          | None -> 
            if is_mem (Bil.Result.value r) then 
              diff_imm key data Diff.Type_error :: diffs
            else diff_imm key data Diff.Undefined :: diffs)
      
  let full_mem_diff t =
    Addr.Table.fold t.mems ~init:[]
      ~f:(fun ~key ~data diffs -> diff_mem key data Diff.Unprovided :: diffs) 

  let mems_diff t ctxt = 
    match ctxt#lookup CPU.mem with
    | None -> full_mem_diff t
    | Some r -> match storage_of_result r with
      | None -> full_mem_diff t
      | Some s ->
        Addr.Table.fold t.mems ~init:[] ~f:(fun ~key ~data diffs ->
            let w = Bitvector.bitwidth data in
            match load s key w with
            | None -> diff_mem key data Diff.Unprovided :: diffs
            | Some w -> 
              if Bitvector.equal w data then diffs
              else diff_mem key data (Diff.Other w) :: diffs)

  let diff t ctxt = vars_diff t ctxt @ mems_diff t ctxt

  let has_diff_vars t ctxt = 
    let exists_equal key data = match ctxt#lookup key with
      | None -> false
      | Some r -> match word_of_result r with 
        | Some w -> Bitvector.equal w data
        | None -> false in
    let is_diff ~key ~data = not (exists_equal key data) in    
    Var.Table.existsi t.vars ~f:is_diff

  let has_diff_mems t ctxt = 
    let exists_equal key data =
      let open Option in
      ctxt#lookup CPU.mem >>= storage_of_result >>= 
      fun s -> load s key (Bitvector.bitwidth data) >>= 
      fun w -> Some (Bitvector.equal w data) in
    let is_diff ~key ~data = match exists_equal key data with
      | Some r -> not r
      | None -> true in
    Addr.Table.existsi t.mems ~f:is_diff

  let is_different t ctxt = 
    has_diff_vars t ctxt || has_diff_mems t ctxt

  let exists_var t var = Var.Table.mem t.vars var
  let exists_mem t addr = Addr.Table.mem t.mems addr
end
