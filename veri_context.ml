open Core_kernel.Std
open Bap.Std

module type T = sig
  type t
  val create: unit -> t
  val to_bili_context : t -> Bili.context
  val update_var : t -> var -> word -> unit
  val update_mem : t -> addr -> word -> unit 
  val exists_var : t -> var -> bool
  val exists_mem : t -> addr -> bool
  val diff : t -> Bili.context -> Diff.t list
  val is_different: t -> Bili.context -> bool
end

module Make (Types : Veri_types.T) : T = struct
  open Types

  type vars = word Var.Table.t
  type mems = word Addr.Table.t
    
  type t = {
    vars : vars;
    mems : mems;
  }

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

  let word_of_result v = 
    let open Bil in
    match Result.value v with
    | Imm w -> Some w
    | _ -> None

  let vars_diff t ctxt = 
    let add diff src ok er = Diff.of_var src ok er :: diff in
    Var.Table.fold t.vars ~init:[] ~f:(fun ~key ~data diff ->
        match ctxt#lookup key with
        | None -> add diff key data None
        | Some r -> match word_of_result r with 
          | Some w -> 
            if data = w then diff 
            else add diff key data (Some r)
          | None -> add diff key data (Some r)) 

  let mems_diff t ctxt = 
    let add diff src ok er = Diff.of_mem src ok er :: diff in
    let all () = 
      Addr.Table.fold t.mems ~init:[]
        ~f:(fun ~key ~data diff -> add diff key data None) in
    match ctxt#lookup CPU.mem with
    | None -> all ()
    | Some r -> match storage_of_result r with
      | None -> all ()
      | Some s ->
        Addr.Table.fold t.mems ~init:[] ~f:(fun ~key ~data diff ->
            let w = Bitvector.bitwidth data in
            match load s key w with
            | None -> add diff key data None
            | Some w -> 
              if w = data then diff
              else add diff key data (Some r))

  let diff t ctxt = 
    vars_diff t ctxt @ mems_diff t ctxt

  let has_diff_vars t ctxt = 
    let exists_equal ~key ~data = match ctxt#lookup key with
      | None -> false
      | Some r -> match word_of_result r with 
        | Some w -> w = data
        | None -> false in
    not (Var.Table.existsi t.vars ~f:exists_equal)

  let has_diff_mems t ctxt = 
    let exists_equal key data =
      let open Option in
      ctxt#lookup CPU.mem >>= storage_of_result >>= 
      fun s -> s#load key >>= fun w -> Some (w = data) in
    let is_diff ~key ~data = match exists_equal key data with
      | Some r -> not r
      | None -> true in
    Addr.Table.existsi t.mems ~f:is_diff

  let is_different t ctxt = has_diff_vars t ctxt || has_diff_mems t ctxt

  let exists_var t var = Var.Table.mem t.vars var
  let exists_mem t addr = Addr.Table.mem t.mems addr
end
