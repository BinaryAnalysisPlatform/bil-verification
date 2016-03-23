open Core_kernel.Std
open Bap.Std
open Bap_traces.Std

module Dis = Disasm_expert.Basic

(** [name_of_insns insns] - returns a name of last instruction in [insns]  *)
let name_of_insns insns = 
  match List.hd (List.rev insns) with
  | None -> Or_error.error_string "instruction hasn't name"
  | Some (_, insn) -> Ok (Insn.(name (of_basic insn)))

let insns_of_mem dis mem = 
  let open Or_error in
  let rec loop insns mem =
    Dis.insn_of_mem dis mem >>= (fun (imem, insn, left) ->
        let insns' = match insn with 
          | Some insn -> (imem, insn) :: insns 
          | None -> insns in
        match left with
        | `left mem -> loop insns' mem 
        | `finished -> Ok (List.rev insns')) in 
  loop [] mem

let insns_of_chunk arch chunk =
  let open Or_error in
  let endian = Arch.endian arch in
  Dis.with_disasm ~backend:"llvm" (Arch.to_string arch)
    ~f:(fun dis ->
        let dis = Dis.store_kinds dis |> Dis.store_asm in
        let mems = Bigstring.of_string (Chunk.data chunk) in
        Memory.create endian (Chunk.addr chunk) mems >>=
        fun mem -> insns_of_mem dis mem >>=
        fun insns -> name_of_insns insns >>= 
        fun name -> return (name, insns))

let lift_insn arch (mem,insn) = 
  let module Target = (val target_of_arch arch) in
  match Target.lift mem insn with
  | Ok bil -> Some bil
  | Error _ -> None

let bil_of_chunk arch chunk = 
  let open Or_error in 
  insns_of_chunk arch chunk >>| fun (name, insns) ->
  let bil = List.filter_map ~f:(lift_insn arch) insns in
  name,List.concat bil
