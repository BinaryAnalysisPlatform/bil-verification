open Core_kernel.Std
open Textutils.Std 
open Text_block

module Abs = Veri_stat.Abs
module Rel = Veri_stat.Rel
module Sum = Veri_stat.Summary

let make_iota max = 
  let rec make acc n = 
    if n < 0 then acc
    else make (n :: acc) (n - 1) in
  make [] (max - 1)

let make_col title to_string vals = 
  vcat ~align:`Center (text title :: List.map ~f:(fun x -> text (to_string x)) vals)

let texts_col title vals = make_col title ident vals
let intgr_col title vals = make_col title (Printf.sprintf "%d") vals
let float_col title vals = make_col title (Printf.sprintf "%.2f") vals

let output sum path = 
  let stats = Sum.stats sum in
  let of_stats f = List.map ~f stats in
  let of_stats' f = List.map ~f:(fun x -> f (snd x)) stats in
  let out = Out_channel.create path in
  let cnter = intgr_col "#" (make_iota (List.length stats)) in
  let names = texts_col "file" (of_stats fst) in
  let total = intgr_col "total" (of_stats' Abs.total) in
  let prcnt = List.map 
      ~f:(fun (name, f) -> float_col name (of_stats' f)) 
             [ "successed, %",   Rel.successed; 
               "misexecuted, %", Rel.misexecuted;
               "overloaded, %",  Rel.overloaded;
               "damaged, %",     Rel.damaged;
               "undisasmed, %",  Rel.undisasmed;
               "mislifted, %",   Rel.mislifted; ] in
  let tab = hcat ~sep:(text "  |  ") ([cnter; names; total] @ prcnt) in
  Out_channel.output_string out (render tab);
  Out_channel.close out
