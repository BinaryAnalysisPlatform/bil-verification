open Core_kernel.Std
open OUnit2
open Veri_error

module Stat = Veri_stat
module Abs = Stat.Abs
module Rel = Stat.Rel
module Names = Stat.Names

let repeat f n stat = 
  let rec loop i stat = 
    if i = n then stat 
    else loop (i + 1) (f stat) in
  loop 0 stat

let repeat' f data stat = 
  let rec loop stat = function
    | [] -> stat
    | hd :: tl -> loop (f stat hd) tl in
  loop stat data

let chunk_error = Error.of_string "spoiled chunk"
let repeat_error n er stat = repeat (fun stat' -> Stat.notify stat' er) n stat
let repeat_overloaded n stat = repeat_error n `Overloaded_chunk stat
let repeat_damaged n stat = repeat_error n (`Damaged_chunk chunk_error) stat
let repeat_undisasmed n stat = repeat_error n (`Disasm_error chunk_error) stat

let add_unlifted =
  repeat' (fun stat' name -> Stat.notify stat' (`Lifter_error (name, chunk_error)))

let add_failed = repeat' (fun stat' name -> Stat.failbil stat' name)
let add_successed = repeat' (fun stat' name -> Stat.success stat' name) 
let mislifted_names = ["CDQ"; "CQO"; "LD_F80m"; "LD_Frr"; "ST_FP80m"]
let successed_names = ["CMOVA64rr"; "CMOVAE32rr";]
let abs_successed_names = ["ADD64rr"; "ADD64mr"; "ADD32rr"; "ADD32mr"; ]
let abs_misexec_names = ["NOOPL"; "NOOPW"; "CMOVAE64rr";]

(** since this both sets describes insns that were sometimes successed and
    sometimes not *)
let misexec_names = successed_names 

let is_same xs xs' = 
  List.length xs = List.length xs' &&
  List.for_all ~f:(fun x' -> List.exists ~f:(fun x -> x = x') xs) xs'

let overloaded_cnt = 8
let damaged_cnt = 16
let undisasmed_cnt = 32

let stat =
  Stat.empty |>
  repeat_overloaded overloaded_cnt |>
  repeat_damaged damaged_cnt       |>
  repeat_undisasmed undisasmed_cnt |>
  add_unlifted mislifted_names  |>
  add_failed (misexec_names @ abs_misexec_names) |>
  add_successed (successed_names @ abs_successed_names)

let test_abs ctxt =
  let abs_successed_cnt = List.length abs_successed_names in
  let successed_cnt = List.length (successed_names @ abs_successed_names) in
  let abs_misexec_cnt = List.length abs_misexec_names in
  let misexec_cnt = List.length (misexec_names @ abs_misexec_names) in
  let mislifted_cnt = List.length mislifted_names in
  let total = overloaded_cnt + damaged_cnt + undisasmed_cnt + 
              successed_cnt + misexec_cnt + mislifted_cnt in
  assert_equal ~ctxt (Abs.successed stat) successed_cnt;
  assert_equal ~ctxt (Abs.abs_successed stat) abs_successed_cnt;
  assert_equal ~ctxt (Abs.misexecuted stat) misexec_cnt;
  assert_equal ~ctxt (Abs.abs_misexecuted stat) abs_misexec_cnt;
  assert_equal ~ctxt (Abs.overloaded stat) overloaded_cnt;
  assert_equal ~ctxt (Abs.damaged stat) damaged_cnt;
  assert_equal ~ctxt (Abs.undisasmed stat) undisasmed_cnt;
  assert_equal ~ctxt (Abs.mislifted stat) mislifted_cnt;
  assert_equal ~ctxt (Abs.total stat) total

let test_rel ctxt = 
  let assert_float descr x y = assert_bool descr (cmp_float x y) in
  let to_float n = float n /. float (Abs.total stat) in
  let to_float' xs = to_float (List.length xs) in
  assert_float "rel successed" 
    (Rel.successed stat) (to_float' (abs_successed_names @ successed_names));
  assert_float "rel misexecuted" 
    (Rel.misexecuted stat) (to_float' (abs_misexec_names @ misexec_names));
  assert_float "rel abs successed" 
    (Rel.abs_successed stat) (to_float' abs_successed_names);
  assert_float "rel abs misexecuted" 
    (Rel.abs_misexecuted stat) (to_float' abs_misexec_names);
  assert_float "rel overloaded" (Rel.overloaded stat) (to_float overloaded_cnt);
  assert_float "rel damaged" (Rel.damaged stat) (to_float damaged_cnt);
  assert_float "rel undisasmed" (Rel.undisasmed stat) (to_float undisasmed_cnt);
  assert_float "rel mislifted" (Rel.mislifted stat) (to_float' mislifted_names)

let test_names ctxt =
  let successed = successed_names @ abs_successed_names in
  let misexecuted = misexec_names @ abs_misexec_names in
  assert_bool "successed names" (is_same (Names.successed stat) successed);
  assert_bool "abs successed names" 
    (is_same (Names.abs_successed stat) abs_successed_names);
  assert_bool "misexecuted names" 
    (is_same (Names.misexecuted stat) misexecuted);
  assert_bool "abs misexecuted names" 
    (is_same (Names.abs_misexecuted stat) abs_misexec_names);
  assert_bool "mislifted names" (is_same (Names.mislifted stat) mislifted_names)

let suite () =
  "Veri stat test" >:::
  [
    "absolute"    >:: test_abs;
    "relative"    >:: test_rel;
    "names"  >:: test_names;
  ]
