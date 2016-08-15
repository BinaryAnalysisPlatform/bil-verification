open Core_kernel.Std
open OUnit2

module Rule = Veri_rule

let assert_ok descr r = assert_bool descr (Result.is_ok r)
let assert_er descr r = assert_bool descr (Result.is_error r)

let test_create ctxt = 
  let insn = "MOV32rr" in
  assert_ok "abs empty" (Rule.create Rule.skip);
  assert_ok "only insn" (Rule.create ~insn Rule.skip);
  assert_ok "only left part" (Rule.create ~left:insn Rule.skip);
  assert_ok "only right part" (Rule.create ~right:insn Rule.skip);
  assert_ok "left and right" (Rule.create ~left:insn ~right:insn Rule.skip);
  assert_ok "regexp on right" (Rule.create ~insn:".*" ~left:insn ~right:".F => .*" Rule.skip);
  assert_ok "regexp in insn" (Rule.create ~insn:".*" ~left:".F => .*" Rule.skip);
  assert_ok "backref regexp" (Rule.create ~insn:".*" ~left:"(.F) <= .*" ~right:"\\1 <= .*" Rule.skip);
  assert_er "error in regexp" (Rule.create ~insn:".*" ~right:"\\1 <= .*" Rule.skip)

let test_create_exn ctxt = 
  let f () = Rule.create_exn ~insn:".*" ~right:"\\1 <= .*" Rule.skip in
  assert_raises ~msg:"Bad field not raised!" 
    (Rule.Bad_field "error in field  \\1 <= .*") f

let test_of_string ctxt = 
  assert_ok "" (Rule.of_string_err "SKIP .* '.F => .*' ''");
  assert_ok "" (Rule.of_string_err "DENY .* '(.F) <= .*' '\\1 <= .*'");
  assert_ok "" (Rule.of_string_err "SKIP .* '.F <= .*' ''");
  assert_er "" (Rule.of_string_err "SOME_ACTION .* '.F <= .*' ''")

let test_match ctxt = 
  let base_assert what descr rule field str = 
    let res = Rule.match_field rule field str in
    match what with
    | `Expect_ok -> assert_bool descr res
    | `Expect_er -> assert_bool descr (not res) in
  let assert_match = base_assert `Expect_ok in
  let assert_mismatch = base_assert `Expect_er in
  assert_match "empty rule" (Rule.create_exn Rule.skip) `Insn  "MOV32rr";
  assert_mismatch "other insn" (Rule.create_exn Rule.skip ~insn:"MOV64rr") `Insn "MOV32rr";
  assert_match "any insn" (Rule.create_exn Rule.skip ~insn:".*") `Insn "MOV32rr";
  assert_match "any left" (Rule.create_exn Rule.skip ~left:".*") `Left "ESP <= 0xC0FFEE";
  assert_match "any right" (Rule.create_exn Rule.skip ~right:".*") `Right "ESP <= 0xC0FFEE";
  assert_match "empty field" (Rule.create_exn Rule.skip ~right:".*") `Left "ESP <= 0xC0FFEE";
  assert_match "backref match" 
    (Rule.create_exn Rule.skip ~left:"(.SP) <= 0xC0FFEE" ~right:"\\1 <= .*")
    `Both "ESP <= 0xC0FFEE ESP <= 0xBED";
  assert_mismatch "backref mismatch" 
    (Rule.create_exn Rule.skip ~left:"(.F) <= (.*)" ~right:"\\1 <= \\2")
    `Both "OF <= 0x1 OF <= 0x0"

let suite () =
  "Veri rule test" >:::
  [
    "create"     >:: test_create;
    "create exn" >:: test_create_exn;
    "of_string"  >:: test_of_string;
    "match"      >:: test_match;
  ]
