open Core_kernel.Std
open OUnit2
open Bap.Std
open Bap_future.Std
open Bap_traces.Std
open Event

module Policy = Veri_policy
module Rule = Veri_rule
module Events = Value.Set

(** this implies, that right reads should be subset of left reads and
    right writes should be subset of left writes *)
let rule_with_skip = Rule.create_exn Rule.skip ~left:".F => .*"
let rule_with_deny = Rule.create_exn Rule.deny ~left:"(.F) <= .*" ~right:"\\1 <= .*"
let rule_with_skip' = Rule.create_exn Rule.skip ~left:".F <= .*"
let policy = 
  let add rule policy = Policy.add policy rule in
  add rule_with_skip Policy.empty |> add rule_with_deny |> add rule_with_skip'

let make_flag var_name flag =
  let var = Var.create var_name (Type.Imm 1) in
  let data' = if flag then Word.b1 else Word.b0 in
  Value.create register_write (Move.({cell = var; data = data';}))

let flag  = make_flag "CF" true
let flag' = make_flag "CF" false

let test_denied ctxt =
  let left  = Events.of_list [make_flag "OF" true; flag; make_flag "SF" true] in
  let right = Events.of_list [make_flag "OF" true; flag'] in
  match Policy.denied policy "some insn" left right with
  | [] -> assert_failure "match result is empty"
  | (rule, (left', right')) :: [] ->
    assert_equal ~ctxt ~cmp:Rule.equal rule rule_with_deny;
    assert_equal ~ctxt [flag] left';
    assert_equal ~ctxt [flag'] right'
  | res -> assert_failure "match result is unexpectable long"

let suite () =
  "Veri rule test" >::: [ 
    "denied"  >:: test_denied;
  ]
