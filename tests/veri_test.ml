open Core_kernel.Std
open OUnit2
open Bap.Std
open Bap_traces.Std
open Event

module Dis = Disasm_expert.Basic

let arch = `x86
let size = 32

let assert_false s = assert_bool s false

let word_of_int x = Word.of_int ~width:size x

let test_tool = 
  Trace.register_tool (module struct
    let name = "test tool"
    let supports tag = true
    let probe uri = false
  end)

let make_move cell' data' = Move.({cell = cell'; data = data';}) 

let make_reg var_name value = 
  let var = Var.create var_name (Type.Imm size) in
  let dat = word_of_int value in
  make_move var dat

let make_mem addr value = 
  make_move (word_of_int addr) (word_of_int value)

let make_chunk addr' data' = 
  Chunk.({addr = word_of_int addr'; data = data';}) 

let make_event tag value = Value.create tag value

let make_events_stream evs = 
  let evs' = ref evs in
  let rec next () = match !evs' with
    | [] -> None
    | hd :: tl -> 
      evs' := tl;
      Some (Ok hd) in
  next

let make_trace code real_evs = 
  let next = make_events_stream (code::real_evs) in
  let trace = Trace.create test_tool next in
  Trace.set_attr trace Meta.arch arch

let is_equal_events evs evs' =
  let is_exists ev = List.exists ~f:(fun ev' -> ev = ev') evs' in
  List.length evs = List.length evs' &&
  List.for_all ~f:is_exists evs

let test_policy =
  let open Veri_policy in
  let rules = [
    Rule.create ~insn:" *" ~left:" *" Rule.deny;
  ] in 
  List.fold ~init:empty ~f:add rules

let eval_trace trace =
  Dis.with_disasm ~backend:"llvm" (Arch.to_string arch) ~f:(fun dis ->
      let dis = Dis.store_asm dis |> Dis.store_kinds in          
      let report = Veri_report.create () in
      let ctxt = new Veri.context test_policy report trace in
      let veri  = new Veri.t arch dis (fun _ -> true) in
      let ctxt' = 
        Monad.State.exec (veri#eval_trace trace) ctxt in
      Ok ctxt'#report)

let first_left_match binds = 
  let open Option in
  List.hd binds >>= fun (_, calls) ->
  List.hd calls >>= fun call ->
  List.hd call >>= fun (_, matched) ->
  match matched with
  | Veri_policy.Left left -> Some left
  | _ -> None

let check_left_diff pref trace expected =
  match eval_trace trace with 
  | Error er -> 
    assert_false (Printf.sprintf "%s: %s" pref (Error.to_string_hum er))
  | Ok report ->
    match first_left_match (Veri_report.binds report) with 
    | None ->
      assert_false (Printf.sprintf "%s: no left match" pref)
    | Some left ->
      let s = Printf.sprintf "%s: diff equality check" pref in
      assert_bool s (is_equal_events expected left)

(** MOV32rr: { EAX := low:32[ESP] } *)
let test_reg ctxt = 
  let code = make_event code_exec (make_chunk 0xF67DE0D0 "\x89\xE0") in
  let e0 = make_event register_read (make_reg "EFLAGS" 0x202) in
  let e1 = make_event register_read (make_reg "EAX" 0x0) in
  let e2 = make_event register_read (make_reg "ESP" 0xF6FFEE50) in
  let e3 = make_event register_write (make_reg "EAX" 0xF6FFEE50) in
  let e4 = make_event register_write (make_reg "EFLAGS" 0x202) in
  let e5 = make_event pc_update (word_of_int 0xF67DE0D2) in
  let real_evs = [e0;e1;e2;e3;e4;e5;] in
  let trace = make_trace code real_evs in
  let expected_diff = [e0; e1; e4; e5] in
  check_left_diff "test_reg" trace expected_diff
      
(** MOV32mr: 
    { 
       mem32 := mem32 with 
             [(pad:32[low:32[EBP]]) + 0xFFFFFFBC:32, el]:u32 <- low:32[EAX]
    } *)
let test_mem_store ctxt =
  let code = make_event code_exec (make_chunk 0xF67E17D4 "\x89\x45\xBC") in
  let e0 = make_event register_read (make_reg "EFLAGS" 0x296) in
  let e1 = make_event register_read (make_reg "EBP" 0xF6FFEE48) in
  let e2 = make_event register_read (make_reg "EAX" 0xF6FFEE50) in
  let e3 = make_event memory_store (make_mem 0xF6FFEE04 0xF6FFEE50) in
  let e4 = make_event register_write (make_reg "EFLAGS" 0x296) in
  let e5 = make_event pc_update (word_of_int 0xF67E17D7) in
  let real_events = [e0;e1;e2;e3;e4;e5;] in
  let trace = make_trace code real_events in
  let expected_diff = [e0; e4; e5] in
  check_left_diff "test_mem_store" trace expected_diff

(** MOV32rm : { EBX := mem32[low:32[ESP], el]:u32 }*)
let test_mem_load ctxt = 
  let code = make_event code_exec (make_chunk 0xF67F57A8 "\x8B\x1C\x24") in
  let e0 = make_event register_read (make_reg "EFLAGS" 0x282) in
  let e1 = make_event register_read (make_reg "ESP" 0xF6FFEDDC) in
  let e2 = make_event memory_load (make_mem 0xF6FFEDDC 0xF67E17CE) in
  let e3 = make_event register_write (make_reg "EBX" 0xF67E17CE) in
  let e4 = make_event register_write (make_reg "EFLAGS" 0x282) in
  let e5 = make_event pc_update (word_of_int 0xF67F57AB) in
  let real_evs = [e0;e1;e2;e3;e4;e5;] in
  let trace = make_trace code real_evs in
  let expected_diff = [e0; e4; e5] in  
  check_left_diff "test_mem_load" trace expected_diff

let test_backref_match ctxt = 
  let open Veri_policy in
  let rule = 
    Rule.create 
      ~insn:" *" 
      ~left:"(.*) => 0x282:32" 
      ~right:"(\\1) => (.*)" Rule.skip in
  let e0 = make_event register_read (make_reg "EFLAGS" 0x282) in
  let e0' = make_event register_read (make_reg "EFLAGS" 0x283) in
  let e1 = make_event memory_load (make_mem 0xF6FFEDDC 0xF67E17CE) in
  let e2 = make_event register_write (make_reg "EBX" 0xF67E17CE) in
  let events = Value.Set.of_list [e0; e1; e2] in
  let events' = Value.Set.of_list [e2; e1; e0'] in
  match match_events rule "insn name" events events' with
  | None -> assert_false "test back reference match empty"
  | Some matched -> 
    let expected = Both [e0, e0'; ] in
    assert_equal ~ctxt matched expected

let suite () =
  "Veri test" >:::
  [
    "reg test"        >:: test_reg;
    "mem store test"  >:: test_mem_store;
    "mem load test"   >:: test_mem_load;
    "test backref"    >:: test_backref_match;
  ]
  
