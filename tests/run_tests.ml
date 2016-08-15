open Core_kernel.Std
open Bap_plugins.Std
open OUnit2

let load_plugins () =
  match Plugins.load () |> Result.all with
  | Ok plugins -> ()
  | Error (p,e)->
    assert_string ("failed to load plugin from " ^ p ^ ": " ^
                   Error.to_string_hum e)

let suite () =
  "Bap-veri" >::: [ 
    Veri_test.suite ();
    Veri_policy_test.suite ();
    Veri_rule_test.suite ();
    Veri_stat_test.suite ();
  ]

let () = 
  load_plugins ();
  run_test_tt_main (suite ())
