open Core_kernel.Std
open Cmdliner
open Bap.Std
open Bap_traces.Std
open Bap_plugins.Std

let () = Frame_trace_plugin.register ()

module Test_x86 = struct
  let arch = `x86
  let uri = Uri.of_string "file:///home/oleg/factory/ls32.frame" 
end

module Test_arm = struct
  let arch = `armv5
  let uri = Uri.of_string "file:///home/oleg/factory/ls_arm.frame" 
end

open Test_arm

let print_diverged difs = 
  Format.fprintf Format.std_formatter "diffs: \n";
  List.iter ~f:(Veri_report.Diff.pp Format.std_formatter) difs;
  Format.fprintf Format.std_formatter "---- \n"

let diff_of_records =
  List.fold ~init:[] ~f:(fun acc (_,r) ->
      acc @ Veri_report.Record.(diff r))

let until_mismatch arch trace =
  let (module V : Verify.D) = Verify.create_debug arch in
  let v = V.create trace in
  match V.until_mismatch v with 
  | Some v -> 
    let recs = Veri_report.Debug.records (V.report v) in
    let diff = diff_of_records recs in
    print_diverged diff
  | None -> ()

let until_mismatch_n arch trace n =  
  let (module V : Verify.D) = Verify.create_debug arch in
  let rec run last_diff cnt v =
    if cnt = n then last_diff
    else match V.until_mismatch v with 
      | Some v -> 
        let recs = Veri_report.Debug.records (V.report v) in
        let diff = diff_of_records (recs) in
        run diff (cnt + 1) v
      | None -> [] in
  let v = V.create trace in  
  match run [] 0 v with 
  | [] -> Printf.printf "no result"
  | diff -> 
    print_diverged diff;
    print_newline ()

let until_mismatch_n' arch trace n =  
  let (module V : Verify.D) = Verify.create_debug arch in
  let rec run cnt v =
    if cnt = n then ()
    else match V.until_mismatch v with 
      | Some v -> 
        let recs = Veri_report.Debug.records (V.report v) in
        let diff = diff_of_records (recs) in
        print_diverged diff;        
        print_newline ();
        run (cnt + 1) v
      | None -> Printf.printf "no result\n" in
  let v = V.create trace in  
  run 0 v 

let run arch trace = 
  let (module V : Verify.V) = Verify.create arch in
  let v = V.execute trace in
  let report = V.report v in
  Veri_report.Light.pp Format.std_formatter report

let () =
  let open Result in
  match Trace.load uri with
  | Error er -> Printf.printf "error occured\n"
  | Ok trace -> 
    (* until_mismatch_n' arch trace 1 *)
    run arch trace

(** TODO: add a cmdline control here:
    - add find - to find first entry of corrupted instruction
    - add mode - either to show statistic either to show first mismatch 
    - output (sexp &&/|| bin_io) *)

