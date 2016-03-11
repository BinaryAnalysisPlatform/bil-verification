open Core_kernel.Std
open Bap.Std
open Bap_traces.Std
open Bap_plugins.Std

let () = Frame_trace_plugin.register ()

module Test_x86 = struct
  let arch = `x86
  let uri = Uri.of_string "file:///home/oleg/factory/ls32.frame" 
end

module Test_arm = struct
  let arch = `armv7
  let uri = Uri.of_string "file:///home/oleg/factory/ls_arm.frame" 
end

open Test_x86

let string_of_bindings binds = 
  let pp (v,r) =
    let value = Bil.Result.value r in
    let ppv = Bil.Result.Value.pp in
    Format.fprintf Format.str_formatter "%a = %a\n" Var.pp v ppv value in
  Seq.iter binds ~f:pp;
  Format.flush_str_formatter ()

let print_bindings binds = 
  Printf.printf "%s\n" (string_of_bindings binds)

let run arch trace = 
  let (module V : Verify.V) = Verify.create arch in
  let v = V.execute trace in
  let h = Stat.histo (V.stat v) in
  List.iter ~f:(fun (name, times) -> Printf.printf "%s %d\n" name times) h

let print_diverged = 
  List.iter ~f:(Diff.pp Format.std_formatter) 

let until_mismatch arch trace =
  let (module V : Verify.V) = Verify.create arch in
  let v = V.create trace in
  match V.until_mismatch v with 
  | [],_ -> Printf.printf "no result"
  | diff,_ -> print_diverged diff

let until_mismatch_n arch trace n =  
  let (module V : Verify.V) = Verify.create arch in
  let rec run last_diff cnt v =
    if cnt = n then last_diff
    else match V.until_mismatch v with 
      | diff, Some v -> run diff (cnt + 1) v
      | diff, None -> diff in
  let v = V.create trace in  
  match run [] 0 v with 
  | [] -> Printf.printf "no result"
  | diff -> 
    print_diverged diff;
    print_newline ()

let until_mismatch_n' arch trace n =  
  let (module V : Verify.V) = Verify.create arch in
  let rec run cnt v =
    if cnt = n then ()
    else match V.until_mismatch v with 
      | diff, Some v -> 
        print_diverged diff;        
        print_newline ();
        run (cnt + 1) v
      | diff, None -> print_diverged diff in
  let v = V.create trace in  
  run 0 v 

let () =
  let open Result in
  match Trace.load uri with
  | Error er -> Printf.printf "error occured\n"
  | Ok trace -> until_mismatch_n' arch trace 5
