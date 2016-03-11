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
  let h = V.histo v in
  List.iter ~f:(fun (name, times) -> Printf.printf "%s %d\n" name times) h

let step arch trace = 
  let (module V : Verify.V) = Verify.create arch in
  let v = V.create trace in
  match V.step v with
  | Some v -> Some (V.right v)
  | None -> None

let print_diverged = 
  List.iter ~f:(Diff.pp Format.std_formatter) 

let until_mismatch arch trace =
  let (module V : Verify.V) = Verify.create arch in
  let v = V.create trace in
  match V.until_mismatch v with 
  | None -> Printf.printf "no result"
  | Some v -> print_diverged (V.diff v)

let until_mismatch_n arch trace n =  
  let (module V : Verify.V) = Verify.create arch in
  let rec run cnt v =
    if cnt = n then Some v
    else match V.until_mismatch v with 
      | Some v -> run (cnt + 1) v
      | None -> None in
  let v = V.create trace in  
  match run 0 v with 
  | None -> Printf.printf "no result"
  | Some v -> 
    print_diverged (V.diff v);
    print_newline ()

let () =
  let open Result in
  match Trace.load uri with
  | Error er -> Printf.printf "error occured\n"
  | Ok trace -> run arch trace


