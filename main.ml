open Core_kernel.Std
open Bap.Std
open Bap_traces.Std



let bk = 
  match Image.available_backends () with
  | [] -> 
    Printf.printf "no backends .. exit\n ";
    exit 1
  | _ -> ()

let () = Frame_trace_plugin.register ()
let arch = `x86_64

let until_error arch trace =
  let (module V : Verify.V) = Verify.create arch in
  let v = V.create trace in
  match V.until_mismatch v with
  | None -> Printf.printf "none was returned\n"
  | Some (right, wrong) ->
    Printf.printf "error occured\n"
    (* let right = right#bindings in *)
    (* let wrong = wrong#bindings in *)
    (* Seq.iter right ~f:(fun (v,r) -> *)
    (*   Var.pp Format.std_formatter v) *)
   
let run arch trace = 
  let (module V : Verify.V) = Verify.create arch in
  let v = V.execute trace in
  V.correct v, V.incorrect v

let step arch trace = 
  let (module V : Verify.V) = Verify.create arch in
  let v = V.create trace in
  match V.step v with
  | Some v -> Some (V.correct v)
  | None -> None

let () =
  let open Result in
  let uri = Uri.of_string "file:///home/oleg/factory/objdump32.frame" in  
  match Trace.load uri with
  | Error _ -> Printf.printf "error occures\n"
  | Ok trace -> 
    until_error arch trace
    (* let c, i = run arch trace in *)
    (* Printf.printf "result %d/%d\n" c i *)
      
                         
