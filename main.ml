open Core_kernel.Std
open Cmdliner

module Run = struct
  open Bap.Std
  open Bap_traces.Std
  open Bap_plugins.Std
  open Veri_types

  let () = Frame_trace_plugin.register ()

  let run arch file  =
    match Arch.of_string arch with
    | None -> Printf.eprintf "unsupported arch %s\n" arch
    | Some arch ->
      let uri = Uri.of_string ("file://" ^ file) in
      match Trace.load uri with
      | Error er -> Printf.printf "error during load trace\n"
      | Ok trace ->
        let (module V : Verify.V) = Verify.create arch in
        let v = V.create trace in
        V.iter v ~f:(fun r -> 
            Format.fprintf Format.std_formatter "%s:\n%a\n"
              (Record.name r) Diffs.pp (Record.diff r))
        
        (* match V.count trace "SUBri" with *)
        (* | None -> Printf.printf "none\n" *)
        (* | Some cnt -> Printf.printf "count: %d \n" cnt *)

        (* Veri_report.pp Format.std_formatter (V.execute trace) *)

end

let () = Run.run "armv5" "/home/oleg/factory/ls_arm.frame"

(* let arch = *)
(*   let doc = "Target architecture" in *)
(*   Arg.(required & pos 0 (some string) None & info [] ~docv:"ARCH" ~doc) *)

(* let filename = *)
(*   let doc = "Input filename" in *)
(*   Arg.(required & pos 1 (some non_dir_file) None & info [] ~doc ~docv:"FILE") *)

(* let proto = *)
(*   let doc = "Protocol for output" in *)
(*   Arg.(value & opt string "sexp" & info ["p"; "proto"] ~docv:"PROTOCOL" ~doc) *)

(* let output = *)
(*   let doc = "Output filemname" in *)
(*   Arg.(value & opt (some string) None & info ["o"; "output"] ~docv:"OUTPUT" ~doc) *)

(* let run_t = Term.(const Run.run $ arch $ filename) *)

(* let info = *)
(*   let doc = "Bil verification tool" in *)
(*   let man = [ *)
(*     `S "SYNOPSIS"; *)
(*     `S "DESCRIPTION"; *)
(*     `P "Perform compare" *)
(* ] in *)
(*   Term.info "veri" ~doc ~man *)

(* let () = match Term.eval (run_t, info) with `Error _ -> exit 1 | _ -> exit 0 *)

(* (\** TODO: add a cmdline control here: *)

(*     usage: *)
(*     - ./veri arch filename *)
(*     - ./veri arch filename -c [insn_name] *)
(*     - ./veri arch -p [proto] -o file.out filename *)
(*     - ./veri_debug until arch filename *)
(*     - ./veri_debug --find [insn_all] arch filename *)
(*     - ./veri_debug --find-all [insn_all] arch filename *)
    
(* *\) *)

