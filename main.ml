open Core_kernel.Std
open Cmdliner

module Run = struct
  open Bap.Std
  open Bap_traces.Std
  open Bap_plugins.Std

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
        let v = V.execute trace in
        let report = V.report v in
        Veri_report.Brief.pp Format.std_formatter report

end

let arch =
  let doc = "Target architecture" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"ARCH" ~doc)

let filename =
  let doc = "Input filename" in
  Arg.(required & pos 1 (some non_dir_file) None & info [] ~doc ~docv:"FILE")

let proto =
  let doc = "Protocol for output" in
  Arg.(value & opt string "sexp" & info ["p"; "proto"] ~docv:"PROTOCOL" ~doc)

let output =
  let doc = "Output filemname" in
  Arg.(value & opt (some string) None & info ["o"; "output"] ~docv:"OUTPUT" ~doc)

let run_t = Term.(const Run.run $ arch $ filename)

let info =
  let doc = "Bil verification tool" in
  let man = [
    `S "SYNOPSIS";
    `S "DESCRIPTION";
    `P "Perform compare"
] in
  Term.info "veri" ~doc ~man

let () = match Term.eval (run_t, info) with `Error _ -> exit 1 | _ -> exit 0

(** TODO: add a cmdline control here:

    usage:
    - ./veri arch filename
    - ./veri arch -p [proto] -o file.out filename
    - ./veri_debug until arch filename
    - ./veri_debug --find [insn_all] arch filename
    - ./veri_debug --find-all [insn_all] arch filename
    
*)

