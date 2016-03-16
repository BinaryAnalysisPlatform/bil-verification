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
        Veri_report.pp Format.std_formatter (V.execute trace)

end

let () = Run.run "armv5" "/home/oleg/factory/ls_arm.frame"
