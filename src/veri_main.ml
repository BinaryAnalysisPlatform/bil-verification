open Core_kernel.Std
open Cmdliner

module Run = struct
  open Bap.Std
  open Bap_traces.Std
  open Bap_plugins.Std

  let cut trace n = 
    let trace' = Trace.create (Trace.tool trace) in
    let trace' = Trace.set_meta trace' (Trace.meta trace) in
    let rec grab acc cnt evs = 
      if cnt > n then acc
      else match Seq.next evs with
      | None -> acc 
      | Some (ev, evs') -> grab (ev::acc) (cnt + 1) evs' in
    let events' = List.rev (grab [] 0 (Trace.events trace)) in
    let events' = Seq.of_list events' in
    Trace.append trace' events'

  let () = 
    List.iter
      ~f:(fun r -> match r with
          | Ok pl -> ()
          | Error (path, er) ->
            Printf.printf "plugin at %s load failed\n" path) (Plugins.load ())

  let string_of_error = function
    | `Protocol_error er -> 
      Printf.sprintf "protocol error: %s" 
        (Info.to_string_hum (Error.to_info er))
    | `System_error er -> 
      Printf.sprintf "system error: %s" (Unix.error_message er)
    | `No_provider -> "no provider"
    | `Ambiguous_uri -> "ambiguous uri"

  let run file =
    let uri = Uri.of_string ("file://" ^ file) in
    match Trace.load uri with
    | Error er -> 
      Printf.eprintf "error during loading trace: %s\n" (string_of_error er)
    | Ok trace ->
      match Dict.find (Trace.meta trace) Meta.arch with
      | Some arch ->
        let report = Veri_report.create () in
        let ctxt = new Veri.context report arch in
        let veri  = new Veri.t (fun _ -> true) in
        let ctxt' = Monad.State.exec (veri#eval_trace trace) ctxt in
        Veri_report.pp Format.std_formatter ctxt'#report
      | None -> Printf.eprintf "trace of unknown arch\n"

end

let filename = 
  let doc = "Input filename" in 
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~doc ~docv:"FILE") 

let info =
  let doc = "Bil verification tool" in
  let man = [] in
  Term.info "veri" ~doc ~man

let run_t = Term.(const Run.run $ filename)

let () = match Term.eval (run_t, info) with `Error _ -> exit 1 | _ -> exit 0
