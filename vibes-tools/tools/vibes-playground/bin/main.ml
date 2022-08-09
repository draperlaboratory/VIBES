module C = Cmdliner
module Log = Vibes_log.Stream
module Versions = Vibes_constants.Versions
module Cli_opts = Vibes_common_cli_options

module Cli = struct

  let name = "vibes-playground"
  let version = Versions.vibes_playground
  let doc = "A dummy command to try stuff out (a playground)."
  let info = C.Cmd.info name ~doc ~version

  let run
      (verbose : bool)
      (no_color : bool)
      : (unit, string) result =
    let () = Cli_opts.Verbosity.setup ~verbose ~no_color in
    Log.send "Running 'vibes-dummy-cli foo'";
    (* START PLAYGROUND (do what you want here) *)

    print_endline "This is a playground.";

    (* END PLAYGROUND *)
    Ok ()

  let runner =
    C.Term.(const run
      $ Cli_opts.Verbosity.verbose
      $ Cli_opts.Verbosity.no_color)

  let () = match Bap_main.init () with
    | Ok () -> ()
    | Error e ->
      failwith (Format.asprintf "%a" Bap_main.Extension.Error.pp e)

  let cmd = C.Cmd.v info runner

end

let () = exit (C.Cmd.eval_result Cli.cmd)
