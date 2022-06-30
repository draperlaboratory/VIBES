module C = Cmdliner
module Log = Vibes_log_lib.Stream
module Versions = Vibes_constants_lib.Versions
module Cli_opts = Vibes_common_cli_options_lib

open Vibes_patcher_lib
module Cli = struct

  let name = "vibes-patch"
  let version = Versions.vibes_playground
  let doc = "Patch a binary"
  let info = C.Cmd.info name ~doc ~version

  let run
      (config_filename : string)
      (is_verbose : bool)
      (is_no_color : bool)
      : (unit, string) result =
    let () = Cli_opts.Verbosity.setup is_verbose is_no_color in
    Log.send "Running 'vibes-dummy-cli foo'";
    let config = Yojson.Safe.from_file config_filename in 
    match Patcher.Config.of_yojson config with
    | Error s -> failwith s
    | Ok config ->
    Patcher.patch config;
    Ok ()

  let runner =
    C.Term.(const run
      $ Cli_opts.Target.target
      $ Cli_opts.Verbosity.is_verbose
      $ Cli_opts.Verbosity.is_no_color)

  let () = match Bap_main.init () with
    | Ok () -> ()
    | Error e ->
      failwith (Format.asprintf "%a" Bap_main.Extension.Error.pp e)

  let cmd = C.Cmd.v info runner

end

let () = exit (C.Cmd.eval_result Cli.cmd)
