open Bap_core_theory

module C = Cmdliner
module Log = Vibes_log_lib.Stream
module Versions = Vibes_constants_lib.Versions
module Cli_opts = Vibes_common_cli_options_lib

module Cli = struct

  let name = "vibes-opt"
  let doc = "Take BAP Core Theory code and produce optimized BIR."
  let version = Versions.vibes_opt
  let info = C.Cmd.info name ~doc ~version

  let bir_filepath : string C.Term.t =
    let info = C.Arg.info ["f"; "bir-filepath"]
      ~docv:"BIR_FILEPATH"
      ~doc:"Path to file containing BIR"
    in
    let parser = C.Arg.some' C.Arg.file in
    let default = None in
    let arg = C.Arg.opt parser default info in
    C.Arg.required arg

  let func_info_filepath : string C.Term.t =
    let info = C.Arg.info ["i"; "function-info-filepath"]
      ~docv:"FUNCTION_INFO_FILEPATH"
      ~doc:"Path to file containing patch function info"
    in
    let parser = C.Arg.some' C.Arg.string in
    let default = None in
    let arg = C.Arg.opt parser default info in
    C.Arg.required arg

  let bir_outfile : string C.Term.t =
    let info = C.Arg.info ["o"; "bir-outfile"]
      ~docv:"BIR_OUTFILE"
      ~doc:"Path/name of output file"
    in
    let parser = C.Arg.some' C.Arg.string in
    let default = None in
    let arg = C.Arg.opt parser default info in
    C.Arg.required arg

  let run
      (verbose : bool)
      (no_color : bool)
      (target : string)
      (language : string)
      (patch_info_filepath : string)
      (bir_filepath : string)
      (func_info_filepath : string)
      (bir_outfile : string)
      : (unit, string) result =
    let () = Cli_opts.Verbosity.setup ~verbose ~no_color in
    Log.send "Running 'vibes-opt'";
    let result = 
      Vibes_opt_lib.Runner.run
        target
        language
        patch_info_filepath
        bir_filepath
        func_info_filepath
        bir_outfile
    in
    match result with
    | Ok () -> Ok ()
    | Error e -> Error (KB.Conflict.to_string e)

  let runner = C.Term.(const run
    $ Cli_opts.Verbosity.verbose
    $ Cli_opts.Verbosity.no_color
    $ Cli_opts.Target.target
    $ Cli_opts.Language.language
    $ Cli_opts.Patch_info.filepath
    $ bir_filepath
    $ func_info_filepath
    $ bir_outfile)

  let cmd = C.Cmd.v info runner

end

let () = match Bap_main.init () with
  | Ok () -> ()
  | Error e ->
  failwith (Format.asprintf "%a" Bap_main.Extension.Error.pp e)

let () = exit (C.Cmd.eval_result Cli.cmd)
