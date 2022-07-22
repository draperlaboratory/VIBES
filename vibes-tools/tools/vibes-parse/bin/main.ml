module C = Cmdliner
module Log = Vibes_log_lib.Stream
module Err = Vibes_error_lib.Std
module Versions = Vibes_constants_lib.Versions
module Cli_opts = Vibes_common_cli_options_lib
module Runner = Vibes_parse_lib.Runner

module Cli = struct

  let name = "vibes-parse"
  let doc = "Parse C-like patch code into BAP Core Theory code."
  let version = Versions.vibes_parse
  let info = C.Cmd.info name ~doc ~version

  let patch_filepath : string C.Term.t =
    let info = C.Arg.info ["f"; "patch-filepath"]
      ~docv:"PATCH_FILEPATH"
      ~doc:"Path to file containing patch code (in a subset of C)"
    in
    let parser = C.Arg.some' C.Arg.file in
    let default = None in
    let arg = C.Arg.opt parser default info in
    C.Arg.required arg

  let bir_outfile : string C.Term.t =
    let info = C.Arg.info ["o"; "bir-outfile"]
      ~docv:"BIR-OUTFILE"
      ~doc:"Path/name of file to output BIR to"
    in
    let parser = C.Arg.some' C.Arg.string in
    let default = None in
    let arg = C.Arg.opt parser default info in
    C.Arg.required arg

  let func_info_outfile : string C.Term.t =
    let info = C.Arg.info ["i"; "func-info-outfile"]
      ~docv:"FUNC_INFO_OUTFILE"
      ~doc:"Path/name of file to output function info to"
    in
    let parser = C.Arg.some' C.Arg.string in
    let default = None in
    let arg = C.Arg.opt parser default info in
    C.Arg.required arg

  let run
      (is_verbose : bool)
      (is_no_color : bool)
      (target : string)
      (patch_info_filepath : string)
      (patch_filepath : string)
      (bir_outfile : string)
      (func_info_outfile : string)
      : (unit, string) result =
    let () = Cli_opts.Verbosity.setup is_verbose is_no_color in
    Log.send "Running 'vibes-parse'";
    let result =
      Runner.run
        target
        patch_info_filepath
        patch_filepath
        bir_outfile
        func_info_outfile
    in
    match result with
    | Ok () -> Ok ()
    | Error e -> Error (Err.to_string e)

  let runner = C.Term.(const run
    $ Cli_opts.Verbosity.is_verbose
    $ Cli_opts.Verbosity.is_no_color
    $ Cli_opts.Target.target
    $ Cli_opts.Patch_info.filepath
    $ patch_filepath
    $ bir_outfile
    $ func_info_outfile)

  let cmd = C.Cmd.v info runner

end

let () = match Bap_main.init () with
  | Ok () -> ()
  | Error e ->
  failwith (Format.asprintf "%a" Bap_main.Extension.Error.pp e)

let () = exit (C.Cmd.eval_result Cli.cmd)
