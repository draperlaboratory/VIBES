open Bap_core_theory

module C = Cmdliner
module Log = Vibes_log.Stream
module Versions = Vibes_constants.Versions
module Cli_opts = Vibes_common_cli_options
module Runner = Vibes_as.Runner

module Cli = struct

  let name = "vibes-as"
  let doc = "Runs register allocation and scheduling on VIBES IR."
  let version = Versions.vibes_as
  let info = C.Cmd.info name ~doc ~version

  let vir_filepath : string C.Term.t =
    let info = C.Arg.info ["f"; "vir-filepath"]
        ~docv:"VIR_FILEPATH"
        ~doc:"Path to file containing VIBES IR" in
    let parser = C.Arg.some' C.Arg.file in
    let default = None in
    let arg = C.Arg.opt parser default info in
    C.Arg.required arg

  let vir_outfile : string C.Term.t =
    let info = C.Arg.info ["o"; "vir-outfile"]
        ~docv:"VIR_OUTFILE"
        ~doc:"Path/name of file to output VIBES IR to" in
    let parser = C.Arg.some' C.Arg.string in
    let default = None in
    let arg = C.Arg.opt parser default info in
    C.Arg.required arg

  let model_filepath : string C.Term.t =
    let info = C.Arg.info ["m"; "model"]
        ~docv:"MZN_MODEL"
        ~doc:"Path/name of file containing the MiniZinc model" in
    let parser = C.Arg.some' C.Arg.string in
    let default = None in
    let arg = C.Arg.opt parser default info in
    C.Arg.required arg

  let run
      (verbose : bool)
      (no_color : bool)
      (target : string)
      (language : string)
      (vir_filepath : string)
      (vir_outfile : string)
      (model_filepath : string) : (unit, string) result =
    let () = Cli_opts.Verbosity.setup ~verbose ~no_color in
    Log.send "Running 'vibes-as'";
    Runner.run
      ~target
      ~language
      ~vir_filepath
      ~vir_outfile
      ~model_filepath |> function
    | Ok () -> Ok ()
    | Error e -> Error (KB.Conflict.to_string e)

  let runner = C.Term.(
      const run
      $ Cli_opts.Verbosity.verbose
      $ Cli_opts.Verbosity.no_color
      $ Cli_opts.Target.target
      $ Cli_opts.Language.language
      $ vir_filepath
      $ vir_outfile
      $ model_filepath
    )

  let cmd = C.Cmd.v info runner

end

let () = match Bap_main.init () with
  | Ok () -> ()
  | Error e ->
    failwith (Format.asprintf "%a" Bap_main.Extension.Error.pp e)

let () = exit (C.Cmd.eval_result Cli.cmd)
