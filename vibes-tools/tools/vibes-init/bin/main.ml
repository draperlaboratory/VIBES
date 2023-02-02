open Bap_core_theory

module C = Cmdliner
module Log = Vibes_log.Stream
module Versions = Vibes_constants.Versions
module Inputs = Vibes_constants.Inputs
module Cli_opts = Vibes_common_cli_options
module Runner = Vibes_init.Runner

module Cli = struct

  let name = "vibes-init"
  let doc = "Initializes a patch project for VIBES."
  let version = Versions.vibes_init
  let info = C.Cmd.info name ~doc ~version

  let patch_names : string list C.Term.t =
    let info = C.Arg.info ["p"; "patch-names"]
        ~docv:"PATCH_NAMES"
        ~doc:"Path" in
    let parser = C.Arg.list C.Arg.string in
    let default = [] in
    let arg = C.Arg.opt parser default info in
    C.Arg.non_empty arg

  let model_filepath : string C.Term.t =
    let info = C.Arg.info ["m"; "model"]
        ~docv:"MZN_MODEL"
        ~doc:"Path/name of file containing the MiniZinc model" in
    let parser = C.Arg.file in
    let default = Inputs.default_model in
    let arg = C.Arg.opt parser default info in
    C.Arg.value arg

  let binary : string C.Term.t =
    let info = C.Arg.info ["i"; "binary"]
        ~docv:"BINARY"
        ~doc:"Path/name of binary to patch" in
    let parser = C.Arg.some' C.Arg.string in
    let default = None in
    let arg = C.Arg.opt parser default info in
    C.Arg.required arg

  let patched_binary : string C.Term.t =
    let info = C.Arg.info ["o"; "patched-binary"]
        ~docv:"PATCHED_BINARY"
        ~doc:"Path/name of file to output the patched binary" in
    let parser = C.Arg.some' C.Arg.string in
    let default = None in
    let arg = C.Arg.opt parser default info in
    C.Arg.required arg

  let run
      (verbose : bool)
      (no_color : bool)
      (language : string option)
      (ogre : string option)
      (model_filepath : string)
      (patch_names : string list)
      (binary : string)
      (patched_binary : string) : (unit, string) result =
    let () = Cli_opts.Verbosity.setup ~verbose ~no_color in
    Log.send "Running 'vibes-init'";
    Runner.run
      ~language
      ~patch_names
      ~model_filepath
      ~binary
      ~patched_binary
      ~ogre
      () |> function
    | Ok () -> Ok ()
    | Error e -> Error (KB.Conflict.to_string e)

  let runner = C.Term.(
      const run
      $ Cli_opts.Verbosity.verbose
      $ Cli_opts.Verbosity.no_color
      $ Cli_opts.Language.language_optional
      $ Cli_opts.Ogre.ogre
      $ model_filepath
      $ patch_names
      $ binary
      $ patched_binary
    )

  let cmd = C.Cmd.v info runner

end

let () = match Bap_main.init () with
  | Ok () -> ()
  | Error e ->
    failwith (Format.asprintf "%a" Bap_main.Extension.Error.pp e)

let () = exit (C.Cmd.eval_result Cli.cmd)
