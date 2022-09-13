open Bap_core_theory

module C = Cmdliner
module Log = Vibes_log.Stream
module Versions = Vibes_constants.Versions
module Cli_opts = Vibes_common_cli_options
module Runner = Vibes_patch.Runner

module Cli = struct

  let name = "vibes-patch"
  let doc = "Attemps to insert the patch assembly into the existing binary."
  let version = Versions.vibes_patch
  let info = C.Cmd.info name ~doc ~version

  let binary : string C.Term.t =
    let info = C.Arg.info ["i"; "binary"]
        ~docv:"BINARY"
        ~doc:"Path/name of binary to patch" in
    let parser = C.Arg.some' C.Arg.string in
    let default = None in
    let arg = C.Arg.opt parser default info in
    C.Arg.required arg

  let asm_filepath : string C.Term.t =
    let info = C.Arg.info ["f"; "asm-filepath"]
        ~docv:"ASM_FILEPATH"
        ~doc:"Path to file containing serialized assembly" in
    let parser = C.Arg.some' C.Arg.file in
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

  let ogre_filepath : string option C.Term.t =
    let info = C.Arg.info ["O"; "ogre"]
        ~docv:"OGRE_FILEPATH"
        ~doc:"Path/name of the optional OGRE loader file" in
    let parser = C.Arg.some' C.Arg.string in
    let default = None in
    let arg = C.Arg.opt parser default info in
    C.Arg.value arg

  let run
      (verbose : bool)
      (no_color : bool)
      (target : string)
      (language : string)
      (patch_info_filepath : string)
      (binary : string)
      (asm_filepath : string)
      (patched_binary : string)
      (ogre_filepath : string option) : (unit, string) result =
    let () = Cli_opts.Verbosity.setup ~verbose ~no_color in
    Log.send "Running 'vibes-as'";
    Runner.run
      ~target
      ~language
      ~patch_info_filepath
      ~binary
      ~asm_filepath
      ~patched_binary
    ~ogre_filepath |> function
    | Ok () -> Ok ()
    | Error e -> Error (KB.Conflict.to_string e)

  let runner = C.Term.(
      const run
      $ Cli_opts.Verbosity.verbose
      $ Cli_opts.Verbosity.no_color
      $ Cli_opts.Target.target
      $ Cli_opts.Language.language
      $ Cli_opts.Patch_info.filepath
      $ binary
      $ asm_filepath
      $ patched_binary
      $ ogre_filepath
    )

  let cmd = C.Cmd.v info runner

end

let () = match Bap_main.init () with
  | Ok () -> ()
  | Error e ->
    failwith (Format.asprintf "%a" Bap_main.Extension.Error.pp e)

let () = exit (C.Cmd.eval_result Cli.cmd)
