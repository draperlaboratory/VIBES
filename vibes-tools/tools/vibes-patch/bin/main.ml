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

  let asm_filepaths : string list C.Term.t =
    let info = C.Arg.info ["f"; "asm-filepaths"]
        ~docv:"ASM_FILEPATHS"
        ~doc:"Path to files containing serialized assembly \
              (separated by space)" in
    let parser = C.Arg.list C.Arg.file in
    let default = [] in
    let arg = C.Arg.opt parser default info in
    C.Arg.non_empty arg

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
      (target : string)
      (language : string)
      (patch_spaces : string option)
      (binary : string)
      (asm_filepaths : string list)
      (patched_binary : string) : (unit, string) result =
    let () = Cli_opts.Verbosity.setup ~verbose ~no_color in
    Log.send "Running 'vibes-as'";
    Runner.run
      ~target
      ~language
      ~patch_spaces
      ~binary
      ~asm_filepaths
      ~patched_binary
      () |> function
    | Ok () -> Ok ()
    | Error e -> Error (KB.Conflict.to_string e)

  let runner = C.Term.(
      const run
      $ Cli_opts.Verbosity.verbose
      $ Cli_opts.Verbosity.no_color
      $ Cli_opts.Target.target
      $ Cli_opts.Language.language
      $ Cli_opts.Patch_info.spaces
      $ binary
      $ asm_filepaths
      $ patched_binary
    )

  let cmd = C.Cmd.v info runner

end

let () = match Bap_main.init () with
  | Ok () -> ()
  | Error e ->
    failwith (Format.asprintf "%a" Bap_main.Extension.Error.pp e)

let () = exit (C.Cmd.eval_result Cli.cmd)
