(***************************************************************************)
(*                                                                         *)
(*  Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.      *)
(*                                                                         *)
(*  This file is provided under the license found in the LICENSE file in   *)
(*  the top-level directory of this project.                               *)
(*                                                                         *)
(*  This research was developed with funding from the Defense Advanced     *)
(*  Research Projects Agency (DARPA).                                      *)
(*                                                                         *)
(***************************************************************************)

open Bap_core_theory

module C = Cmdliner
module Log = Vibes_log.Stream
module Versions = Vibes_constants.Versions
module Inputs = Vibes_constants.Inputs
module Cli_opts = Vibes_common_cli_options
module Runner = Vibes_as.Runner

module Cli = struct

  let name = "vibes-as"
  let doc = "Runs register allocation and scheduling on VIBES IR, \
             and outputs a serialized assembly program."
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

  let asm_outfile : string C.Term.t =
    let info = C.Arg.info ["o"; "asm-outfile"]
        ~docv:"ASM_OUTFILE"
        ~doc:"Path/name of file to output the serialized assembly" in
    let parser = C.Arg.some' C.Arg.string in
    let default = None in
    let arg = C.Arg.opt parser default info in
    C.Arg.required arg

  let model_filepath : string C.Term.t =
    let info = C.Arg.info ["m"; "model"]
        ~docv:"MZN_MODEL"
        ~doc:"Path/name of file containing the MiniZinc model" in
    let parser = C.Arg.file in
    let default = Inputs.default_model in
    let arg = C.Arg.opt parser default info in
    C.Arg.value arg

  let extra_constraints_filepath : string option C.Term.t =
    let info = C.Arg.info ["extra-constraints"]
        ~docv:"EXTRA_CONSTRAINTS"
        ~doc:"Optional path/name of file containing extra MiniZinc \
              constraints" in
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
      (vir_filepath : string)
      (asm_outfile : string)
      (model_filepath : string)
      (extra_constraints_filepath : string option) : (unit, string) result =
    let () = Cli_opts.Verbosity.setup ~verbose ~no_color in
    Log.send "Running 'vibes-as'";
    Runner.run
      ~target
      ~language
      ~vir_filepath
      ~patch_info_filepath
      ~asm_outfile
      ~model_filepath
      ~extra_constraints_filepath 
      () |> function
    | Ok () -> Ok ()
    | Error e -> Error (KB.Conflict.to_string e)

  let runner = C.Term.(
      const run
      $ Cli_opts.Verbosity.verbose
      $ Cli_opts.Verbosity.no_color
      $ Cli_opts.Target.target
      $ Cli_opts.Language.language
      $ Cli_opts.Patch_info.filepath
      $ vir_filepath
      $ asm_outfile
      $ model_filepath
      $ extra_constraints_filepath
    )

  let cmd = C.Cmd.v info runner

end

let () = match Bap_main.init () with
  | Ok () -> ()
  | Error e ->
    failwith (Format.asprintf "%a" Bap_main.Extension.Error.pp e)

let () = exit (C.Cmd.eval_result Cli.cmd)
