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

open Core
open Bap.Std
open Bap_core_theory

module T = Theory
module CT = Vibes_utils.Core_theory
module Sexps = Vibes_utils.Sexp
module Files = Vibes_utils.Files
module Log = Vibes_log.Stream
module Inputs = Vibes_constants.Inputs

let (let*) x f = Result.bind x ~f

let run
    ?(ogre : string option = None)
    ?(language : string option = None)
    ~(patch_names : string list)
    ~(model_filepath : string)
    ~(binary : string)
    ~(patched_binary : string)
    () : (unit, KB.conflict) result =
  Log.send "Vibes_init.Runner.run '%s' '%s' '%s' '%s'"
    (String.concat patch_names ~sep:", ") model_filepath binary
    patched_binary;
  let* language = match language with
    | Some language -> Result.(CT.get_language language >>| Option.some)
    | None -> Ok None in
  let* t = Types.create ()
      ~language ~patch_names ~ogre ~binary ~patched_binary
      ~model:model_filepath ~spaces:Inputs.default_patch_spaces in
  Log.send "Generating template files";
  let* () = Types.generate_files t in
  Log.send "Generating Makefile";
  let data = Format.asprintf "%a" Types.pp_makefile t in
  let* () = Files.write_or_error data "Makefile" in
  Ok ()
