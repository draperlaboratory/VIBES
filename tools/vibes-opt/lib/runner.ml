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
module Log = Vibes_log.Stream
module Utils = Vibes_utils
module Serializers = Vibes_serializers
module Patch_info = Vibes_patch_info.Types
module Spaces = Patch_info.Spaces
module Bir_helpers = Vibes_bir.Helpers

open KB.Syntax

let deserialize_and_opt
    (sexp : Sexp.t)
    ~(target : T.target)
    ~(language : T.language)
    ~(patch_info : Patch_info.t) : sub term KB.t =
  let* sub = Serializers.Bir.deserialize sexp in
  Log.send "Deserialized BIR:\n\n%a" Sub.pp sub;
  Bir_passes.run sub ~target ~language ~patch_info

(* Try to deserialize and optimize the BIR program while
   preserving the toplevel state. *)
let try_deserialize_and_opt
    (bir_sexp : Sexp.t)
    ~(target : T.target)
    ~(language : T.language)
    ~(patch_info : Patch_info.t) : (sub term, KB.conflict) result =
  try
    let result = Toplevel.var "vibes-opt" in
    Toplevel.put result @@ deserialize_and_opt bir_sexp
      ~target ~language ~patch_info;
    Result.return @@ Toplevel.get result
  with Toplevel.Conflict err -> Error err

let run
    ~(target : string)
    ~(language : string)
    ~(patch_info_filepath : string)
    ~(bir_filepath : string)
    ~(bir_outfile : string)
    () : (unit, KB.conflict) result =
  let (let*) x f = Result.bind x ~f in
  Log.send "Vibes_opt.Runner.run '%s' '%s' '%s' '%s' '%s'"
    target language patch_info_filepath
    bir_filepath bir_outfile;
  let* patch_info = Patch_info.from_file patch_info_filepath in
  let* target = Utils.Core_theory.get_target target in
  let* language = Utils.Core_theory.get_language language in
  let* raw_bir_code = Utils.Files.get_file_contents_non_empty bir_filepath
      ~error:(fun s -> Errors.No_bir (
          Format.sprintf "No serialized BIR in file '%s'" s)) in
  let* bir_sexps = Utils.Sexp.to_sexp raw_bir_code
      ~error:(fun s -> Errors.Invalid_bir (
          Format.sprintf "Invalid BIR S-exp: %s" s)) in
  let* bir_sexp = match bir_sexps with
    | [sexp] -> Ok sexp
    | _ -> Error (Errors.Invalid_bir "Expected single S-exp") in
  let* sub = try_deserialize_and_opt bir_sexp
      ~target ~language ~patch_info in
  Log.send "Serializing BIR";
  let bir_sexp = Serializers.Bir.serialize sub in
  let bir_data = Sexp.to_string_hum bir_sexp in
  let finalized_bir = Format.sprintf "%s\n" bir_data in
  let* () = Utils.Files.write_or_error finalized_bir bir_outfile in
  Ok ()
