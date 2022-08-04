open Core
open Bap.Std
open Bap_core_theory

module T = Theory
module Log = Vibes_log_lib.Stream
module Utils = Vibes_utils_lib
module Serializers = Vibes_serializers_lib
module Patch_info = Vibes_patch_info_lib.Types
module Function_info = Vibes_function_info_lib.Types
module Bir_helpers = Vibes_bir_lib.Helpers

open KB.Syntax

let liftr (r : ('a, KB.Conflict.t) result) : 'a KB.t = match r with
  | Error err -> KB.fail err
  | Ok r -> !!r

let deserialize_and_opt
    (name : string)
    (sexps : Sexp.t list)
    ~(target : T.target)
    ~(language : T.language)
    ~(patch_info : Patch_info.t)
    ~(func_info : Function_info.t) : Types.t KB.t =
  let* blks = Serializers.Bir.deserialize sexps in
  let* sub = Bir_helpers.create_sub name blks in
  Log.send "BIR:\n%a" Sub.pp sub;
  Bir_passes.run sub ~target ~language ~patch_info ~func_info

(* Try to deserialize and optimize the BIR program while
   preserving the toplevel state. *)
let try_deserialize_and_opt
    (bir_filepath : string)
    (bir_sexps : Sexp.t list)
    ~(target : T.target)
    ~(language : T.language)
    ~(patch_info : Patch_info.t)
    ~(func_info : Function_info.t) : (Types.t, KB.Conflict.t) result =
  let current = Toplevel.current () in
  let result = try
      let result = Toplevel.var "vibes-opt" in
      let name = Filename.basename bir_filepath in
      Toplevel.put result @@ deserialize_and_opt name bir_sexps
        ~target ~language ~patch_info ~func_info;
      Result.return @@ Toplevel.get result
    with Toplevel.Conflict err -> Error err in
  Toplevel.set current;
  result

let run
    (target : string)
    (language : string)
    (patch_info_filepath : string)
    (bir_filepath : string)
    (func_info_filepath : string)
    (outfile : string) : (unit, KB.Conflict.t) result =
  let (let*) x f = Result.bind x ~f in
  Log.send "Vibes_opt_lib.Runner.run '%s' '%s' '%s' '%s' '%s' '%s'"
    target language patch_info_filepath
    bir_filepath func_info_filepath outfile;
  Log.send "Loading patch-info";
  let* patch_info = Patch_info.from_file patch_info_filepath in
  let* target = Utils.Core_theory.get_target target in
  let* language = Utils.Core_theory.get_language language in
  let* raw_bir_code = Utils.Files.get_file_contents_non_empty bir_filepath
      ~error:(fun s -> Errors.No_bir (
          Format.sprintf "No serialized BIR in file '%s'" s)) in
  let* bir_sexps = Utils.Sexp.to_sexp raw_bir_code
      ~error:(fun s -> Errors.Invalid_bir (
          Format.sprintf "Invalid BIR S-exp: %s" s)) in
  let* func_info = Function_info.from_file func_info_filepath in
  Log.send "Function info:\n%a" Function_info.pp func_info;
  let* _ = try_deserialize_and_opt bir_filepath bir_sexps
      ~target ~language ~patch_info ~func_info in

  (* TODO:
     Write new bir to file, and also cfg, exclude_regs, and argument_tids?
     Look to vibes-parse/lib/runner.ml as a model for how to write files... *)

  Ok ()
