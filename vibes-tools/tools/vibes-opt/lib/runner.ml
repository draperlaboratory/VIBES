open Core

module T = Bap_core_theory.Theory
module Log = Vibes_log_lib.Stream
module Err = Vibes_error_lib.Std
module Utils = Vibes_utils_lib
module Serializers = Vibes_serializers_lib
module Patch_info = Vibes_patch_info_lib.Types
module Function_info = Vibes_function_info_lib.Types

open Vibes_error_lib.Let

let log_bir bir =
  Log.send (Format.asprintf "BIR:\n%a" Bap.Std.Blk.pp bir)

let run (target : string) (language : string) (patch_info_filepath : string)
    (bir_filepath : string) (func_info_filepath : string)
    (outfile : string) : (unit, Err.t) result =
  let msg = Format.sprintf
    "Vibes_opt_lib.Runner.run '%s' '%s' '%s' '%s' '%s' '%s'"
    target language patch_info_filepath
    bir_filepath func_info_filepath outfile
  in
  Log.send msg;

  Log.send "Loading patch-info";
  let- patch_info = Patch_info.from_file patch_info_filepath in
  let sp_align = Patch_info.sp_align patch_info in
  let hvars = Patch_info.patch_vars patch_info in

  let- target = Utils.Core_theory.get_target target in
  let- language = Utils.Core_theory.get_language language in

  let error s =
    Types.No_bir (Format.sprintf "No serialized BIR in file '%s'" s) in
  let- raw_bir_code = Utils.Files.get_file_contents bir_filepath ~error in

  let error s =
    Types.Invalid_bir (Format.sprintf "Invalid BIR S-exp: %s" s) in
  let- bir_sexps = Utils.Sexp.to_sexp raw_bir_code ~error in

  let- blks = Result.all (List.map bir_sexps
    ~f:(fun sexp -> Serializers.Bir.deserialize sexp ~target))
  in
  List.iter blks ~f:log_bir;

  let- func_info = Function_info.from_file func_info_filepath in
  Log.send (Format.asprintf "Function info:\n%a" Function_info.pp func_info);

  let- _ (* {bir; cfg; exclude_regs; argument_tids} *) =
    Bir_passes.run blks 
      ~target ~language ~patch_info ~func_info ~hvars ~sp_align
  in

  (* TODO:
     Write new bir to file, and also cfg, exclude_regs, and argument_tids?
     Look to vibes-parse/lib/runner.ml as a model for how to write files... *)

  Ok ()
