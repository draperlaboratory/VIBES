open Core
open Bap.Std
open Bap_core_theory

module T = Theory
module Log = Vibes_log.Stream
module Utils = Vibes_utils
module Serializers = Vibes_serializers
module Ir = Vibes_ir.Types
module Hvar = Vibes_higher_vars.Higher_var
module Patch_info = Vibes_patch_info.Types

open KB.Syntax

let deserialize_and_select
    (sexp : Sexp.t)
    ~(hvars : Hvar.t list)
    ~(target : T.target)
    ~(language : T.language) : Ir.t KB.t =
  let* sub = Serializers.Bir.deserialize sexp in
  Log.send "Deserialized BIR:\n\n%a" Sub.pp sub;
  Selector.run sub ~hvars ~target ~language    

let try_deserialize_and_select
    (bir_sexp : Sexp.t)
    ~(hvars : Hvar.t list)
    ~(target : T.target)
    ~(language : T.language) : (Ir.t, KB.conflict) result =
  try
    let result = Toplevel.var "vibes-select" in
    Toplevel.put result @@
    deserialize_and_select bir_sexp ~hvars ~target ~language;
    Result.return @@ Toplevel.get result      
  with Toplevel.Conflict err -> Error err
         
let run
    ~(target : string)
    ~(language : string)
    ~(patch_info_filepath : string)
    ~(bir_filepath : string)
    ~(vir_outfile : string) : (unit, KB.conflict) result =
  let (let*) x f = Result.bind x ~f in
  Log.send "Vibes_select.Runner.run '%s' '%s' '%s' '%s"
    target language bir_filepath vir_outfile;
  Log.send "Loading patch-info";
  let* patch_info = Patch_info.from_file patch_info_filepath in
  let hvars = patch_info.patch_vars in
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
  let* ir = try_deserialize_and_select bir_sexp ~hvars ~target ~language in
  Log.send "Serializing VIBES IR";
  let ir_sexp = Serializers.Vir.serialize ir in
  let ir_data = Sexp.to_string_hum ir_sexp in
  let finalized_ir = Format.sprintf "%s\n" ir_data in
  let* () = Utils.Files.write_or_error finalized_ir vir_outfile in
  Ok ()
