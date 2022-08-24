open Core
open Bap.Std
open Bap_core_theory

module T = Theory
module Log = Vibes_log.Stream
module Utils = Vibes_utils
module Serializers = Vibes_serializers
module C_toolkit = Vibes_c_toolkit
module Function_info = Vibes_function_info.Types
module Patch_info = Vibes_patch_info.Types
module Hvar = Vibes_higher_vars.Higher_var
module Bir_helpers = Vibes_bir.Helpers

open KB.Syntax

let parse_c_code (raw_code : string) : (Types.ast, KB.conflict) result =
  Log.send "Parsing C code";
  match C_toolkit.Parse_c.parse raw_code with
  | Error msg -> Error (Errors.Invalid_C msg)
  | Ok ast ->
    Log.send "Parsed:\n%s" @@
    C_toolkit.C_utils.print_c Cprint.print_def ast;
    Ok ast

let compile
    (name : string)
    (ast : Types.ast)
    (target : T.target)
    (hvars : Hvar.t list) : (sub term * Function_info.t, KB.conflict) result =
  try
    let result = Toplevel.var "vibes-parse" in
    Toplevel.put result begin
      let* label, func_infos = Compile.to_core ast target hvars in
      let* sem = KB.collect T.Semantics.slot label in
      Log.send "Semantics:\n%a" KB.Value.pp sem;
      let* blks = Blk.KB.from_insns [sem] in
      let+ sub = Bir_helpers.create_sub name blks in
      Log.send "Lifted BIR program:\n%a" Sub.pp sub;
      sub, func_infos
    end;
    Ok (Toplevel.get result)
  with Toplevel.Conflict err -> Error err

let no_patch_code filename =
  Errors.No_patch_code (Format.sprintf "No patch code in file: '%s'" filename)

let run
    ~(target : string)
    ~(patch_info_filepath : string)
    ~(patch_filepath : string)
    ~(bir_outfile : string)
    ~(func_info_outfile : string) : (unit, KB.conflict) result =
  let (let*) x f = Result.bind x ~f in
  Log.send "Vibes_parse.Runner.run '%s' '%s' '%s' '%s' '%s'"
    target patch_info_filepath patch_filepath bir_outfile func_info_outfile;
  Log.send "Loading patch-info";
  let* patch_info = Patch_info.from_file patch_info_filepath in
  let hvars = patch_info.patch_vars in
  let* target = Utils.Core_theory.get_target target in
  let* raw_code = Utils.Files.get_file_contents_non_empty
      patch_filepath ~error:no_patch_code in
  let* ast = parse_c_code raw_code in
  let bir_name = Filename.basename bir_outfile in
  let* bir, func_info = compile bir_name ast target hvars in
  let finalized_func_info = Function_info.to_string func_info in
  Log.send "Finalized function info:\n%s" finalized_func_info;
  let bir_sexp = Serializers.Bir.serialize bir in
  let bir_data = Sexp.to_string_hum bir_sexp in
  Log.send "Writing serialized BIR";
  let finalized_bir = Format.sprintf "%s\n" bir_data in
  let* () = Utils.Files.write_or_error finalized_bir bir_outfile in
  Log.send "Writing serialized function info";
  Utils.Files.write_or_error finalized_func_info func_info_outfile 
