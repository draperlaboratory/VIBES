open Core
open Bap.Std
open Bap_core_theory

module T = Theory
module Log = Vibes_log_lib.Stream
module Utils = Vibes_utils_lib
module Serializers = Vibes_serializers_lib
module C_toolkit = Vibes_c_toolkit_lib
module Function_info = Vibes_function_info_lib.Types
module Patch_info = Vibes_patch_info_lib.Types
module Hvar = Vibes_higher_vars_lib.Higher_var
module Parsed_c_code = Types.Parsed_c_code

open KB.Syntax

let log_blk (blk : blk term) : unit =
  Log.send "BIR block:\n%a" Blk.pp blk

let parse_c_code (raw_code : string) : (Types.ast, KB.Conflict.t) result =
  Log.send "Parsing C code";
  match C_toolkit.Parse_c.parse raw_code with
  | Error msg -> Error (Errors.Invalid_C msg)
  | Ok ast ->
    Log.send "Parsed:\n%s" @@
    C_toolkit.C_utils.print_c Cprint.print_def ast;
    Ok ast

let create_compilation_unit (target : T.target) : T.Unit.t KB.t =
  let* compilation_unit = KB.Object.create T.Unit.cls in
  let* () = KB.provide T.Unit.target compilation_unit target in
  KB.return compilation_unit

let to_core
    (ast : Types.ast)
    (target : T.target)
    (hvars : Hvar.t list) : T.label KB.t =
  let* label = KB.Object.create T.Program.cls in
  let* compilation_unit = create_compilation_unit target in
  let* () = KB.provide T.Label.unit label (Some compilation_unit) in
  let* theory = T.instance () in
  let* (module Core) = T.require theory in
  let module C_parser = C_toolkit.Core_c.Eval(Core) in
  let* sem, func_info = C_parser.parse hvars target ast in
  let* sem = sem in
  let* () = Parsed_c_code.set label sem in
  let+ () = Parsed_c_code.stash_function_info label func_info in
  label

type computed = T.Semantics.t * Function_info.t

let compute
    (ast : Types.ast)
    (target : T.target)
    (hvars : Hvar.t list) : (computed, KB.Conflict.t) result =
  match KB.run T.Program.cls (to_core ast target hvars) KB.empty with
  | Error _ as err -> err
  | Ok (snapshot, _) ->
    Log.send "Snapshot:\n%a" KB.Value.pp snapshot;
    let sem = KB.Value.get T.Semantics.slot snapshot in
    Log.send "Promised semantics:\n%a" KB.Value.pp sem;
    let func_info = KB.Value.get Parsed_c_code.function_info_slot snapshot in
    Log.send "Promised function info:\n%a" Function_info.pp func_info;
    Ok (sem, func_info)

let no_patch_code filename =
  Errors.No_patch_code (Format.sprintf "No patch code in file: '%s'" filename)

(* Make sure to discard the changes to the Toplevel KB. *)
let lift_bir (sem : insn) : blk term list =
  let current = Toplevel.current () in
  let bir = Blk.from_insns [sem] in
  Toplevel.set current;
  List.map bir ~f:(Format.asprintf "%a" Blk.pp) |>
  String.concat ~sep:"\n" |>
  Log.send "Lifted BIR program:\n%s";
  bir

let run
    (target : string)
    (patch_info_filepath : string)
    (patch_filepath : string)
    (bir_outfile : string)
    (func_info_outfile : string) : (unit, KB.Conflict.t) result =
  let (let*) x f = Result.bind x ~f in
  Log.send "Vibes_parse_lib.Runner.run '%s' '%s' '%s' '%s' '%s'"
    target patch_info_filepath patch_filepath bir_outfile func_info_outfile;
  Log.send "Loading patch-info";
  let* patch_info = Patch_info.from_file patch_info_filepath in
  let hvars = Patch_info.patch_vars patch_info in
  let* target = Utils.Core_theory.get_target target in
  let* raw_code = Utils.Files.get_file_contents_non_empty
      patch_filepath ~error:no_patch_code in
  let* ast = parse_c_code raw_code in
  let* semantics, func_info = compute ast target hvars in
  let finalized_func_info = Function_info.to_string func_info in
  Log.send "Finalized function info:\n%s" finalized_func_info;
  let bir = lift_bir semantics in
  let bir_sexps = List.map bir ~f:Serializers.Bir.serialize in
  let bir_strings = List.map bir_sexps ~f:Sexp.to_string in
  let bir_data = String.concat bir_strings ~sep:"\n" in
  Log.send "Serialized BIR:\n%s" bir_data;
  Log.send "Writing serialized BIR";
  let finalized_bir = Format.sprintf "%s\n" bir_data in
  let* () = Utils.Files.write_or_error finalized_bir bir_outfile in
  Log.send "Writing serialized function info";
  Utils.Files.write_or_error finalized_func_info func_info_outfile 
