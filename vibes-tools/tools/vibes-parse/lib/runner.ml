open Core

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB
module Log = Vibes_log_lib.Stream
module Err = Vibes_error_lib.Std
module Utils = Vibes_utils_lib
module Serializers = Vibes_serializers_lib
module C_toolkit = Vibes_c_toolkit_lib
module Func_info = C_toolkit.Types.Func_info
module Patch_info = Vibes_patch_info_lib.Types
module Hvar = Vibes_higher_vars_lib.Higher_var
module Parsed_c_code = Types.Parsed_c_code

open KB.Let
open Vibes_error_lib.Let

let log_blk (blk : Bap.Std.Blk.t) : unit =
  Log.send (Format.asprintf "BIR block:\n%a" Bap.Std.Blk.pp blk)

let log_func_info (info : Func_info.t) : unit =
  Log.send (Format.sprintf "Func info: %s" (Func_info.to_string info))

let parse_C_code (raw_code : string) : (Types.ast, Err.t) result =
  Log.send "Parsing C code";
  match C_toolkit.Parse_c.parse raw_code with
  | Ok ast ->
     let code_str = C_toolkit.C_utils.print_c Cprint.print_def ast in
     let msg = Format.sprintf "Parsed:\n%s" code_str in
     Log.send msg;
     Ok ast
  | Error msg ->
     Error (Types.Invalid_C msg)

let create_compilation_unit (target : T.Target.t) : T.Unit.t KB.t =
  let* compilation_unit = KB.Object.create T.Unit.cls in
  let* () = KB.provide T.Unit.target compilation_unit target in
  KB.return compilation_unit

let to_core (ast : Types.ast) (target : T.Target.t) (hvars : Hvar.t list)
    : T.Label.t KB.t =
  let* label = KB.Object.create T.Program.cls in
  let* compilation_unit = create_compilation_unit target in
  let* () = KB.provide T.Label.unit label (Some compilation_unit) in
  let* theory = T.instance () in
  let* (module Core) = T.require theory in
  let module C_parser = C_toolkit.Core_c.Eval(Core) in
  let* (sem, func_infos) = C_parser.parse hvars target ast in
  let* sem = sem in
  let* () = Parsed_c_code.set label sem in
  let* () = Parsed_c_code.stash_func_infos label func_infos in
  KB.return label

let compute (ast : Types.ast) (target : T.Target.t) (hvars : Hvar.t list)
    : (T.Semantics.t * Func_info.t list, Err.t) result =
  let result = KB.run T.Program.cls (to_core ast target hvars) KB.empty in
  match result with
  | Ok (snapshot, _) ->
     Log.send (Format.asprintf "Snapshot:\n%a" KB.Value.pp snapshot);
     let sem = KB.Value.get T.Semantics.slot snapshot in
     Log.send (Format.asprintf "Promised semantics:\n%a" KB.Value.pp sem);
     let func_infos = KB.Value.get Parsed_c_code.func_infos_slot snapshot in
     List.iter func_infos ~f:log_func_info;
     Ok (sem, func_infos)
  | Error e ->
     let msg = Format.asprintf
       "Error computing semantics: %a\n%!"
       KB.Conflict.pp e
     in
     Error (Types.KB_error msg)

let run (target : string) (patch_info_filepath : string)
    (patch_filepath : string) (bir_outfile : string)
    (func_info_outfile : string) : (unit, Err.t) result =
  let msg = Format.sprintf
    "Vibes_parse_lib.Runner.run '%s' '%s' '%s' '%s' '%s'"
    target patch_info_filepath patch_filepath bir_outfile func_info_outfile
  in
  Log.send msg;

  Log.send "Loading patch-info";
  let- patch_info = Patch_info.json_of patch_info_filepath in
  let hvars = Patch_info.patch_vars patch_info in

  let- target = Utils.Core_theory.get_target target in

  let error s =
    Types.No_patch_code (Format.sprintf "No patch code in file: '%s'" s) in
  let- raw_code = Utils.Files.get_file_contents patch_filepath ~error in

  let- ast = parse_C_code raw_code in
  let- (semantics, func_infos) = compute ast target hvars in

  let bir = Bap.Std.Blk.from_insns [semantics] in
  List.iter bir ~f:log_blk;

  let- bir_sexps = Result.all (List.map bir ~f:Serializers.Bir.serialize) in
  let bir_strings = List.map bir_sexps ~f:Sexp.to_string in
  let bir_data = String.concat bir_strings ~sep:"\n" in
  Log.send (Format.sprintf "Serialized BIR:\n%s" bir_data);
  let finalized_bir_data = Format.sprintf "%s\n" bir_data in

  let func_infos_sexps = List.map func_infos ~f:Func_info.to_sexp in
  let func_infos_strings = List.map func_infos_sexps ~f:Sexp.to_string in
  let func_infos_data = String.concat func_infos_strings ~sep:"\n" in
  Log.send (Format.sprintf "Serialized func infos:\n%s" func_infos_data);
  let finalized_func_infos_data = Format.sprintf "%s\n" func_infos_data in

  Log.send "Writing serialized BIR";
  let- () = Utils.Files.write_or_error finalized_bir_data bir_outfile in

  Log.send "Writing serialized func info";
  Utils.Files.write_or_error finalized_func_infos_data func_info_outfile 
