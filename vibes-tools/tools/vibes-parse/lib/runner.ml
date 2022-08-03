open Core

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB
module Log = Vibes_log_lib.Stream
module Err = Vibes_error_lib.Std
module Utils = Vibes_utils_lib
module Serializers = Vibes_serializers_lib
module C_toolkit = Vibes_c_toolkit_lib
module Function_info = Vibes_function_info_lib.Types
module Patch_info = Vibes_patch_info_lib.Types
module Hvar = Vibes_higher_vars_lib.Higher_var
module Parsed_c_code = Types.Parsed_c_code

open KB.Let
open Vibes_error_lib.Let

let log_blk (blk : Bap.Std.Blk.t) : unit =
  Log.send (Format.asprintf "BIR block:\n%a" Bap.Std.Blk.pp blk)

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
  let* (sem, func_info) = C_parser.parse hvars target ast in
  let* sem = sem in
  let* () = Parsed_c_code.set label sem in
  let* () = Parsed_c_code.stash_function_info label func_info in
  KB.return label

let compute (ast : Types.ast) (target : T.Target.t) (hvars : Hvar.t list)
    : (T.Semantics.t * Function_info.t, Err.t) result =
  let result = KB.run T.Program.cls (to_core ast target hvars) KB.empty in
  match result with
  | Ok (snapshot, _) ->
     Log.send (Format.asprintf "Snapshot:\n%a" KB.Value.pp snapshot);
     let sem = KB.Value.get T.Semantics.slot snapshot in
     Log.send (Format.asprintf "Promised semantics:\n%a" KB.Value.pp sem);
     let func_info = KB.Value.get Parsed_c_code.function_info_slot snapshot in
     let msg = 
       Format.asprintf "Promised function info:\n%a"
         Function_info.pp func_info in
     Log.send msg;
     Ok (sem, func_info)
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
  let- patch_info = Patch_info.from_file patch_info_filepath in
  let hvars = Patch_info.patch_vars patch_info in

  let- target = Utils.Core_theory.get_target target in

  let error s =
    Types.No_patch_code (Format.sprintf "No patch code in file: '%s'" s) in
  let- raw_code = Utils.Files.get_file_contents patch_filepath ~error in

  let- ast = parse_C_code raw_code in
  let- (semantics, func_info) = compute ast target hvars in

  let finalized_func_info = Function_info.to_string func_info in
  Log.send
    (Format.sprintf "Finalized function info:\n%s" finalized_func_info);

  let bir = Bap.Std.Blk.from_insns [semantics] in
  List.iter bir ~f:log_blk;

  let- bir_sexps = Result.all (List.map bir ~f:Serializers.Bir.serialize) in
  let bir_strings = List.map bir_sexps ~f:Sexp.to_string in
  let bir_data = String.concat bir_strings ~sep:"\n" in
  Log.send (Format.sprintf "Serialized BIR:\n%s" bir_data);
  let finalized_bir = Format.sprintf "%s\n" bir_data in

  Log.send "Writing serialized BIR";
  let- () = Utils.Files.write_or_error finalized_bir bir_outfile in
  
  Log.send "Writing serialized function info";
  Utils.Files.write_or_error finalized_func_info func_info_outfile 
