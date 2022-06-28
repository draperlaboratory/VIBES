open Core_kernel
open Vibes_c_toolkit_lib

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB
module Log = Vibes_log_lib.Stream
module Err = Vibes_error_lib.Std
module Utils = Vibes_utils_lib
module Serializers = Vibes_serializers_lib
module Parsed_C_code = Types.Parsed_C_code

open KB.Let
open Vibes_error_lib.Let

let get_target (name : string) : (T.Target.t, Err.t) result =
  match T.Target.lookup name with
  | None ->
     let msg = Format.sprintf "Unknown target: '%s'" name in
     Error (Types.Unknown_target msg)
  | Some target -> Ok target

let load_from_file (filepath : string) : (string, Err.t) result =
  let- lines = Utils.Files.get_lines_or_error filepath in
  let raw_code = String.concat lines ~sep:"\n" in
  if String.is_empty raw_code then
    let msg = Format.sprintf "No code in file '%s'" filepath in
    Error (Types.No_patch_code msg)
  else
    Ok raw_code

let parse_C_code (raw_code : string) : (Types.ast, Err.t) result =
  Log.send "Parsing C code";
  match Parse_c.parse raw_code with
  | Ok ast ->
     let code_str = C_utils.print_c Cprint.print_def ast in
     let msg = Format.sprintf "Parsed:\n%s" code_str in
     Log.send msg;
     Ok ast
  | Error msg ->
     Error (Types.Invalid_C msg)

let create_compilation_unit (target : T.Target.t) : T.Unit.t KB.t =
  let* compilation_unit = KB.Object.create T.Unit.cls in
  let* () = KB.provide T.Unit.target compilation_unit target in
  KB.return compilation_unit

let to_core (ast : Types.ast) (target : T.Target.t) : T.Label.t KB.t =
  let* label = KB.Object.create T.Program.cls in
  let* compilation_unit = create_compilation_unit target in
  let* () = KB.provide T.Label.unit label (Some compilation_unit) in
  let* theory = T.instance () in
  let* (module Core) = T.require theory in
  let module C_parser = Core_c.Eval(Core) in
  let hvars = [] in (* TODO: Pull in hvars from file *)
  let* sem = C_parser.parse hvars target ast in
  let* () = Parsed_C_code.set label sem in
  KB.return label

let compute (ast : Types.ast) (target : T.Target.t)
    : (T.Semantics.t, Err.t) result =
  let result = KB.run T.Program.cls (to_core ast target) KB.empty in
  match result with
  | Ok (snapshot, _) ->
     Log.send (Format.asprintf "Snapshot:\n%a" KB.Value.pp snapshot);
     let sem = KB.Value.get T.Semantics.slot snapshot in
     Log.send (Format.asprintf "Promised semantics:\n%a" KB.Value.pp sem);
     Ok sem
  | Error e ->
     let msg = Format.asprintf
       "Error computing semantics: %a\n%!"
       KB.Conflict.pp e
     in
     Error (Types.KB_error msg)

let run (target : string) (filepath : string) (outfile : string)
    : (unit, Err.t) result =
  let msg = Format.sprintf
    "Vibes_parse_lib.Runner.run '%s' '%s'"
    filepath
    outfile
  in
  Log.send msg;

  let- target = get_target target in
  let- raw_code = load_from_file filepath in
  let- ast = parse_C_code raw_code in
  let- semantics = compute ast target in

  let birs = Bap.Std.Blk.from_insns [semantics] in
  let log_blk blk =
    Log.send (Format.asprintf "BIR block:\n%a" Bap.Std.Blk.pp blk)
  in
  List.iter birs ~f:log_blk;

  let- sexps = Result.all (List.map birs ~f:Serializers.Bir.serialize) in
  let sexps_str = List.map sexps ~f:Sexp.to_string in
  let data = String.concat sexps_str ~sep:"\n" in
  Log.send (Format.sprintf "Serialized BIR:\n%s" data);

  let final_data = Format.sprintf "%s\n" data in
  Utils.Files.write_or_error final_data outfile
