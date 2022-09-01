open Core
open Bap.Std
open Bap_core_theory

module T = Theory
module Log = Vibes_log.Stream
module Utils = Vibes_utils
module Serializers = Vibes_serializers
module Ir = Vibes_ir.Types

let (let*) x f = Result.bind x ~f

let try_deserialize (vir_sexp : Sexp.t) : (Ir.t, KB.conflict) result =
  try
    let result = Toplevel.var "vibes-as" in
    Toplevel.put result @@ Serializers.Vir.deserialize vir_sexp;
    Result.return @@ Toplevel.get result      
  with Toplevel.Conflict err -> Error err

let printer
    (target : T.target)
    (language : T.language) : (Types.Assembly.printer, KB.conflict) result =
  if T.Target.belongs Arm_target.parent target then
    let is_thumb = Utils.Core_theory.is_thumb language in
    Ok (Arm_printer.ir ~is_thumb)
  else
    let msg = Format.asprintf
        "Unsupported target %a"
        T.Target.pp target in
    Error (Errors.Unsupported_target msg)

let run
    ~(target : string)
    ~(language : string)
    ~(vir_filepath : string)
    ~(asm_outfile : string)
    ~(model_filepath : string) : (unit, KB.conflict) result =
  Log.send "Vibes_as.Runner.run '%s' '%s' '%s' '%s' '%s'"
    target language vir_filepath asm_outfile model_filepath;
  let* target = Utils.Core_theory.get_target target in
  let* language = Utils.Core_theory.get_language language in
  let* raw_vir_code = Utils.Files.get_file_contents_non_empty vir_filepath
      ~error:(fun s -> Errors.No_vir (
          Format.sprintf "No serialized VIR in file '%s'" s)) in
  let* vir_sexps = Utils.Sexp.to_sexp raw_vir_code
      ~error:(fun s -> Errors.Invalid_vir (
          Format.sprintf "Invalid VIR S-exp: %s" s)) in
  let* vir_sexp = match vir_sexps with
    | [sexp] -> Ok sexp
    | _ -> Error (Errors.Invalid_vir "Expected single S-exp") in
  let* ir = try_deserialize vir_sexp in
  Log.send "Deserialized VIBES IR:\n%a\n" Ir.pp ir;
  let* ir = Solver.solve ir target language model_filepath in
  Log.send "Solved VIBES IR:\n%a\n" Ir.pp ir;
  let* printer = printer target language in
  let* asm = printer ir in
  Log.send "Assembly:\n%a\n" Types.Assembly.pp asm;
  let asm_sexp = Types.Assembly.sexp_of_t asm in
  let asm_data = Sexp.to_string_hum asm_sexp in
  Log.send "Writing serialized assembly";
  let finalized_asm = Format.sprintf "%s\n" asm_data in
  let* () = Utils.Files.write_or_error finalized_asm asm_outfile in
  Ok ()
