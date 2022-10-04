open Core
open Bap.Std
open Bap_core_theory

module T = Theory
module CT = Vibes_utils.Core_theory
module Sexps = Vibes_utils.Sexp
module Files = Vibes_utils.Files
module Log = Vibes_log.Stream
module Serializers = Vibes_serializers
module Ir = Vibes_ir.Types
module Patch_info = Vibes_patch_info.Types

let (let*) x f = Result.bind x ~f

let try_deserialize (vir_sexp : Sexp.t) : (Ir.t, KB.conflict) result =
  try
    let result = Toplevel.var "vibes-as" in
    Toplevel.put result @@ Serializers.Vir.deserialize vir_sexp;
    Result.return @@ Toplevel.get result      
  with Toplevel.Conflict err -> Error err

let run
    ?(extra_constraints_filepath : string option = None)
    ~(target : string)
    ~(language : string)
    ~(vir_filepath : string)
    ~(patch_info_filepath : string)
    ~(asm_outfile : string)
    ~(model_filepath : string) 
     () : (unit, KB.conflict) result =
  Log.send "Vibes_as.Runner.run '%s' '%s' '%s' '%s' '%s' '%s' '%s'"
    target language vir_filepath patch_info_filepath asm_outfile
    model_filepath (Option.value extra_constraints_filepath ~default:"(none)");
  let* target = CT.get_target target in
  let* language = CT.get_language language in
  let* patch_info = Patch_info.from_file patch_info_filepath in
  let* constraints = match extra_constraints_filepath with
    | None -> Ok None
    | Some path ->
      let* constraints = Files.get_file_contents path in
      Ok (Some constraints) in
  let* raw_vir_code = Files.get_file_contents_non_empty vir_filepath
      ~error:(fun s -> Errors.No_vir (
          Format.sprintf "No serialized VIR in file '%s'" s)) in
  let* vir_sexps = Sexps.to_sexp raw_vir_code
      ~error:(fun s -> Errors.Invalid_vir (
          Format.sprintf "Invalid VIR S-exp: %s" s)) in
  let* vir_sexp = match vir_sexps with
    | [sexp] -> Ok sexp
    | _ -> Error (Errors.Invalid_vir "Expected single S-exp") in
  let* ir = try_deserialize vir_sexp in
  Log.send "Deserialized VIBES IR:\n%a\n" Ir.pp ir;
  let* ir = Solver.solve ir target language model_filepath ~constraints in
  Log.send "Solved VIBES IR:\n%a\n" Ir.pp ir;
  let* printer = Utils.asm_printer target language in
  let* asm = printer ir patch_info in
  Log.send "Assembly:\n%a\n" Types.Assembly.pp asm;
  let asm_sexp = Types.Assembly.sexp_of_t asm in
  let asm_data = Sexp.to_string_hum asm_sexp in
  Log.send "Writing serialized assembly";
  let finalized_asm = Format.sprintf "%s\n" asm_data in
  let* () = Files.write_or_error finalized_asm asm_outfile in
  Ok ()
