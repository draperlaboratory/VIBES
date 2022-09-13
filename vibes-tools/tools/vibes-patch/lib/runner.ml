open Core
open Bap_core_theory

module T = Theory
module CT = Vibes_utils.Core_theory
module Sexps = Vibes_utils.Sexp
module Files = Vibes_utils.Files
module Log = Vibes_log.Stream
module Asm = Vibes_as.Types.Assembly
module Patch_info = Vibes_patch_info.Types

let (let*) x f = Result.bind x ~f

let run
    ?(ogre_filepath : string option = None)
    ~(target : string)
    ~(language : string)
    ~(patch_info_filepath : string)
    ~(binary : string)
    ~(asm_filepath : string)
    ~(patched_binary : string) : (unit, KB.conflict) result =
  Log.send "Vibes_patch.Runner.run '%s' '%s' '%s' '%s' '%s' '%s'"
    target language patch_info_filepath binary asm_filepath patched_binary;
  let* target = CT.get_target target in
  let* language = CT.get_language language in
  let* patch_info = Patch_info.from_file patch_info_filepath in
  Log.send "Parsed patch info:\n%a\n" Patch_info.pp patch_info;
  let* asm_sexp_raw = Files.get_file_contents_non_empty asm_filepath
      ~error:(fun s ->
          let msg = Format.sprintf "Expected non-empty ASM for file %s" s in
          Errors.Invalid_asm msg) in
  let* asm_sexps = Sexps.to_sexp asm_sexp_raw
      ~error:(fun s -> Errors.Invalid_asm (
          Format.sprintf "Invalid ASM S-exp: %s" s)) in
  let* asm_sexp = match asm_sexps with
    | [sexp] -> Ok sexp
    | _ ->
      let msg = Format.sprintf "Expected single ASM S-exp" in
      Error (Errors.Invalid_asm msg) in
  let* asm = try Ok (Asm.t_of_sexp asm_sexp) with
    | _ ->
      let msg = Format.asprintf "Invalid ASM: %a" Sexp.pp_hum asm_sexp in
      Error (Errors.Invalid_asm msg) in
  Log.send "Assembly:\n%a\n%!" Asm.pp asm;
  let* () = match ogre_filepath with
    | None -> Ok ()
    | Some path -> match Sys_unix.file_exists path with
      | `Yes -> Ok ()
      | `No | `Unknown ->
        let msg = Format.sprintf "OGRE file %s not found" path in
        Error (Errors.Invalid_ogre msg) in
  let* patch =
    Patcher.patch patch_info target language
      asm ~binary ~patched_binary ~backend:ogre_filepath in
  Format.printf "Placed at 0x%Lx using %Ld bytes\n%!"
    patch.Patcher.addr patch.Patcher.len;
  Ok ()
