open Core
open Bap_core_theory

module T = Theory
module CT = Vibes_utils.Core_theory
module Sexps = Vibes_utils.Sexp
module Files = Vibes_utils.Files
module Log = Vibes_log.Stream
module Asm = Vibes_as.Types.Assembly
module Patch_info = Vibes_patch_info.Types
module Spaces = Patch_info.Spaces

let (let*) x f = Result.bind x ~f

let parse_asms
    (asm_filepaths : string list) : (Asm.t list, KB.conflict) result =
  let rec aux acc = function
    | [] -> Ok (List.rev acc)
    | path :: rest ->
      let* asm_sexp_raw = Files.get_file_contents_non_empty path
          ~error:(fun s ->
              let msg = Format.sprintf
                  "Expected non-empty ASM for file %s" s in
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
      aux (asm :: acc) rest in
  aux [] asm_filepaths

let run
    ?(patch_spaces : string option = None)
    ?(ogre_filepath : string option = None)
    ~(target : string)
    ~(language : string)
    ~(binary : string)
    ~(asm_filepaths : string list)
    ~(patched_binary : string) 
    () : (unit, KB.conflict) result =
  Log.send "Vibes_patch.Runner.run '%s' '%s' '%s' '%s' '%s'"
    target language binary
    (String.concat ~sep:", " asm_filepaths)
    patched_binary;
  let* target = CT.get_target target in
  let* language = CT.get_language language in
  let* patch_spaces = match patch_spaces with
    | Some path -> Spaces.from_file path
    | None -> Ok Spaces.empty in
  let* () = match ogre_filepath with
    | None -> Ok ()
    | Some path -> match Sys_unix.file_exists path with
      | `Yes -> Ok ()
      | `No | `Unknown ->
        let msg = Format.sprintf "OGRE file %s not found" path in
        Error (Errors.Invalid_ogre msg) in
  let* asms = parse_asms asm_filepaths in
  let* _, spaces =
    Patcher.patch target language asms
      ~binary ~patched_binary ~patch_spaces
      ~backend:ogre_filepath in
  if not @@ Spaces.is_empty spaces then
    Log.send "Remaining patch spaces:\n%a" Spaces.pp spaces;
  Ok ()
