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
module Json = Vibes_utils.Json

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
    ?(ogre : string option = None)
    ?(patch_spaces : string option = None)
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
  let had_spaces = Option.is_some patch_spaces in
  let* patch_spaces = match patch_spaces with
    | Some path -> Spaces.from_file path
    | None -> Ok Spaces.empty in
  let* ogre = match ogre with
    | None -> Ok None
    | Some path -> match Ogre.Doc.from_file path with
      | Error err -> Error (Errors.Invalid_ogre (Error.to_string_hum err))
      | Ok ogre -> Ok (Some ogre) in
  let* asms = parse_asms asm_filepaths in
  let* res = Patcher.patch target language asms
      ~binary ~patched_binary ~patch_spaces ~ogre in
  let* () =
    if Spaces.is_empty res.spaces then
      Ok (if had_spaces then Log.send "No patch spaces remaining")
    else
      let path = Format.sprintf "%s-patch-spaces.json" patched_binary in
      let pp = Json.pp ~yojson_of_t:Spaces.yojson_of_t in
      let data = Format.asprintf "%a" pp res.spaces in
      Log.send "Remaining patch spaces:\n%a" Spaces.pp res.spaces;
      Log.send "Writing to %s" path;
      Files.write_or_error data path in
  let* () = match res.new_ogre with
    | None -> Ok ()
    | Some ogre ->
      let path = Format.sprintf "%s.ogre" patched_binary in
      let data = Ogre.Doc.to_string ogre in
      Log.send "Writing new OGRE specification to %s" path;
      Files.write_or_error data path in
  Ok ()
