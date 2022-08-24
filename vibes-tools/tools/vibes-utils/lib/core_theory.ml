open Core
open Bap_core_theory

module T = Theory

let is_thumb (language : T.language) : bool = 
  String.is_substring ~substring:"thumb" @@
  T.Language.to_string language

let get_target (name : string) : (T.target, KB.conflict) result =
  match T.Target.lookup name with
  | None ->
    let msg = Format.sprintf "Unknown target: '%s'" name in
    Error (Errors.Unknown_target msg)
  | Some target -> Ok target
 
let get_language (name : string) : (T.language, KB.conflict) result =
  try
    let language = T.Language.read ~package:"bap" name in
    Ok language
  with _ ->
    let msg = Format.sprintf "Unknown language: '%s'" name in
    Error (Errors.Unknown_language msg)
