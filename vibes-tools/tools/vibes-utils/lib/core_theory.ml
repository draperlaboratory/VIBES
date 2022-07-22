open Core

module T = Bap_core_theory.Theory
module Err = Vibes_error_lib.Std

let is_thumb (language : T.Language.t) : bool = 
  String.is_substring ~substring:"thumb" @@
  T.Language.to_string language

let get_target (name : string) : (T.Target.t, Err.t) result =
  match T.Target.lookup name with
  | None ->
    let msg = Format.sprintf "Unknown target: '%s'" name in
    Error (Types.Unknown_target msg)
  | Some target -> Ok target
 
let get_language (name : string) : (T.Language.t, Err.t) result =
  try
    let language = T.Language.read ~package:"bap" name in
    Ok language
  with _ ->
    let msg = Format.sprintf "Unknown language: '%s'" name in
    Error (Types.Unknown_language msg)
