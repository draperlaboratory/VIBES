open Core

(* Parses a string into a Cabs.file *)
let parse_c_file (input : string) : (Cabs.file, string) result =
  let open Clexer in
  try
    init_lexicon ();
    Ok (Cparser.file initial @@ Lexing.from_string input)
  with
  | Parsing.Parse_error -> Error "Parsing.Parse_error"
  | Cabs.BadType -> Error "Cabs.BadType"
  | Cabs.BadModifier -> Error "Cabs.BadModifier"
  | Invalid_argument _ ->
    (* Workaround due to a FrontC bug *)
    Error "Some FrontC parse error"
  | _ -> Error "Internal parse error."

(* Parses a sequence of C statements (a body) by wrapping it in a
   dummy function, parsing that as a file and then extracting the
   first def from the result *)
let parse (input : string) : (Cabs.definition, string) result =
  let p = Printf.sprintf "int VIBES_PATCH_WRAPPER_FUN(){\n%s\n}" input in
  match parse_c_file p with
  | Ok [def] -> Ok def
  | Ok [] -> Error "No definitions parsed"
  | Ok _ -> Error "More than one definition parsed, expected only one"
  | Error _ as err -> err
