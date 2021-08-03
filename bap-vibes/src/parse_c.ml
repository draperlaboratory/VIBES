open Core_kernel

(* Parses a string into a Cabs.file *)
let parse_c_file (input : string) : (Cabs.file, string) result =
  let open Clexer in
  try
    init_lexicon ();
    Ok (Cparser.file initial (Lexing.from_string input))
  with
  (* | Cparser.Error -> Error "Cparser.Error" *)
  | Parsing.Parse_error -> Error "Parsing.Parse_error"
  | Cabs.BadType -> Error "Cabs.BadType"
  | Cabs.BadModifier -> Error "Cabs.BadModifier"
  (* Workaround due to a FrontC bug *)
  | Invalid_argument _ -> Error "Some FrontC parse error"

(* Parses a sequence of C statements (a body) by wrapping it in a
   dummy function, parsing that as a file and then extracting the
   first def from the result *)
let parse_c_patch (input : string) : (Cabs.definition, string) result =
  let (let*) = Result.(>>=) in
  let p = Printf.sprintf "int VIBES_PATCH_WRAPPER_FUN(){\n%s\n}" input in
  let* parse_result = parse_c_file p in
  match parse_result with
  | def::[] -> Ok def
  (* FIXME: forward the error type from FrontC to the user *)
  | _ -> Error "VIBES: Unexpected Form in FrontC Parse"
