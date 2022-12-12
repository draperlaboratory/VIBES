open Core

let dummy_decl : string = "void dummy()"
let dummy_len : int = String.length dummy_decl + 1

let file_pos (input : string) (lexbuf : Lexing.lexbuf) : string =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  let l, c =
    (* The lexer seems to ignore the newlines when updating the
       position in the buffer, so we will do it manually. *)
    let c = pos.pos_cnum - pos.pos_bol + 1 in
    let init = 1, 1, c and finish (l, c, _) = l, c in
    String.fold_until input ~init ~finish ~f:(fun (l, c, r) x ->
        let l, c, r = match x with
          | '\r' -> l, 1, r
          | '\n' -> l + 1, 1, r
          | _ -> l, c + 1, r - 1 in
        if r <= 1 then Stop (l, c)
        else Continue (l, c, r)) in
  (* First line should account for the dummy declaration. *)
  let c = if l = 1 then c - dummy_len else c in
  Format.sprintf "line %d, column %d" l c

(* Parses a string into a Cabs.file *)
let parse_c_file (input : string) : (Cabs.file, string) result =
  let lexbuf = Lexing.from_string input ~with_positions:true in
  Clexer.init_lexicon ();
  match Cparser.file Clexer.initial lexbuf with
  | exception exn ->
    let msg =
      Format.asprintf "%a: %s" Exn.pp exn @@
      file_pos input lexbuf in
    Error msg
  | ast -> Ok ast

(* Parses a sequence of C statements (a body) by wrapping it in a
   dummy function, parsing that as a file and then extracting the
   first def from the result *)
let parse (input : string) : (Cabs.definition, string) result =
  let p = Printf.sprintf "%s{%s}" dummy_decl input in
  match parse_c_file p with
  | Ok [def] -> Ok def
  | Ok [] -> Error "No definitions parsed"
  | Ok _ -> Error "More than one definition parsed, expected only one"
  | Error _ as err -> err
