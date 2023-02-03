(***************************************************************************)
(*                                                                         *)
(*  Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.      *)
(*                                                                         *)
(*  This file is provided under the license found in the LICENSE file in   *)
(*  the top-level directory of this project.                               *)
(*                                                                         *)
(*  This research was developed with funding from the Defense Advanced     *)
(*  Research Projects Agency (DARPA).                                      *)
(*                                                                         *)
(***************************************************************************)

open Core
open Bap_core_theory

module Files = Vibes_utils.Files
module Proc = Vibes_utils.Proc
module Log = Vibes_log.Stream

let (let*) x f = Result.bind x ~f

let dummy_decl : string = "void dummy()"

let err (msg : string) : (_, KB.conflict) result =
  Error (Errors.Parse_c msg)

type builtin = {
  name   : string;
  size   : int;
  signed : bool;
}

let builtin_typenames : builtin list = [
  {name = "int8_t";   size = 8;  signed = true};
  {name = "uint8_t";  size = 8;  signed = false};
  {name = "int16_t";  size = 16; signed = true};
  {name = "uint16_t"; size = 16; signed = false};
  {name = "int32_t";  size = 32; signed = true};
  {name = "uint32_t"; size = 32; signed = false};
  {name = "int64_t";  size = 64; signed = true};
  {name = "uint64_t"; size = 64; signed = false};
]

type pos = {
  lineno : int;
  column : int;
  line   : string option;
}

(* Find the position in the input stream where the parser failed. *)
let file_pos (ic : In_channel.t) (lexbuf : Lexing.lexbuf) : pos =
  let open Lexing in
  In_channel.seek ic 0L;
  let input = In_channel.input_all ic in
  let pos = lexbuf.lex_curr_p in
  let l, c =
    (* The lexer seems to ignore the newlines when updating the
       position in the buffer, so we will do it manually. *)
    let r = pos.pos_cnum - pos.pos_bol + 1 in
    let init = 1, 1, r and finish (l, c, _) = l, c in
    String.fold_until input ~init ~finish ~f:(fun (l, c, r) x ->
        let l, c, r = match x with
          | '\r' -> l,     1,     r - 1
          | '\n' -> l + 1, 1,     r - 1
          | _    -> l,     c + 1, r - 1 in
        if r <= 1 then Stop (l, c - 1) else Continue (l, c, r)) in
  let line = List.nth (String.split_lines input) (l - 1) in
  {lineno = l; column = c; line}

let parse_c_file (filename : string) : (Cabs.file, KB.conflict) result =
  In_channel.with_file filename ~f:(fun ic ->
      let open Clexer in
      init {
        !current_handle with
        h_in_channel = ic;
        h_file_name = filename;
        h_out_channel = stderr;
      };
      init_lexicon ();
      List.iter builtin_typenames ~f:(fun {name; _} -> add_type name);
      let lexbuf = Lexing.from_function @@ get_buffer current_handle in
      match Cparser.file initial lexbuf with
      | exception _ ->
        let p = file_pos ic lexbuf in
        begin match p.line with
          | None ->
            err @@ Format.asprintf "Parser error at %s:%d:%d"
              filename p.lineno p.column
          | Some line ->
            (* Have an arrow point to the column of the offending line. *)
            let arrow = String.init p.column ~f:(fun i ->
                if i = p.column - 1 then '^' else ' ') in
            err @@ Format.asprintf "Parser error at %s:%d:%d:\n%s\n%s"
              filename p.lineno p.column line arrow
        end
      | ast -> Ok ast)

let preprocess (input : string) : (string, KB.conflict) result =
  let p = Printf.sprintf "%s{\n%s}" dummy_decl input in
  let filename = Stdlib.Filename.temp_file "vibes" ".c" in
  let* () = Files.write_or_error p filename in
  Log.send "Running the C preprocessor";
  let filename_cpp = Stdlib.Filename.temp_file "vibes-preprocessed" ".c" in
  let* _ = Proc.run "cpp" [filename; "-P"; "-o"; filename_cpp] in
  Ok filename_cpp

let parse (input : string) : (Cabs.definition, KB.conflict) result =
  let* filename = preprocess input in
  match parse_c_file filename with
  | Ok [def] -> Ok def
  | Ok [] -> err "No definitions parsed"
  | Ok _ -> err "More than one definition parsed, expected only one"
  | Error _ as err -> err
