open Core

module Err = Vibes_error_lib.Std

let to_sexp ~(error : string -> Err.t) (data : string)
    : (Sexp.t list, Err.t) result =
  let lexbuf = Stdlib.Lexing.from_string data in
  try Ok (Sexp.scan_sexps lexbuf)
  with Failure s -> Error (error s)
