open Core
open Bap_core_theory

let to_sexp
  (data : string)
  ~(error : string -> KB.Conflict.t) : (Sexp.t list, KB.Conflict.t) result =
  let lexbuf = Stdlib.Lexing.from_string data in
  try Ok (Sexp.scan_sexps lexbuf)
  with Failure s -> Error (error s)
