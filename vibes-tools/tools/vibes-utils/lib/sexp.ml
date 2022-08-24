open Core
open Bap_core_theory

let to_sexp
  (data : string)
  ~(error : string -> KB.conflict) : (Sexp.t list, KB.conflict) result =
  let lexbuf = Stdlib.Lexing.from_string data in
  try Ok (Sexp.scan_sexps lexbuf)
  with Failure s -> Error (error s)
