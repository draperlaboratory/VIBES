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

let to_sexp
  (data : string)
  ~(error : string -> KB.conflict) : (Sexp.t list, KB.conflict) result =
  let lexbuf = Stdlib.Lexing.from_string data in
  try Ok (Sexp.scan_sexps lexbuf)
  with Failure s -> Error (error s)
