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

open Bap_core_theory

(** [build_constraints_file constraints ~model_filepath] creates a
    temporary file containing the extra constraints based on the
    existing model at [model_filepath]. The path to the temporary
    file is returned. *)
val build_constraints_file :
  string ->
  model_filepath:string ->
  string

(** [run_minizinc params ~model_filepath ?solver] runs the MiniZinc
    process on the parameters [params] according to the model at
    [model_filepath]. An alternative [solver] may be provided.

    On success, the filepath to the solution (a JSON file) will be
    returned.
*)
val run_minizinc :
  ?solver:string ->
  Yojson.Safe.t ->
  model_filepath:string ->
  (string, KB.conflict) result
