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

(** Data observed on stdout. *)
type stdout_data = string list

(** Data observed on stderr. *)
type stderr_data = string list

(** The result of running the command. *)
type cmd_result = (stdout_data * stderr_data, KB.conflict) result

(** The result of running the command, with a boolean value indicating
    whether the process encountered an error. *)
type cmd_result_error = stdout_data * stderr_data * bool

(** [run command args] spawns a new process with command [command] and
    arguments [args], returning the output of the process if successful. *)
val run : string -> string list -> cmd_result

(** Same as [run], but allows inspection of stdout and stderr regardless
    of whether the process failed or not. *)
val run_with_error : string -> string list -> cmd_result_error
