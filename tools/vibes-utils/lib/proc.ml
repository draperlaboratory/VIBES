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

module Log = Vibes_log.Stream

type stdout_data = string list
type stderr_data = string list
type cmd_result = (stdout_data * stderr_data, KB.conflict) result
type cmd_result_error = stdout_data * stderr_data * bool

let run (command : string) (args : string list) : cmd_result =
  let env = Caml_unix.environment () in
  let cmd = String.concat (command :: args) ~sep:" " in
  Log.send "Running cmd: '%s'" cmd;
  let std_out, std_in, std_err = Caml_unix.open_process_full cmd env in
  let output = In_channel.input_lines std_out in
  Log.send "STDOUT:\n%s" @@ String.concat output ~sep:"\n";
  let error = In_channel.input_lines std_err in
  Log.send "STDERR:\n%s" @@ String.concat error ~sep:"\n";
  Log.send "Closing process channels";
  match Caml_unix.close_process_full (std_out, std_in, std_err) with
  | WEXITED 0 ->
    Log.send "Exit code: 0";
    Ok (output, error)
  | WEXITED 127 ->
    let msg = Format.sprintf "'%s' not found on path\n%!" command in
    Log.send "Exit code: 127";
    Error (Errors.Not_on_path msg)
  | WEXITED n ->
    let msg = Format.sprintf "Exited with non-zero exit code '%d'\n%!" n in
    Log.send "Exit code: '%d'" n;
    Error (Errors.Bad_exit_code msg)
  | _ ->
    Log.send "Unknown exit status";
    let msg =
      Format.sprintf 
        "Command exited with unknown status.\n \
         COMMAND:\n%s\n \
         STDOUT:\n%s\n \
         STDERR:\n%s\n"
        cmd
        (String.concat output ~sep:"\n")
        (String.concat error ~sep:"\n") in
    Error (Errors.Unknown_exit msg)

let run_with_error (command : string) (args : string list) : cmd_result_error =
  let env = Caml_unix.environment () in
  let cmd = String.concat (command :: args) ~sep:" " in
  Log.send "Running cmd: '%s'" cmd;
  let std_out, std_in, std_err = Caml_unix.open_process_full cmd env in
  let output = In_channel.input_lines std_out in
  Log.send "STDOUT:\n%s" @@ String.concat output ~sep:"\n";
  let error = In_channel.input_lines std_err in
  Log.send "STDERR:\n%s" @@ String.concat error ~sep:"\n";
  Log.send "Closing process channels";
  match Caml_unix.close_process_full (std_out, std_in, std_err) with
  | WEXITED 0 -> output, error, false
  | _ -> output, error, true
