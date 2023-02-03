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

type KB.conflict +=
   | Not_on_path of string
   | Bad_exit_code of string
   | Unknown_exit of string
   | Failed_file_read of string
   | Failed_file_write of string
   | Json_parse_error of string 
   | Json_deserialization_error of string
   | Unknown_target of string
   | Unknown_language of string
   | Invalid_binary of string

let printer (e : KB.conflict) : string option =
  match e with
  | Not_on_path s -> Some s
  | Bad_exit_code s -> Some s
  | Unknown_exit s -> Some s
  | Failed_file_read s -> Some s
  | Failed_file_write s -> Some s
  | Json_parse_error s -> Some s
  | Json_deserialization_error s -> Some s
  | Unknown_target s -> Some s
  | Unknown_language s -> Some s
  | Invalid_binary s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
