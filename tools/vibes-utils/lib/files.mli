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

(** [get_lines filename] returns the lines in file [filename]. *)
val get_lines : string -> string list

(** Same as [get_lines], but returns an error if [get_lines] raises
    an exception. *)
val get_lines_or_error : string -> (string list, KB.conflict) result

(** [get_file_contents_non_empty filename ~error] returns the raw
    contents of the file [filename]. If the contents are empty,
    then an [Error] is returned according to [error] *)
val get_file_contents_non_empty :
  string -> 
  error:(string -> KB.conflict) ->
  (string, KB.conflict) result

(** Returns the raw file contents or an [Error]. *)
val get_file_contents : string -> (string, KB.conflict) result

(** [write data filename] writes [data] to the file [filename]. *)
val write : string -> string -> unit

(** Same as [write_data], but returns an error if [write] raises
    an exception. *)
val write_or_error : string -> string -> (unit, KB.conflict) result
