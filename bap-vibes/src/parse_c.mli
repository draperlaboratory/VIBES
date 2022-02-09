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

(**
   This module implements the parser for C-like inputs.
*)

(** [parse_C_patch c_like_block] ingests a string in C-like syntax and
   uses FrontC to produce an AST. *)
val parse_c_patch : string -> (Cabs.definition, string) result
