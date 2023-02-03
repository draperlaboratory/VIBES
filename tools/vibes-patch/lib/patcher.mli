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

(** A placed patch. *)
type patch = {
  data       : string;
  addr       : int64;
  loc        : int64;
  inline     : int;
  root       : int64;
  trampoline : bool;
}

(** The result. *)
type t = {
  patches  : patch list;
  spaces   : Vibes_patch_info.Types.Spaces.t;
  new_ogre : Ogre.doc option;
}

(** [patch target language asms ~binary ~patched_binary ~backend ?spaces ?ogre]
    will assemble and place a list of patches in the original [binary], whose
    contents are written to the file at path [patched_binary].

    The result is the list of patches applied to the binary, as well as the
    resulting list of external spaces that are now occupied.

    The if the optional [ogre] specification is supplied, then a new
    specification will be returned if patching is successful, which shall
    contain extra information about patch spaces that were used.
*)
val patch :
  ?ogre:Ogre.doc option ->
  ?patch_spaces:Vibes_patch_info.Types.Spaces.t ->
  Theory.target ->
  Theory.language ->
  Vibes_as.Types.Assembly.t list ->
  binary:string ->
  patched_binary:string ->
  (t, KB.conflict) result
