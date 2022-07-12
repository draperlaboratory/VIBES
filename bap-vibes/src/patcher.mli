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

(** Creates the patched executable.

    This module is responsible for taking the assembly-like instructions
    produced by the {!Compiler}, converting them into binary instructions,
    and then splicing that binary code into (a copy of) the original
    executable, thereby producing a new, patched executable. *)

open Core_kernel
open Bap_knowledge
open Bap_core_theory
module KB = Knowledge

type patch = {
  name : string;
  assembly : string list;
  orig_loc : int64;
  orig_size : int64;
}

type patch_site = {
  location : int64;
  size : int64
}

(** A [placed_patch] is a patch that has a chosen location to place it in the
    binary. It optionally may have a jump placed after it.

    - [assembly] is the actual patch code that will be fed to the system assembler.
    - [orig_loc] is the file offset at which the patch will be inserted.
    - [orig_size] is the available space for the patch at the patch point.
    - [patch_loc] is the actual location where the patch is placed. It may be equal
      to [orig_loc] if the patch was able to fit in the specified location.
    - [jmp] is the (optional) destination of a jump that was inserted at the end of
      the patch code. This is for the case where the patch was not an exact fit, uses
      a literal pool, or had to be placed elsewhere.
    - [org_offset] is the (optional) offset from an origin of 0 that the patch will
      be assembled at. This occurs when a literal pool is used by the patch code and
      the patcher anticipates an alignment correction.
 *)
type placed_patch = {
  assembly : string list;
  orig_loc : int64;
  orig_size : int64;
  patch_loc : int64;
  jmp : int64 option;
  org_offset : int option;
}

(** A [patch_region] represents the location of the (to be placed)
   patch *region* within a binary *)
type patch_region = {
  region_addr : int64;
  region_offset : int64;
}

type compute_region = Ogre.doc -> loc:int64 -> patch_region Or_error.t

type patcher =
  Theory.language ->
  filename:string ->
  placed_patch list ->
  int64 ->
  string

(** [patch ~patcher obj spec] uses the [patcher] function to patch the original
    executable associated with the provided [obj]. *)
val patch :
  ?compute_region:compute_region ->
  ?patcher:patcher ->
  Data.t ->
  Ogre.doc ->
  unit KB.t

val place_patches :
  Theory.target ->
  Theory.language ->
  int64 ->
  patch list ->
  patch_site list ->
  placed_patch list
