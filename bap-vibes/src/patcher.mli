(** Creates the patched executable.

    This module is responsible for taking the assembly-like instructions
    produced by the {!Compiler}, converting them into binary instructions,
    and then splicing that binary code into (a copy of) the original
    executable, thereby producing a new, patched executable. *)

open Bap_knowledge
open Bap_core_theory
module KB = Knowledge

type patch = {
    assembly : string list;
    orig_loc : int64;
    orig_size : int64;
  }

type patch_site = {
    location : int64;
    size : int64
  }

(** A [placed_patch] is a patch that has a chosen location to place it in the
    binary. It optionally may have a jump placed after it. *)
type placed_patch = {
    assembly : string list;
    orig_loc : int64;
    orig_size : int64;
    patch_loc : int64;
    jmp : int64 option
  }

(** [patch ~patcher obj] uses the [patcher] function to patch the original
    executable associated with the provided [obj]. *)
val patch :
  ?patcher:(Theory.language -> string -> placed_patch list -> string) ->
  Data.t ->
  unit KB.t

val place_patches :
  Theory.language ->
  patch list ->
  patch_site list ->
  placed_patch list
