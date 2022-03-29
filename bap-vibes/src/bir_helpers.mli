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

(** Miscellaneous helper functions for BIR. *)

open Core_kernel
open Bap.Std
open Bap_core_theory

(** This attribute is set for defs that are for preserving/restoring
    caller-save registers. *)
val spill_tag : unit tag

(** Create a dummy subroutine from a list of blocks. *)
val create_sub : blk term list -> sub term KB.t

(** Returns [true] if the block is an implicit exit (has no jumps). *)
val is_implicit_exit : blk term -> bool

(** Returns the exit nodes from the program:

    - no jmps:
      this is implicitly an exit block since it has no successors
      in the CFG.

    - [goto <indirect>]:
      this block is explicitly jumping out of the program
      to somewhere else in the binary.

    - [call <x> with noreturn]:
      this block must implicitly return to an exit block.

    - [call <x> with return <indirect>]:
      this block will not be returning to the patch code.
*)
val exit_blks : blk term list -> blk term list KB.t

(** Returns [true] if the block contains a call to a subroutine. *)
val has_call : blk term -> bool

(** Returns the list of tids that contain calls. *)
val call_blks : blk term list -> tid list

(** Returns [true] if the jump is unconditional. *)
val is_unconditional : jmp term -> bool
