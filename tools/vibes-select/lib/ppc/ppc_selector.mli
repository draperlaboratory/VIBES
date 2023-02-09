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

open Bap.Std
open Bap_core_theory

(** The subpiece of the condition register (CR) that we'll be using.

    There are 8 subpieces, 4 bits each, from CR0 to CR7, most of which are
    volatile. CR7 is the least significant 4 bits.
*)
val cr_num : int

(** Runs the PowerPC instruction selector on the BIR program. *)
val select : sub term -> Vibes_ir.Types.t KB.t
