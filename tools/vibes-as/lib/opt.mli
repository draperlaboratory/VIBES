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
open Vibes_ir.Types

(** Applies generic peephole optimizations to the VIBES IR solution.
    They are parameterized by predicates that are target-specific. *)
val peephole :
  t ->
  is_nop:(Operation.t -> bool) ->
  unconditional_branch_target:(Operation.t -> tid option) ->
  is_move:(Operation.t -> bool) ->
  t
