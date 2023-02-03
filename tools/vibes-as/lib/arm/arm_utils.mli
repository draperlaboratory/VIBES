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

(** Returns [true] if the operation is a no-op. *)
val is_nop : Operation.t -> bool

(** If the operation is an unconditional branch, then return
    the target label, if it exists. *)
val unconditional_branch_target : Operation.t -> tid option

(** Returns [true] if the operation is a move. *)
val is_move : Operation.t -> bool
