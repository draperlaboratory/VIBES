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

open Core
open Bap_core_theory

(** Serializes the VIBES IR. *)
val serialize : Vibes_ir.Types.t -> Sexp.t

(** Deserializes the VIBES IR. *)
val deserialize : Sexp.t -> Vibes_ir.Types.t KB.t
