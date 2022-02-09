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

(** Ingests the patch code.

    This module is responsible for taking the patch code provided by the
    user, and loading that (possibly lifting it) to BIR. *)

open Bap_knowledge
module KB = Knowledge

(** Processes the whole patch associated with the [Data.t] argument,
    populating all the relevant KB slots with semantic data associated
    with the patch syntax. *)
val ingest : Data.t -> unit KB.t
