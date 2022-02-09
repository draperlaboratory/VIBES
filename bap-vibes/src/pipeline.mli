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

(** This module runs the VIBES pipeline. *)

open !Core_kernel

(** [run config] runs the VIBES pipeline, using the parameters/configuration
    specified in [config]. *)
val run : Config.t -> (string, Toplevel_error.t) result
