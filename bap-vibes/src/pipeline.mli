(** This module runs the VIBES pipeline. *)

open! Core_kernel

val run : Config.t -> (string, Toplevel_error.t) result
(** [run config] runs the VIBES pipeline, using the parameters/configuration
    specified in [config]. *)
