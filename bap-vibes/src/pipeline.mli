(** This module runs the VIBES pipeline. *)

open !Core_kernel

(** [run config] runs the VIBES pipeline, using the parameters/configuration
    specified in [config]. *)
val run : Config.t -> (string, Toplevel_error.t) result
