(* This module runs the VIBES pipeline. *)

open !Core_kernel
open Bap_knowledge
module KB = Knowledge

(* [run config] runs the VIBES pipeline, using the parameters/configuration
   specified in [config]. *)
val run : Config.t -> (string, KB.Conflict.t) result
