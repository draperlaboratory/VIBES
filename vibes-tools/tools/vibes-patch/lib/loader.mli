open Bap.Std
open Bap_core_theory

(** [image filepath] attemps to load the raw binary at [filepath]. *)
val image : string -> (image, KB.conflict) result
