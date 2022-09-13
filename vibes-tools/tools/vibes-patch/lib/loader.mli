open Bap.Std
open Bap_core_theory

(** [image filepath ?backend] attemps to load the raw binary at
    [filepath]. An optional loader [backend] can be provided. *)
val image : ?backend:string -> string -> (image, KB.conflict) result
