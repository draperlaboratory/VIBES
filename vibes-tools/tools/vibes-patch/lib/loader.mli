open Bap.Std
open Bap_core_theory

(** The name of the VIBES loader. *)
val name : string

(** [register data] registers the VIBES loader using the raw OGRE [data].
    Should only be called once. *)
val register : string -> unit

(** [image filepath ?backend] attemps to load the raw binary at
    [filepath]. An optional loader [backend] can be provided. *)
val image : ?backend:string -> string -> (image, KB.conflict) result
