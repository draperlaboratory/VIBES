open Bap_core_theory

(** Returns [true] if the language is Thumb. *)
val is_thumb : Theory.language -> bool

(** [get_target name] returns the target with the name [name],
    if it exists. *)
val get_target : string -> (Theory.target, KB.conflict) result

(** [get_language name] returns the language with the name [name],
    if it exists. *)
val get_language : string -> (Theory.language, KB.conflict) result
