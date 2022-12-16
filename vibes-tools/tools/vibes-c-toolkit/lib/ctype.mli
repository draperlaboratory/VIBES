open Bap_c.Std

(** Lookup for named types. *)
type gamma = string -> C.Type.t

(** Lookup for structures and unions. *)
type tag = {
  lookup : 'a. (string -> 'a list -> C.Type.t) -> string -> C.Type.t;
}

(** Converts a FrontC type to a BAP C type. *)
val ctype : gamma -> tag -> Cabs.base_type -> C.Type.t

(** Conversion back to the FrontC representation. *)
module To_cabs : sig

  (** Convert a BAP C type to a FrontC type. *)
  val go : C.Type.t -> Cabs.base_type

end
