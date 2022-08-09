(** The prefix used for creating custom tags via [Bap.Std.Value]. *)
val prefix : string

(** Adds the prefix to the tag name. *)
val make : string -> string
