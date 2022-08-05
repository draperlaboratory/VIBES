(** [print_c pp data] applies the FrontC pretty-printer [pp]
    to [data], and returns its string representation. *)
val print_c : ('a -> unit) -> 'a -> string
