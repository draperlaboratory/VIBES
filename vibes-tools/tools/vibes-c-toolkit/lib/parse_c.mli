(** [parse input] takes a single block of C source code and parses
    it into a single function definition if it is well-formed. *)
val parse : string -> (Cabs.definition, string) result
