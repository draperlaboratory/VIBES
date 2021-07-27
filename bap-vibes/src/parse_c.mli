(**
   This module implements the parser for C-like inputs.
*)

(** [parse_C_patch c_like_block] ingests a string in C-like syntax and
   uses FrontC to produce an AST. *)
val parse_c_patch : string -> (Cabs.definition, string) result
