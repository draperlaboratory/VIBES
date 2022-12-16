open Bap_core_theory

(** A builtin type. *)
type builtin = {
  name   : string;
  size   : int;
  signed : bool;
}

(** A list of typenames that the C lexer is made aware of when
    parsing. *)
val builtin_typenames : builtin list

(** [parse input] takes a single block of C source code and parses
    it into a single function definition if it is well-formed. *)
val parse : string -> (Cabs.definition, KB.conflict) result
