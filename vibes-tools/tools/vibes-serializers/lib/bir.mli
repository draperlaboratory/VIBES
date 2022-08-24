open Core
open Bap.Std
open Bap_core_theory

(** Serializes a subroutine. *)
val serialize : sub term -> Sexp.t

(** Deserializes a subroutine. *)
val deserialize : Sexp.t -> sub term KB.t
