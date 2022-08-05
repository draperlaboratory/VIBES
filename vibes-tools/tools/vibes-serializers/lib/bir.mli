open Core
open Bap.Std
open Bap_core_theory

(** Serializes a single block. *)
val serialize : blk term -> Sexp.t

(** Deserializes a list of blocks. It is assumed that these
    blocks are connected as part of a single program. *)
val deserialize : Sexp.t list -> blk term list KB.t
