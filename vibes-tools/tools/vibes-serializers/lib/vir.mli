open Core
open Bap_core_theory

(** Serializes the VIBES IR. *)
val serialize : Vibes_ir.Types.t -> Sexp.t

(** Deserializes the VIBES IR. *)
val deserialize : Sexp.t -> Vibes_ir.Types.t KB.t
