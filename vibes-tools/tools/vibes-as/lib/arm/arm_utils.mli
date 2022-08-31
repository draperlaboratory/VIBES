open Bap.Std
open Vibes_ir.Types

(** Returns [true] if the operation is a no-op. *)
val is_nop : Operation.t -> bool

(** If the operation is an unconditional branch, then return
    the target label, if it exists. *)
val unconditional_branch_target : Operation.t -> tid option
