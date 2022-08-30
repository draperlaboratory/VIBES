open Bap.Std
open Vibes_ir.Types

(** Helpers for instruction selection. *)
module Sel : sig

  (** Accumulates the effects of a program.

      - [data]: the data effects for the current block
        we are generating code for. They are accumulated
        in reverse order.

      - [ctrl]: same as [data], but for control flow.

      - [ir]: the accumulated VIBES IR program
  *)
  type eff = {
    data : Operation.t list;
    ctrl : Operation.t list;
    ir : t;
  }

  (** A value computed for a pure expression, with the
      intermediate effects (and accumulated IR program). *)
  type pure = {
    value : Operand.t;
    eff : eff;
  }

  (** The empty effect. *)
  val empty_eff : eff

  (** [x @. y] appends [x] and [y]. *)
  val (@.) : eff -> eff -> eff

  (** Appends a data effect. *)
  val instr : Operation.t -> eff -> eff

  (** Appends a control effect. *)
  val control : Operation.t -> eff -> eff

end

(** Helpers for preassigning temporaries to registers. *)
module Preassign : sig

  (** Transforms a variable name into a register variable. *)
  type transform = typ -> string -> var

  (** If the variable was marked as a register, then return its
      original name. *)
  val reg_name : var -> string option

  (** Runs the preassignment pass on all opvars in the IR, according
      to the transformer [f]. *)
  val run : t -> f:transform -> t
  
end
