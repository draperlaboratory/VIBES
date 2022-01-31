(**
   This module implements the Theory.Core term generator for C-like inputs.
*)

open Bap_core_theory

(** [provide_args dst args] marks [args] as the subroutine arguments that
    are passed for the call to [dst]. *)
val provide_args : Theory.label -> Bap.Std.var list -> unit KB.t

(** [collect_args dst] returns the list of arguments needed by the call. *)
val collect_args : Theory.label -> Bap.Std.var list KB.t

(** [declare_call dst] marks [dst] as a subroutine. *)
val declare_call : Theory.label -> unit KB.t

(** [is_call dst] returns true if the label is indented to be the
    destination of a "call-like" instruction, i.e. respecting the
    target ABI for C function calls.  *)
val is_call : Theory.label -> bool KB.t

module Eval(T : Theory.Core) : sig
  (** [c_patch_to_sexp_eff tgt code] ingests a C AST produced using FrontC and
      outputs a [unit eff] representing the semantics of the code.

      Some restrictions on the input:
      - [int x, y, z;] at the top of your patch to declare variables [x y z] (as machine words)
      - No local scopes
      - No structured loops (for the moment)
      - [(0xdead)();] gets translated into a call statement to that address
      - [goto L_0xdead;] gets translated into a goto statement to the address after the [L_]
      - Supports operators +, -, *,, /, = <<, ++, !=, <, >, <=, >=

      Here is an example of supported C:

      {[
      int x, y, z;
      x = 0x7;
      x = *y;
      if(x > 0){
        y = z;
      } else {
        goto L_0xdead;
      }
      ]}

      If a construct (as parsed by FrontC) is not supported,
      [failwith] is called and crashes the program.
  *)
  val c_patch_to_eff :
    Higher_var.t list -> Theory.target -> Cabs.definition -> unit Theory.eff
end
