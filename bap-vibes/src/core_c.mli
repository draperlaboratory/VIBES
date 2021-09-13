(**
   This module implements the Theory.Core term generator for C-like inputs.
*)

open Bap_core_theory

(** [declare_call dst] tags [dst] as being a function call with bells and whistles. *)
val declare_call : Theory.label -> unit KB.t

(** [is_call dst] returns true if the label is indented to be the
    destination of a "call-like" instruction, i.e. respecting the
    target ABI for C function calls.  *)
val is_call : Theory.label -> bool KB.t

module Eval(T : Theory.Core) : sig
  (** [c_patch_to_sexp_eff tgt code] ingests a C AST produced using FrontC and
      outputs a [unit eff] representing the semantics of the code.

      Some restrictions on the input:
      - `int x, y, z;` at the top of your patch to declare variables `x y z` (as machine words)
      - No local scopes
      - No structured loops (for the moment)
      - `(0xdead)();` gets translated into a goto statement
      - Supports operators +, -, *,, /, = <<, ++, !=, <, >, <=, >=

      Here is an example of supported C:

      {[
      int x, y, z;
      x = 0x7;
      x = *y;
      if(x > 0){
        y = z;
      } else {
        (0xdead)();
      }
      ]}

      If a construct (as parsed by FrontC) is not supported,
      [failwith] is called and crashes the program.
  *)
  val c_patch_to_eff : Theory.target -> Cabs.definition -> unit Theory.eff
end
