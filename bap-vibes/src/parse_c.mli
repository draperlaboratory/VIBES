(**
   This module implements the Theory.Core term generator for C-like inputs.
*)

open Bap_core_theory

(** [parse_C_patch c_like_block] ingests a string in C-like syntax and
   uses FrontC to produce an AST. *)
val parse_c_patch : string -> (Cabs.definition, string) result

module Eval(T : Theory.Core) : sig
  (** [c_patch_to_sexp_eff tgt code] ingests a C AST produced using FrontC and
      outputs a [unit eff] representing the semantics of the code.

      Some restrictions on the input:
      - `int x, y, z;` at the top of your patch to convert to `(var-decls x y z)`
      -  Only if then elses of the form `if(cond_expr){goto l1;}else{goto l2;};` ->
        `(branch sexp_of_cond_expr (goto l1) (goto l2))`
      - `goto foo;` -> `(goto foo)`
      - `goto fallthrough;` -> `fallthrough`
      - Supports operators +, -, *,, /, = <<, ++, !=, <, >, <=, >=
      - Dereferencing `*a` -> `(load a)`
      - Array indexing `a[j]` -> `(load (+ a j))`
      - Constants like 0x7 are maintained as unadultered strings

      Here is an example of supported C:

      ```
      int x, y, z;
      x = 0x7;
      x = *y;
      if(x > 0){
        goto fred;
      } else{
        goto larry;
      }
      ```

      If a construct (as parsed by FrontC) is not supported,
      [failwith] is called and crashes the program.
  *)
  val c_patch_to_eff : Theory.target -> Cabs.definition -> unit Theory.eff
end
