(* Verifies the patched executable.

   This module is responsible for comparing the patched executable (produced
   by the {!Patcher}) against the original executable, to determine if the
   patched executable is correct.

   This module checks correctness by using CBAT to confirm whether a
   correctness property specified by the user holds of the patched executable
   relative to the original executable. *)

open !Core_kernel
open Bap.Std
open Bap_knowledge
open Bap_wp

module KB = Knowledge

(* A [result] record that a [verifier] function can return. *)
type result = {
  status : Z3.Solver.status;
  solver : Z3.Solver.solver;
  precond : Constraint.t;
  orig_env : Environment.t;
  patch_env : Environment.t;
  orig_sub : Sub.t;
  patch_sub : Sub.t;
}

(* A [verifier] function takes two projects, the name of a function,
   and a correctness property, it verifies their correctness, and
   returns the [result]. *)
type verifier = sub term -> sub term -> Sexp.t -> result

(* Indicates whether the patching is done, or should be attempted again. *)
type next_step = Done | Again of Sexp.t

(* [verify obj ~loader ~verifier] uses the specified [~loader] and [~verifier]
   to load the exes and verify whether the patched executable associated with
   [obj] is correct. If so, this function returns [Done]. If not, it returns
   [Again property], to indicate that the VIBES pipeline/CEGIS loop should
   try again with the new correctness [property]. *)
val verify : ?loader:(Exe_ingester.loader) -> ?verifier:(verifier) ->
  Data.t -> next_step KB.t
