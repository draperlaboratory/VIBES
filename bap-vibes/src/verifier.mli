(** Verifies the patched executable.

    This module is responsible for comparing the patched executable (produced
    by the {!Patcher}) against the original executable, to determine if the
    patched executable is correct.

    This module checks correctness by using CBAT to confirm whether a
    correctness property specified by the user holds of the patched executable
    relative to the original executable. *)

open !Core_kernel
open Bap.Std
open Bap_wp

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

(** A [verifier] function takes two subroutines and a correctness property,
    it verifies their correctness, and it returns a {!result}. *)
type verifier = sub term -> sub term -> Sexp.t -> result

(** A [printer] function takes a [result] and prints it. *)
type printer = result -> unit

(** Indicates whether the patching is done, or should be attempted again. *)
type next_step = Done | Again

(** [verify orig_prog patch_prog func property ~verifier ~printer] uses the
    [verifier] to verify that the [func] in the [orig_prog] and [patch_prog]
    satisfies the provided [property]. The [printer] is used to print the
    verifier's results. *) 
val verify : ?verifier:(verifier) -> ?printer:(printer) ->
  orig_prog:Program.t -> patch_prog:Program.t ->
  string -> Sexp.t ->
  (next_step, Toplevel_error.t) Core_kernel.result
