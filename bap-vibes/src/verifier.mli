(** Verifies the patched executable.

    This module is responsible for comparing the patched executable (produced
    by the {!Patcher}) against the original executable, to determine if the
    patched executable is correct.

    This module checks correctness by using CBAT to confirm whether a
    correctness property specified by the user holds of the patched executable
    relative to the original executable. *)

open !Core_kernel

(** A [result] is the result of calling WP. *)
type result = (string, Toplevel_error.t) Result.t

(** A [verifier] function takes the filepath to the original program, the
    patched program, a correctness property, and the name of a function.
    It then verifies the correctness of the function in those two programs,
    and it returns the {!result}. *)
type verifier = orig_exe_filepath:string -> patched_exe_filepath:string ->
  property:Sexp.t -> string -> result

(** A [printer] function takes a [result] and prints it. *)
type printer = result -> unit

(** Indicates whether the patching is done, or should be attempted again. *)
type next_step = Done | Again

(** [verify orig_prog patch_prog func property ~verifier ~printer] uses the
    [verifier] to verify that the [func] in the [orig_prog] and [patch_prog]
    satisfies the provided [property]. The [printer] is used to print the
    verifier's results. *)
val verify :
  ?verifier:(verifier) ->
  ?printer:(printer) ->
  orig_exe_filepath:string ->
  patched_exe_filepath:string ->
  property:Sexp.t ->
  string ->
  (next_step, Toplevel_error.t) Core_kernel.result
