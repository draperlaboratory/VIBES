(* Implements {!Verifier}. *)

open !Core_kernel
open Bap.Std
open Bap_wp
open Bap_core_theory

(* A result record that a verifier can return. *)
type result = {
  status : Z3.Solver.status;
  solver : Z3.Solver.solver;
  precond : Constraint.t;
  orig_env : Environment.t;
  patch_env : Environment.t;
  orig_sub : Sub.t;
  patch_sub : Sub.t;
}

let (let*) x f = Result.bind x ~f

(* The type for a verifier used by the [verify] function. *)
type verifier = Theory.target -> Sub.t -> Sub.t -> Sexp.t -> result

(* The type for a printer used by the [printer] function. *)
type printer = result -> unit

(* The next step the CEGIS loop should take. *)
type next_step =
  | Done
  | Again

(* A verifier that uses CBAT's WP library to verify the correctness
   property of the specified function in the original/patched executables. *)
let wp_verifier (tgt : Theory.target) (orig_sub : Sub.t) (patch_sub : Sub.t)
    (property : Sexp.t) : result =

  let z3_ctx = Environment.mk_ctx () in
  let var_gen = Environment.mk_var_gen () in

  let env_1 = Precondition.mk_env ~target:tgt z3_ctx var_gen in
  let env_2 = Precondition.mk_env ~target:tgt z3_ctx var_gen in
  let env_2 = Environment.set_freshen env_2 true in

  let vars_1 = Precondition.get_vars env_1 orig_sub in
  let vars_2 = Precondition.get_vars env_2 patch_sub in
  let _, env_1 = Precondition.init_vars vars_1 env_1 in
  let _, env_2 = Precondition.init_vars vars_2 env_2 in

  let smtlib_hyp = "" in
  let smtlib_post = Sexp.to_string property in
  let postconds, hyps =
    Compare.compare_subs_smtlib ~smtlib_hyp ~smtlib_post in

  let precond, env_1, env_2 = Compare.compare_subs
      ~postconds:[postconds] ~hyps:[hyps]
      ~original:(orig_sub, env_1) ~modified:(patch_sub, env_2) in

  let solver = Z3.Solver.mk_solver z3_ctx None in
  let status = Precondition.check solver z3_ctx precond in
  { status; solver; precond;
    orig_env = env_1; patch_env = env_2;
    orig_sub; patch_sub }

(* Prints the output of a verification. *)
let naive_printer (r : result) : unit =
  (* TODO: Use new wp [Output] functionality to send output to [Events]. *)
  Output.print_result r.solver r.status r.precond
    ~show:[]
    ~orig:(r.orig_env, r.orig_sub)
    ~modif:(r.patch_env, r.patch_sub)

(** Verifies the correctness of the patched exe relative to the original exe.
    Takes a [verifier] and a [printer], which it uses to actually verify
    the exe and print the results.

    This function returns the [next_step] that the CEGIS loop should take:

    - [Done]  Indicates that the patched exe is correct
              and the CEGIS loop is done.

   -  [Again] Indicates that the patched exe is not correct
              and the CEGIS loop should try again. *)
let verify ?verifier:(verifier=wp_verifier) ?printer:(printer=naive_printer)
    ~orig_prog:(orig_prog : Program.t) ~patch_prog:(patch_prog : Program.t)
    (tgt : Theory.target) ~func:(func : string) (property : Sexp.t)
    : (next_step, Toplevel_error.t) Core_kernel.result =
  Events.(send @@ Header "Starting Verifier");

  Events.(send @@ Info "Beginning weakest-precondition analysis...");
  let* orig_sub = Result.of_option (Utils.get_func orig_prog func)
                    ~error:(Toplevel_error.Missing_func_orig func) in
  let* patch_sub = Result.of_option (Utils.get_func patch_prog func)
                     ~error:(Toplevel_error.Missing_func_patched func) in
  let result = verifier tgt orig_sub patch_sub property in
  printer result;

  match result.status with
  | Z3.Solver.UNSATISFIABLE ->
    Events.(send @@
            Info "Weakest-precondition analysis returned: correct");
    Events.(send @@ Info "The patched binary is correct");
    Ok Done
  | Z3.Solver.SATISFIABLE ->
    Events.(send @@
            Info "Weakest-precondition analysis returned: incorrect");
    Events.(send @@ Info "The patched binary is not correct");
    Events.(send @@ Info "Trying again");
    Ok Again
  | Z3.Solver.UNKNOWN ->
    let msg = "Weakest-precondition analysis returned: unknown" in
    Events.(send @@ Info msg);
    Events.(send @@ Info "Unable to determine correctness of patched exe");
    Error (Toplevel_error.WP_result_unknown msg)
