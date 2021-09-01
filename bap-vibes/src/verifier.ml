(* Implements {!Verifier}. *)

open !Core_kernel
open Bap.Std
open Bap_core_theory

module Params = Bap_wp.Run_parameters
module Runner = Bap_wp.Runner

type status = Z3.Solver.status

let (let*) x f = Result.bind x ~f


type verifier =
  Params.t
  -> Runner.input list
  -> (status, Bap_main.error) result

(* The next step the CEGIS loop should take. *)
type next_step =
  | Done
  | Again

(* A verifier that uses CBAT's WP library to verify the correctness
   property of the specified function in the original/patched executables. *)
let wp_verifier (p : Params.t) inputs =
  Runner.run p inputs

(** Verifies the correctness of the patched exe relative to the original exe.
    Takes a [verifier] and a [printer], which it uses to actually verify
    the exe and print the results.

    This function returns the [next_step] that the CEGIS loop should take:

    - [Done]  Indicates that the patched exe is correct
              and the CEGIS loop is done.

   -  [Again] Indicates that the patched exe is not correct
              and the CEGIS loop should try again. *)
let verify ?verifier:(verifier=wp_verifier)
    ~orig_prog:(orig_prog : Program.t * string)
    ~patch_prog:(patch_prog : Program.t * string)
    (tgt : Theory.target) ~func:(func : string) (property : Sexp.t)
    : (next_step, Toplevel_error.t) result =
  Events.(send @@ Header "Starting Verifier");

  Events.(send @@ Info "Beginning weakest-precondition analysis...");
  let* func =
    Result.(
      func |>
      Utils.get_func (fst orig_prog) |>
      Result.of_option ~error:(Toplevel_error.Missing_func_orig func) >>|
      Sub.name)
  in
  let param = Params.default ~func:func in
  let post = Sexp.to_string property in
  let param =
    {
      param with
      postcond = post;
      ext_solver_path = Some "boolector"
    }
  in
  let (prog1, name1) = orig_prog in
  let (prog2, name2) = patch_prog in
  let input1 = Runner.{program = prog1; target = tgt; filename = name1} in
  let input2 = Runner.{program = prog2; target = tgt; filename = name2} in
  let* status =
    verifier param
      [input1; input2]
  |> Result.map_error ~f:(fun e -> Toplevel_error.WP_failure e)
  in
  match status with
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
