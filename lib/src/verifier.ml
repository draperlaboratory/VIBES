(* Implements {!Verifier}. *)

open !Core_kernel
open Bap.Std
open Bap_knowledge
open Knowledge.Syntax
open Bap_wp

module KB = Knowledge

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

(* The type for a verifier used by the [verify] function. *)
type verifier = Sub.t -> Sub.t -> Sexp.t -> result

(* The type for a printer used by the [printer] function. *)
type printer = result -> unit

(* The next step the CEGIS loop should take. *)
type next_step =
  | Done
  | Again of Sexp.t

(* A verifier that uses CBAT's WP library to verify the correctness
   property of the specified function in the original/patched executables. *)
let wp_verifier (orig_sub : Sub.t) (patch_sub : Sub.t)
    (property : Sexp.t) : result =

  let z3_ctx = Environment.mk_ctx () in
  let var_gen = Environment.mk_var_gen () in

  let env_1 = Precondition.mk_env z3_ctx var_gen in
  let env_2 = Precondition.mk_env z3_ctx var_gen in
  let env_2 = Environment.set_freshen env_2 true in

  let vars_1 = Precondition.get_vars env_1 orig_sub in
  let vars_2 = Precondition.get_vars env_2 patch_sub in
  let _, env_1 = Precondition.init_vars vars_1 env_1 in
  let _, env_2 = Precondition.init_vars vars_2 env_2 in

  let smtlib_hyp = "" in
  let smtlib_post = Sexp.to_string property in
  let postconds, hyps =
    Compare.compare_subs_smtlib ~smtlib_hyp ~smtlib_post in

  let precond, _env_1, _env_2 = Compare.compare_subs
      ~postconds:[postconds] ~hyps:[hyps]
      ~original:(orig_sub, env_1) ~modified:(patch_sub, env_2) in

  let solver = Z3.Solver.mk_solver z3_ctx None in
  let status = Precondition.check solver z3_ctx precond in
  { status ; solver ; precond ; orig_env = _env_1 ; patch_env = _env_2 ;
    orig_sub ; patch_sub }

(* Prints the output of a verification. *)
let naive_printer (r : result) : unit =
  (* TO DO: Maybe use other functions from the [Output] module to get strings
     so we can push this data to our Events channel instead of stdout. *)
  Output.print_result r.solver r.status r.precond
    ~show:[]
    ~orig:(r.orig_env, r.orig_sub)
    ~modif:(r.patch_env, r.patch_sub)

(* Verifies the correctness of the patched exe relative to the original exe.
   Takes a [loader] and a [verifier], which it uses to load the exes and
   to verify their correctness.

   Returns the [next_step] that the CEGIS loop should take:

   - [Done]  Indicates that the patched exe is correct
             and the CEGIS loop is done.

   - [Again property] Indicates that the patched exe is not correct
             and the CEGIS loop should try again with the provided
             correctness property. *)
let verify
    ?loader:(loader=Exe_loader.load) ?verifier:(verifier=wp_verifier)
    ?printer:(printer=naive_printer)
    (obj : Data.t) : next_step KB.t =
  Events.(send @@ Header "Starting Verifier");

  let get_sub prog name =
    let subs = Term.enum sub_t prog in
    Seq.find_exn ~f:(fun s -> String.(Sub.name s = name)) subs
  in

  Data.Original_exe.get_prog_exn obj >>= fun orig_prog ->
  Data.Patched_exe.get_tmp_filepath_exn obj >>= fun patch_exe_filepath ->
  Events.(send @@ Info "Loading patched exe...");
  Data.Verifier.get_property_exn obj >>= fun property ->
  Data.Verifier.get_func_exn obj >>= fun func ->

  loader patch_exe_filepath >>= fun patch_proj ->
  let patch_prog = Project.program patch_proj in
  let patch_sub = get_sub patch_prog func in
  let orig_sub =  get_sub orig_prog func in

  Events.(send @@ Info "Beginning weakest-precondition analysis...");
  let result = verifier orig_sub patch_sub property in
  printer result;

  match result.status with
  | Z3.Solver.UNSATISFIABLE ->
    Events.(send @@
            Info "Weakest-precondition analysis returned: correct");
    Events.(send @@ Info "The patched binary is correct");
    KB.return Done
  | Z3.Solver.SATISFIABLE ->
    Events.(send @@
            Info "Weakest-precondition analysis returned: incorrect");
    Events.(send @@ Info "The patched binary is not correct");
    (* We will just fail for now, but in the future we will return [Again]
       and let the CEGIS loop try again. *)
    let msg = "Halting for now." in
    Errors.fail (Errors.Other msg)
  | Z3.Solver.UNKNOWN ->
    let msg = "Weakest-precondition analysis returned: unknown" in
    Events.(send @@ Info msg);
    Events.(send @@ Info "Unable to determine correctness of patched exe");
    Errors.fail (Errors.WP_result_unknown msg)
