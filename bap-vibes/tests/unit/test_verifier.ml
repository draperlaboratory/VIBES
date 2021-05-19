open Bap_vibes
open OUnit2
module H = Helpers

(* Some dummy values to use in the tests below. *)
let orig_proj = H.dummy_proj "orig_exe" ~name:H.func

let orig_prog = H.prog_exn orig_proj

let patch_proj = H.dummy_proj "patched_exe" ~name:H.func

let patch_prog = H.prog_exn patch_proj

let tgt = Bap_core_theory.Theory.Target.unknown

(* A helper to print the results. *)
let res_str r : string =
  match r with
  | Ok Verifier.Done -> "[Ok Verifier.Done]"
  | Ok Verifier.Again -> "[Ok Verifier.Again]"
  | Error (Toplevel_error.WP_result_unknown _) ->
      "[Error (Toplevel_error.WP_result_unknown)]"
  | Error e -> Format.asprintf "[Error (%a)]" Toplevel_error.pp e

(* Test that the verifier works as expected when [WP] returns [UNSAT]. *)
let test_verify_unsat (_ : test_ctxt) : unit =
  let result =
    Verifier.verify tgt ~func:H.func H.property ~orig_prog ~patch_prog
      ~verifier:H.verify_unsat
  in
  let expected = Ok Verifier.Done in
  let msg =
    Format.sprintf "Expected [Ok Verifier.Done], but got %s" (res_str result)
  in
  assert_bool msg (result = expected)

(* Test that the verifier works as expected when [WP] returns [SAT]. *)
let test_verify_sat (_ : test_ctxt) : unit =
  let result =
    Verifier.verify tgt ~func:H.func H.property ~orig_prog ~patch_prog
      ~verifier:H.verify_sat
  in
  let expected = Ok Verifier.Again in
  let msg =
    Format.sprintf "Expected [Ok Verifier.Again], but got %s" (res_str result)
  in
  assert_bool msg (result = expected)

(* Test that the verifier works as expected when [WP] returns [UNKNOWN]. *)
let test_verify_unknown (_ : test_ctxt) : unit =
  let result =
    Verifier.verify tgt ~func:H.func H.property ~orig_prog ~patch_prog
      ~verifier:H.verify_unknown
  in
  let msg =
    Format.sprintf
      "Expected [Error Toplevel_error.WP_result_unknown], but got %s"
      (res_str result)
  in
  match result with
  | Error (Toplevel_error.WP_result_unknown _) -> assert_bool msg true
  | _ -> assert_bool msg false

let suite =
  [
    "Test Verifier.verify: UNSAT" >:: test_verify_unsat;
    "Test Verifier.verify: SAT" >:: test_verify_sat;
    "Test Verifier.verify: UNKNOWN" >:: test_verify_unknown;
  ]
