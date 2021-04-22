open Bap_vibes
open OUnit2

module H = Helpers

(* Some dummy values to use in the tests below. *)
let orig_exe_filepath = H.original_exe
let patched_exe_filepath = H.patched_exe
let func = H.func
let property = H.property

(* Some dummy functions to use in the tests below. *)
let verifier_printer _ = ()
let verify_unsat
    ~orig_exe_filepath:(_ : string) ~patched_exe_filepath:(_ : string)
    ~property:(_ : Core_kernel.Sexp.t) (_ : string) 
    : Verifier.result = Ok "UNSAT"
let verify_sat
    ~orig_exe_filepath:(_ : string) ~patched_exe_filepath:(_ : string)
    ~property:(_ : Core_kernel.Sexp.t) (_ : string) 
    : Verifier.result = Ok "SAT"
let verify_unknown
    ~orig_exe_filepath:(_ : string) ~patched_exe_filepath:(_ : string)
    ~property:(_ : Core_kernel.Sexp.t) (_ : string) 
    : Verifier.result = Ok "UNKNOWN"

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
  let result = Verifier.verify func
    ~verifier:verify_unsat ~printer:verifier_printer
    ~orig_exe_filepath ~patched_exe_filepath ~property
  in
  let expected = Ok Verifier.Done in
  let msg = Format.sprintf
    "Expected [Ok Verifier.Done], but got %s" (res_str result)
  in
  assert_bool msg (result = expected)

(* Test that the verifier works as expected when [WP] returns [SAT]. *)
let test_verify_sat (_ : test_ctxt) : unit =
  let result = Verifier.verify func
    ~verifier:verify_sat ~printer:verifier_printer
    ~orig_exe_filepath ~patched_exe_filepath ~property
  in
  let expected = Ok Verifier.Again in
  let msg = Format.sprintf
    "Expected [Ok Verifier.Again], but got %s" (res_str result)
  in
  assert_bool msg (result = expected)

(* Test that the verifier works as expected when [WP] returns [UNKNOWN]. *)
let test_verify_unknown (_ : test_ctxt) : unit =
  let result = Verifier.verify func
    ~verifier:verify_unknown ~printer:verifier_printer
    ~orig_exe_filepath ~patched_exe_filepath ~property
  in
  let msg = Format.sprintf
    "Expected [Error Toplevel_error.WP_result_unknown], but got %s"
    (res_str result)
  in
  match result with
  | Error (Toplevel_error.WP_result_unknown _) -> assert_bool msg true
  | _ -> assert_bool msg false

let suite = [
  "Test Verifier.verify: UNSAT" >:: test_verify_unsat;
  "Test Verifier.verify: SAT" >:: test_verify_sat;
  "Test Verifier.verify: UNKNOWN" >:: test_verify_unknown;
]
