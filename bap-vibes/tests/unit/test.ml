(***************************************************************************)
(*                                                                         *)
(*  Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.      *)
(*                                                                         *)
(*  This file is provided under the license found in the LICENSE file in   *)
(*  the top-level directory of this project.                               *)
(*                                                                         *)
(*  This research was developed with funding from the Defense Advanced     *)
(*  Research Projects Agency (DARPA).                                      *)
(*                                                                         *)
(***************************************************************************)

open OUnit2

let suite () =
  "Full suite" >::: [
    "Patch_ingester" >::: Test_patch_ingester.suite;
    "Compiler" >::: Test_compiler.suite;
    "Ir" >::: Test_ir.suite;
    "Minizinc" >::: Test_minizinc.suite;
    "Patcher" >::: Test_patcher.suite;
    "Verifier" >::: Test_verifier.suite;
    "Arm_selector" >::: Test_arm_selector.suite;
    "Bir_opt" >::: Test_bir_opt.suite;
    "C Parser" >::: Test_parse_c.suite;
    "Linear SSA" >::: Test_linear_ssa.suite;
    "Substituter" >::: Test_substituter.suite;
  ]

let _ =
  match Bap_main.init () with
  | Error err ->
    Format.eprintf "Failed to initialize BAP: %a@\n%!"
      Bap_main.Extension.Error.pp err;
    exit 1;
  | Ok () ->
    run_test_tt_main @@ suite ()
