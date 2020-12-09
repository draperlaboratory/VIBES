open OUnit2

let suite = "Full suite" >::: [
  "Exe_ingester" >::: Test_exe_ingester.suite;
  "Patch_ingester" >::: Test_patch_ingester.suite;
  "Compiler" >::: Test_compiler.suite;
  "Vibes_ir" >::: Test_vibes_ir.suite;
  "Minizinc" >::: Test_minizinc.suite;
  "Patcher" >::: Test_patcher.suite;
  "Verifier" >::: Test_verifier.suite;
  "Arm_gen" >::: Test_arm.suite;
]

let _ = run_test_tt_main suite
