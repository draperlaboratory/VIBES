open OUnit2

let suite = "Full suite" >::: [
    "Patch_ingester" >::: Test_patch_ingester.suite;
    "Compiler" >::: Test_compiler.suite;
    "Ir" >::: Test_ir.suite;
    "Minizinc" >::: Test_minizinc.suite;
    "Patcher" >::: Test_patcher.suite;
    "Verifier" >::: Test_verifier.suite;
    "Arm_selector" >::: Test_arm_selector.suite;
    "Bir_opt" >::: Test_bir_opt.suite;
    "C Parser" >::: Test_parse_c.suite;
  ]

let _ = run_test_tt_main suite
