open OUnit2

let suite = "Full suite" >::: [
    "ARM executables" >::: Test_arm_exes.suite;
  ]

let _ = run_test_tt_main suite
