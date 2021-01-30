open OUnit2

let suite = "Full suite" >::: [
    "Dummy" >::: Test_dummy.suite;
  ]

let _ = run_test_tt_main suite
