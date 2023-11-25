open OUnit2

let tests =
  "Test" >::: [
    Day01_tests.tests;
  ]

let _ = run_test_tt_main tests
