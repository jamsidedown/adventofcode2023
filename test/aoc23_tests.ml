open OUnit2

let tests =
  "Test" >::: [
    Day01_tests.tests;
    Day02_tests.tests;
    Day03_tests.tests;
    Day04_tests.tests;
    Day05_tests.tests;
    Day06_tests.tests;
  ]

let _ = run_test_tt_main tests
