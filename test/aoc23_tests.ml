open OUnit2

let tests =
	"Test" >::: [
		Day01_tests.tests;
		Day02_tests.tests;
		Day03_tests.tests;
		Day04_tests.tests;
		Day05_tests.tests;
		Day06_tests.tests;
		Day07_tests.tests;
		Day08_tests.tests;
		Day09_tests.tests;
		Day10_tests.tests;
		Day11_tests.tests;
		Day12_tests.tests;
		Day13_tests.tests;
		Day14_tests.tests;
	]

let _ = run_test_tt_main tests
