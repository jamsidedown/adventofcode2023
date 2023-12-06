open OUnit2
open Aoc23

let sample_input = [
    "Time:      7  15   30";
    "Distance:  9  40  200"
];;

let test_can_parse_time _ =
    let input = "Time:      7  15   30" in
    let numbers = Day06.parse_numbers input in
    assert_equal numbers [7; 15; 30];;

let test_can_solve_part_one_with_sample_input _ =
    let result = Day06.part_one sample_input in
    assert_equal result 288;;

let test_can_parse_single_time _ =
    let input = "Time:      7  15   30" in
    let number = Day06.parse_single_number input in
    assert_equal number (Some 71530);;

let test_can_solve_part_two_with_sample_input _ =
    let result = Day06.part_two sample_input in
    assert_equal result 71503;;

let tests =
    "day 5" >::: [
        "can parse time" >:: test_can_parse_time;
        "can solve part one for sample input" >:: test_can_solve_part_one_with_sample_input;
        "can parse single time" >:: test_can_parse_single_time;
        "can solve part two for sample input" >:: test_can_solve_part_two_with_sample_input;
    ]