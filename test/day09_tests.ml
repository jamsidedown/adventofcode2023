open OUnit2
open Aoc23

let first_sample = [0; 3; 6; 9; 12; 15];;
let second_sample = [1; 3; 6; 10; 15; 21];;
let third_sample = [10; 13; 16; 21; 30; 45];;

let sample_input = [
    "0 3 6 9 12 15";
    "1 3 6 10 15 21";
    "10 13 16 21 30 45"
];;

let test_can_get_differences_in_first_sample _ =
    let differences = Day09.get_differences first_sample in
    assert_equal differences [3; 3; 3; 3; 3];;

let test_can_get_differences_in_second_sample _ =
    let differences = Day09.get_differences second_sample in
    assert_equal differences [2; 3; 4; 5; 6];;

let test_can_extrapolate_first_sample _ =
    let result = Day09.extrapolate first_sample in
    assert_equal result 18;;

let test_can_extrapolate_second_sample _ =
    let result = Day09.extrapolate second_sample in
    assert_equal result 28;;

let test_can_extrapolate_third_sample _ =
    let result = Day09.extrapolate third_sample in
    assert_equal result 68;;

let test_can_solve_part_one _ =
    let result = Day09.part_one sample_input in
    assert_equal result 114;;

let test_can_extrapolate_third_sample_backwards _ =
    let input = List.rev third_sample in
    let result = Day09.extrapolate input in
    assert_equal result 5;;

let tests =
    "day 9" >::: [
        "can get differences in first sample" >:: test_can_get_differences_in_first_sample;
        "can get differences in second sample" >:: test_can_get_differences_in_second_sample;
        "can extrapolate first sample" >:: test_can_extrapolate_first_sample;
        "can extrapolate second sample" >:: test_can_extrapolate_second_sample;
        "can extrapolate third sample" >:: test_can_extrapolate_third_sample;
        "can solve part one" >:: test_can_solve_part_one;
        "can extrapolate backwards for third sample" >::test_can_extrapolate_third_sample_backwards;
    ]
