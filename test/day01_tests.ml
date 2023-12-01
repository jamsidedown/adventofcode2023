open OUnit2
open Aoc23

let can_convert_chars_to_int _ =
    let lst = ['1'; '2'] in
    let converted = Int.of_chars lst in
    assert_equal converted 12;;

let can_get_first_and_last_ints _ =
    let lst = ['1'; '2'; '3'; '4'] in
    let first_and_last = Day01.first_and_last lst in
    assert_equal first_and_last (Some 14);;

let can_sum_for_part_one _ =
    let input = ["1abc2"; "pqr3stu8vwx"; "a1b2c3d4e5f"; "treb7uchet"] in
    let sum = Day01.part_one input in
    assert_equal sum 142;;

let can_parse_string_ints_from_chars _ =
    let line = "two1nine" in
    let chars = String.to_list line in
    let parsed = Day01.parse_string_ints chars in
    assert_equal parsed ['2'; '1'; '9'];;

let can_parse_edge_case _ =
    let line = "xtwone3four" in
    let chars = String.to_list line in
    let parsed = Day01.parse_string_ints chars in
    assert_equal parsed ['2'; '1'; '3'; '4'];;

let can_sum_for_part_two _ =
    let input = ["two1nine"; "eightwothree"; "abcone2threexyz"; "xtwone3four"; "4nineeightseven2"; "zoneight234"; "7pqrstsixteen"] in
    let sum = Day01.part_two input in
    assert_equal sum 281;;

let tests =
    "day 1" >::: [
        "can convert chars to an int" >:: can_convert_chars_to_int;
        "can get first can last values from a list" >:: can_get_first_and_last_ints;
        "can solve sample input for part one" >:: can_sum_for_part_one;
        "can parse written numbers from line" >:: can_parse_string_ints_from_chars;
        "can parse edge case" >:: can_parse_edge_case;
        "can solve sample input for part two" >:: can_sum_for_part_two;
    ]
