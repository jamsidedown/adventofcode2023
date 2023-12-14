open OUnit2
open Aoc23

let sample_input = [
    "???.### 1,1,3";
    ".??..??...?##. 1,1,3";
    "?#?#?#?#?#?#?#? 1,3,1,6";
    "????.#...#... 4,1,1";
    "????.######..#####. 1,6,5";
    "?###???????? 3,2,1"
];;

let test_can_parse_first_line_in_sample _ =
    let springs, groups = Day12.parse_line "???.### 1,1,3" in
    assert_equal springs [Unknown; Unknown; Unknown; Operational; Damaged; Damaged; Damaged];
    assert_equal groups [1; 1; 3];;

let test_can_match_springs _ =
    let truth = Day12.parse_springs "???.###" in
    let springs = Day12.parse_springs "#.#.###" in
    let result = Day12.matches_springs truth springs in
    assert_equal result true;;

let test_can_match_more_springs _ =
    let truth = Day12.parse_springs "?#?#?#?#?#?#?#?" in
    let springs = Day12.parse_springs ".#.###.#.######" in
    let result = Day12.matches_springs truth springs in
    assert_equal result true;;

let test_can_detect_bad_springs _ =
    let truth = Day12.parse_springs "???.###" in
    let springs = Day12.parse_springs "#.#.#.#" in
    let result = Day12.matches_springs truth springs in
    assert_equal result false;;

let test_can_match_groups _ =
    let groups = [1; 1; 3] in
    let springs = Day12.parse_springs "#.#.###" in
    let result = Day12.matches_groups groups springs in
    assert_equal result true;;

let test_can_detect_bad_groups _ =
    let groups = [1; 1; 3] in
    let springs = Day12.parse_springs "#.#.#.###" in
    let result = Day12.matches_groups groups springs in
    assert_equal result false;;

let test_can_detect_bad_groups_in_first_sample _ =
    let groups = [1; 1; 3] in
    let first_springs = Day12.parse_springs ".##.###" in
    let second_springs = Day12.parse_springs "##..###" in
    let first_result = Day12.matches_groups groups first_springs in
    let second_result = Day12.matches_groups groups second_springs in
    assert_equal first_result false;
    assert_equal second_result false;;

let test_first_sample_generates_one_arrangement _ =
    let springs, groups = Day12.parse_line "???.### 1,1,3" in
    let arrangements = Day12.generate_matches springs groups in
    let expected = Day12.parse_springs "#.#.###" :: [] in
    assert_equal arrangements expected;;

let test_second_sample_generates_four_arrangements _ =
    let springs, groups = Day12.parse_line ".??..??...?##. 1,1,3" in
    let arrangements = Day12.generate_matches springs groups in
    assert_equal (List.length arrangements) 4;;

let test_third_sample_generates_one_arrangement _ =
    let springs, groups = Day12.parse_line "?#?#?#?#?#?#?#? 1,3,1,6" in
    let arrangements = Day12.generate_matches springs groups in
    assert_equal (List.length arrangements) 1;;

let test_fourth_sample_generates_one_arrangement _ =
    let springs, groups = Day12.parse_line "????.#...#... 4,1,1" in
    let arrangements = Day12.generate_matches springs groups in
    assert_equal (List.length arrangements) 1;;

let test_fifth_sample_generates_four_arrangements _ =
    let springs, groups = Day12.parse_line "????.######..#####. 1,6,5" in
    let arrangements = Day12.generate_matches springs groups in
    assert_equal (List.length arrangements) 4;;

let test_sixth_sample_generates_ten_arrangements _ =
    let springs, groups = Day12.parse_line "?###???????? 3,2,1" in
    let arrangements = Day12.generate_matches springs groups in
    assert_equal (List.length arrangements) 10;;

let test_can_solve_part_one _ =
    let result = Day12.part_one sample_input in
    assert_equal result 21;;

let test_can_solve_part_two _ =
    let result = Day12.part_two sample_input in
    assert_equal result 525152;;

let tests =
    "day 12" >::: [
        "can parse first line in sample" >:: test_can_parse_first_line_in_sample;
        "can match springs" >:: test_can_match_springs;
        "can match more springs" >:: test_can_match_more_springs;
        "can detect bad springs" >:: test_can_detect_bad_springs;
        "can match groups" >:: test_can_match_groups;
        "can detect bad groups" >:: test_can_detect_bad_groups;
        "can detect bad groups in first sample input" >:: test_can_detect_bad_groups_in_first_sample;
        "first sample generates one arrangement" >:: test_first_sample_generates_one_arrangement;
        "second sample generates four arrangements" >:: test_second_sample_generates_four_arrangements;
        "third sample generates one arrangement" >:: test_third_sample_generates_one_arrangement;
        "fourth sample generates one arrangement" >:: test_fourth_sample_generates_one_arrangement;
        "fifth sample generates four arrangements" >:: test_fifth_sample_generates_four_arrangements;
        "sixth sample generates ten arrangements" >:: test_sixth_sample_generates_ten_arrangements;
        "can solve part one with sample input" >:: test_can_solve_part_one;
        "can solve part two with sample input" >:: test_can_solve_part_two;
    ]