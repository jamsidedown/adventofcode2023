open OUnit2
open Aoc23

let first_sample_input = [
    "#.##..##.";
    "..#.##.#.";
    "##......#";
    "##......#";
    "..#.##.#.";
    "..##..##.";
    "#.#.##.#."
];;

let second_sample_input = [
    "#...##..#";
    "#....#..#";
    "..##..###";
    "#####.##.";
    "#####.##.";
    "..##..###";
    "#....#..#"
];;

let test_can_find_symmetry_in_first_sample _ =
    let symmetry = Day13.find_symmetry first_sample_input in
    assert_equal symmetry 5;;

let test_can_find_symmetry_in_second_sample _ =
    let symmetry = Day13.find_symmetry second_sample_input in
    assert_equal symmetry 400;;

let test_can_find_alternate_symmetry_in_first_sample _ =
    let symmetry = Day13.find_alternate_symmetry first_sample_input in
    assert_equal symmetry 300;;

let test_can_find_alternate_symmetry_in_second_sample _ =
    let symmetry = Day13.find_alternate_symmetry second_sample_input in
    assert_equal symmetry 100;;

let tests =
    "day 13" >::: [
        "can find vertical symmetry in first sample" >:: test_can_find_symmetry_in_first_sample;
        "can find horizontal symmetry in second sample" >:: test_can_find_symmetry_in_second_sample;
        "can find alternate symmetry in first sample" >:: test_can_find_alternate_symmetry_in_first_sample;
        "can find alternate symmetry in second sample" >:: test_can_find_alternate_symmetry_in_second_sample;
    ]
