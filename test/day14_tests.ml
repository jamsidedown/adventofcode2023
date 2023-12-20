open OUnit2
open Aoc23

let sample_input = [
    "O....#....";
    "O.OO#....#";
    ".....##...";
    "OO.#O....O";
    ".O.....O#.";
    "O.#..O.#.#";
    "..O..#O..O";
    ".......O..";
    "#....###..";
    "#OO..#...."
]

let test_can_solve_part_one_with_sample_input _ =
    let result = Day14.part_one sample_input in
    assert_equal result 136;;

let test_can_cycle_sample_input_once _ =
    let platform = Day14.parse_platform sample_input in
    let rotated = List.rotate_ccw platform in
    let cycled = Day14.cycle rotated in
    let rerotated = List.rotate_cw cycled in
    Day14.print_platform rerotated;;

let test_can_solve_part_two_with_sample_input _ =
    let result = Day14.part_two sample_input in
    assert_equal result 64;;

let tests =
    "day 14" >::: [
        "can solve part one with sample input" >:: test_can_solve_part_one_with_sample_input;
        (* "test_can_cycle_sample_input_once" >:: test_can_cycle_sample_input_once; *)
        "can solve part two with sample input" >:: test_can_solve_part_two_with_sample_input;
    ]
