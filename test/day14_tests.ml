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

let test_can_tilt_first_row _ =
    let row =
        Day14.parse_platform sample_input
        |> List.rotate_ccw
        |> List.hd in
    let tilted = Day14.tilt_row row in
    assert_equal tilted [Empty; Cube; Round; Empty; Empty; Cube; Round; Empty; Empty; Empty];;

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
        "can tilt first row" >:: test_can_tilt_first_row;
        (* "test_can_cycle_sample_input_once" >:: test_can_cycle_sample_input_once; *)
        (* "can solve part two with sample input" >:: test_can_solve_part_two_with_sample_input; *)
    ]
