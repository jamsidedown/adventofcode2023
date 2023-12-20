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

let tests =
    "day 14" >::: [
        "can solve part one with sample input" >:: test_can_solve_part_one_with_sample_input;
    ]
