open OUnit2
open Aoc23

let sample_input = [
    ".|...\\....";
    "|.-.\\.....";
    ".....|-...";
    "........|.";
    "..........";
    ".........\\";
    "..../.\\\\..";
    ".-.-/..|..";
    ".|....-|.\\";
    "..//.|...."
]

let test_can_solve_part_one_with_sample_input _ =
    let result = Day16.part_one sample_input in
    assert_equal result 46;;

let test_can_solve_part_two_with_sample_input _ =
    let result = Day16.part_two sample_input in
    assert_equal result 51;;

let tests =
    "day 16" >::: [
        "can solve part one for sample input" >:: test_can_solve_part_one_with_sample_input;
        "can solve part two for sample input" >:: test_can_solve_part_two_with_sample_input;
    ]
