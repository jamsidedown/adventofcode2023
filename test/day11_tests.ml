open OUnit2
open Aoc23

let my_input = [
    ".#..";
    "...#";
    "....";
    ".#.#"
]

let sample_input = [
    "...#......";
    ".......#..";
    "#.........";
    "..........";
    "......#...";
    ".#........";
    ".........#";
    "..........";
    ".......#..";
    "#...#....."
]

let test_can_parse_my_galaxies _ =
    let galaxies = Day11.parse_galaxies my_input in
    assert_equal galaxies [
        {x=1; y=0};
        {x=3; y=1};
        {x=1; y=3};
        {x=3; y=3}
    ]

let test_can_expand_my_galaxies _ =
    let galaxies = Day11.parse_galaxies my_input in
    let expanded = Day11.expand_galaxies galaxies in
    assert_equal expanded [
        {x=2; y=0};
        {x=5; y=1};
        {x=2; y=4};
        {x=5; y=4}
    ]

let test_can_parse_sample_galaxies _ =
    let galaxies = Day11.parse_galaxies sample_input in
    assert_equal galaxies [
        {x=3; y=0}; {x=7; y=1}; {x=0; y=2};
        {x=6; y=4}; {x=1; y=5}; {x=9; y=6};
        {x=7; y=8}; {x=0; y=9}; {x=4; y=9}
    ];;

let test_can_solve_part_one_with_sample_input _ =
    let result = Day11.part_one sample_input in
    assert_equal result 374;;

let test_can_solve_part_two_with_sample_input_and_10_times_larger _ =
    let result = Day11.part_two ~factor:10 sample_input in
    assert_equal result 1030;;

let test_can_solve_part_two_with_sample_input_and_100_times_larger _ =
    let result = Day11.part_two ~factor:100 sample_input in
    assert_equal result 8410;;

let tests =
    "day 11" >::: [
        "can parse galaxies from my input" >:: test_can_parse_my_galaxies;
        "can expand galaxies from my input" >:: test_can_expand_my_galaxies;
        "can parse galaxies from sample input" >:: test_can_parse_sample_galaxies;
        "can solve part one with sample input" >:: test_can_solve_part_one_with_sample_input;
        "can solve part two with sample input and factor of 10" >:: test_can_solve_part_two_with_sample_input_and_10_times_larger;
        "can solve part two with sample input and factor of 10" >:: test_can_solve_part_two_with_sample_input_and_100_times_larger;
    ]