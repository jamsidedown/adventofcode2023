open OUnit2
open Aoc23

let test_can_solve_part_one _ =
    let result = Day05.part_one [] in
    assert_equal result 0;;

let tests =
    "day 4" >::: [
        "can solve part one with sample input" >:: test_can_solve_part_one;
    ]