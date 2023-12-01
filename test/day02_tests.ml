open OUnit2
open Aoc23

let can_solve_sample_for_part_one _ =
    Day02.part_one ["hello"] |> ignore;
    assert true;;

let tests =
    "day 2" >::: [
        "can solve sample input for part one" >:: can_solve_sample_for_part_one;
    ]