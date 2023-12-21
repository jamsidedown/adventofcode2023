open OUnit2
open Aoc23

let sample_input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";;

let test_can_hash_hash _ =
    let result = Day15.hash "HASH" in
    assert_equal result 52;;

let test_can_solve_part_one_for_sample_input _ =
    let split = sample_input |> String.split_on_char ',' in
    let result = Day15.part_one split in
    assert_equal result 1320;;

let test_can_solve_part_two_for_sample_input _ =
    let split = sample_input |> String.split_on_char ',' in
    let result = Day15.part_two split in
    assert_equal result 145;;

let tests =
    "day 15" >::: [
        "can hash HASH" >:: test_can_hash_hash;
        "can solve part one for sample input" >:: test_can_solve_part_one_for_sample_input;
        "can solve part two for sample input" >:: test_can_solve_part_two_for_sample_input;
    ]
