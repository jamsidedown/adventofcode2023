open OUnit2
open Aoc23

let sample_input = [
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53";
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19";
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1";
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83";
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36";
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"];;

let test_can_parse_winning_numbers_in_first_sample _ =
    let input = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53" in
    let parsed = Day04.parse_line input in
    match parsed with
    | None -> assert false
    | Some card -> assert_equal card.winning (Day04.IntSet.of_list [41; 48; 83; 86; 17]);;

let test_can_parse_chosen_numbers_in_first_sample _ = 
    let input = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53" in
    let parsed = Day04.parse_line input in
    match parsed with
    | None -> assert false
    | Some card -> assert_equal card.chosen (Day04.IntSet.of_list [83; 86; 6; 31; 17; 9; 48; 53]);;

let test_can_calculate_points_of_first_sample _ =
    let input = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53" in
    let parsed = Day04.parse_line input in
    match parsed with
    | None -> assert false
    | Some card -> 
        let points = Day04.calculate_points card in
        assert_equal points 8;;

let test_can_solve_part_one _ =
    let result = Day04.part_one sample_input in
    assert_equal result 13;;

let test_can_solve_part_two _ =
    let result = Day04.part_two sample_input in
    assert_equal result 30;;

let tests =
    "day 4" >::: [
        "can parse winning numbers in first sample line" >:: test_can_parse_winning_numbers_in_first_sample;
        "can parse chosen numbers in first sample line" >:: test_can_parse_chosen_numbers_in_first_sample;
        "can calculate points in first sample" >:: test_can_calculate_points_of_first_sample;
        "can solve part one with sample input" >:: test_can_solve_part_one;
        "can solve part two with sample input" >:: test_can_solve_part_two;
    ]