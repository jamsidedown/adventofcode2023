open OUnit2
open Aoc23

let sample_input =
    "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..";;

let test_can_read_numbers_in_line _ =
    let line = "467..114.." in
    let numbers = Day03.read_numbers line 0 in
    assert_equal numbers [
        {value=467; line=0; start=0; stop=2};
        {value=114; line=0; start=5; stop=7};
    ];;

let test_can_read_number_at_end_of_line _ =
    let line = "..123" in
    let numbers = Day03.read_numbers line 0 in
    match numbers with
    | [n] -> assert_equal n.value 123
    | _ -> assert false;;

let test_can_read_symbol_in_line_with_number _ =
    let line = "617*......" in
    let symbols = Day03.read_symbols line 4 in
    assert_equal symbols [{symbol='*'; line=4; position=3}];;

let test_can_read_multiple_symbols_in_line _ =
    let line = "...$.*...." in
    let symbols = Day03.read_symbols line 8 in
    assert_equal symbols [
        {symbol='$'; line=8; position=3};
        {symbol='*'; line=8; position=5}
    ];;

let test_can_filter_numbers_not_adjacent_to_symbols _ =
    let numbers = Day03.read_numbers "467..114.." 0 in
    let symbols = Day03.read_symbols "...*......" 1 in
    let filtered = Day03.not_adjacent symbols numbers in
    match filtered with
    | [n] -> assert_equal n.value 114
    | _ -> assert false;;

let test_can_solve_part_one _ =
    let input = String.split_on_char '\n' sample_input in
    let result = Day03.part_one input in
    assert_equal result 4361;;

let test_can_solve_part_two _ =
    let input = String.split_on_char '\n' sample_input in
    let result = Day03.part_two input in
    assert_equal result 467835;;

let tests =
    "day 3" >::: [
        "can read numbers in line" >:: test_can_read_numbers_in_line;
        "can read number at end of line" >:: test_can_read_number_at_end_of_line;
        "can read symbol in line with number" >:: test_can_read_symbol_in_line_with_number;
        "can read symbols in line" >:: test_can_read_multiple_symbols_in_line;
        "can filter numbers not adjacent to symbols" >::test_can_filter_numbers_not_adjacent_to_symbols;
        "can solve part one with sample input" >:: test_can_solve_part_one;
        "can solve part two with sample input" >:: test_can_solve_part_two;
    ]
