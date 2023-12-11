open OUnit2
open Aoc23

let sample_input = [
    "7-F7-";
    ".FJ|7";
    "SJLL7";
    "|F--J";
    "LJ.LJ"
]

let part_two_first_sample_input = [
    ".F----7F7F7F7F-7....";
    ".|F--7||||||||FJ....";
    ".||.FJ||||||||L7....";
    "FJL7L7LJLJ||LJ.L-7..";
    "L--J.L7...LJS7F-7L7.";
    "....F-J..F7FJ|L7L7L7";
    "....L7.F7||L7|.L7L7|";
    ".....|FJLJ|FJ|F7|.LJ";
    "....FJL-7.||.||||...";
    "....L---J.LJ.LJLJ..."
]

let part_two_second_sample_input = [
    "FF7FSF7F7F7F7F7F---7";
    "L|LJ||||||||||||F--J";
    "FL-7LJLJ||||||LJL-77";
    "F--JF--7||LJLJ7F7FJ-";
    "L---JF-JLJ.||-FJLJJ7";
    "|F|F-JF---7F7-L7L|7|";
    "|FFJF7L7F-JF7|JL---7";
    "7-L-JL7||F7|L7F-7F7|";
    "L.L7LFJ|||||FJL7||LJ";
    "L7JLJL-JLJLJL--JLJ.L"
]

let test_can_parse_sample_input_and_find_start _ =
    let grid = Day10.parse_grid sample_input in
    let position = Day10.find_in_grid grid Day10.Start in
    assert_equal position (Some {x=0; y=2});;

let test_can_step_east_from_start _ =
    let grid = Day10.parse_grid sample_input in
    let start = Day10.find_in_grid grid Day10.Start in
    match start with
    | None -> assert false
    | Some s ->
        let coord, direction = Day10.step grid s Day10.E in
        assert_equal coord (Some {x=1; y=2});
        assert_equal direction (Some Day10.N);;


let test_can_follow_pipe_east _ =
    let grid = Day10.parse_grid sample_input in
    let start = Day10.find_in_grid grid Day10.Start in
    let path = Day10.follow grid (Option.get start) Day10.E in
    assert_equal (List.length path) 16;;

let test_can_solve_part_one _ =
    let result = Day10.part_one sample_input in
    assert_equal result 8;;

let test_can_find_area_from_0_0_in_first_sample _ =
    let grid = Day10.parse_grid part_two_first_sample_input in
    let start = Day10.find_in_grid grid Day10.Start |> Option.get in
    let loop = Day10.find_loop grid start in
    let area = Day10.find_area grid loop {x=0; y=0} |> Day10.CoordSet.of_list in
    let expected = Day10.CoordSet.of_list [{x=0; y=0}; {x=0; y=1}; {x=0; y=2}] in
    assert_equal (Day10.CoordSet.equal area expected) true;;

let test_first_sample_turns_cw_more_than_acw _ =
    let grid = Day10.parse_grid sample_input in
    let start = Day10.find_in_grid grid Day10.Start |> Option.get in
    let turns = Day10.count_turns grid start Day10.E in
    assert_equal (turns > 0) true;;

let test_second_sample_turns_acw_more_than_cw _ =
    let grid = Day10.parse_grid part_two_first_sample_input in
    let start = Day10.find_in_grid grid Day10.Start |> Option.get in
    let turns = Day10.count_turns grid start Day10.E in
    assert_equal (turns < 0) true;;

let test_can_solve_part_two_for_first_input _ =
    let result = Day10.part_two part_two_first_sample_input in
    assert_equal result 8;;

let test_can_solve_part_two_for_second_input _ =
    let result = Day10.part_two part_two_second_sample_input in
    assert_equal result 10;;

let tests =
    "day 10" >::: [
        "can parse sample input and find start" >:: test_can_parse_sample_input_and_find_start;
        "can step east from start" >:: test_can_step_east_from_start;
        "can follow pipe east" >:: test_can_follow_pipe_east;
        "can solve part one" >:: test_can_solve_part_one;
        "can find area from (0, 0) in first sample" >:: test_can_find_area_from_0_0_in_first_sample;
        "first sample turns cw more than acw" >:: test_first_sample_turns_cw_more_than_acw;
        "second sample turns acw more than cw" >:: test_second_sample_turns_acw_more_than_cw;
        "can solve part two for first input" >:: test_can_solve_part_two_for_first_input;
        "can solve part two for second input" >:: test_can_solve_part_two_for_second_input;
    ]