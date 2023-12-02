open OUnit2
open Aoc23

let can_parse_first_game_id _ =
    let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" in
    let game = Day02.parse_game input in
    match game with
    | Some g -> assert_equal g.game_id 1
    | _ -> assert false;;

let can_parse_second_game_id _ =
    let input = "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue" in
    let game = Day02.parse_game input in
    match game with
    | Some g -> assert_equal g.game_id 2
    | _ -> assert false;;

let can_parse_first_round _ =
    let input = " 3 blue, 4 red" in
    let round = Day02.parse_round input in
    assert_equal round { red= 4; green=0; blue=3 };;

let can_parse_first_game _ =
    let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" in
    let game = Day02.parse_game input in
    assert_equal game (Some { game_id=1; rounds= [
        { red=4; green=0; blue=3 };
        { red=1; green=2; blue=6 };
        { red=0; green=2; blue=0 }
    ] });;

let can_check_that_game_is_possible _ =
    let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" in
    let game = Day02.parse_game input in
    match game with
    | None -> assert false
    | Some g ->
        let max_round: Day02.round = { red=12; green=13; blue=14 } in
        let possible = Day02.is_possible_game g max_round in
        assert_equal possible true;;

let can_check_that_game_is_impossible _ =
    let input = "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red" in
    let game = Day02.parse_game input in
    match game with
    | None -> assert false
    | Some g ->
        let max_round: Day02.round = { red=12; green=13; blue=14 } in
        let possible = Day02.is_possible_game g max_round in
        assert_equal possible false;;

let can_solve_part_one _ =
    let input = [
        "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";
        "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue";
        "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red";
        "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red";
        "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green";
    ] in
    let result = Day02.part_one input in
    assert_equal result 8;;

let can_get_minimum_cubes_for_first_game _ =
    match Day02.parse_game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" with
    | None -> assert false
    | Some game ->
        let expected: Day02.round = { red=4; green=2; blue=6 } in
        let minimum = Day02.get_minumum_cubes game in
        assert_equal minimum expected;;

let can_calculate_power_of_first_game _ =
    match Day02.parse_game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" with
    | None -> assert false
    | Some game ->
        let minimum = Day02.get_minumum_cubes game in
        let power = Day02.get_power minimum in
        assert_equal power 48;;

let can_solve_part_two _ =
    let input = [
        "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";
        "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue";
        "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red";
        "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red";
        "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green";
    ] in
    let result = Day02.part_two input in
    assert_equal result 2286;;

let tests =
    "day 2" >::: [
        "can parse game id from first sample line" >:: can_parse_first_game_id;
        "can parse game id from second sample line" >:: can_parse_second_game_id;
        "can parse first round" >:: can_parse_first_round;
        "can parse first game" >:: can_parse_first_game;
        "can check that game is possible" >:: can_check_that_game_is_possible;
        "can check that game is impossible" >:: can_check_that_game_is_impossible;
        "can solve part one" >:: can_solve_part_one;
        "can get minumum cubes for first game" >:: can_get_minimum_cubes_for_first_game;
        "can calculate power of first game" >:: can_calculate_power_of_first_game;
        "can solve part two" >:: can_solve_part_two;
    ]