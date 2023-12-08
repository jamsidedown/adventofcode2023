open OUnit2
open Aoc23

let sample_input = [
    "LLR";
    "";
    "AAA = (BBB, BBB)";
    "BBB = (AAA, ZZZ)";
    "ZZZ = (ZZZ, ZZZ)"
];;

let test_can_parse_directions _ =
    let line = "LLR" in
    let dirs = Day08.parse_directions line in
    assert_equal dirs [Day08.Left; Day08.Left; Day08.Right];;

let test_can_parse_network_node _ =
    let line = "BBB = (AAA, ZZZ)" in
    let node = Day08.parse_network_node line in
    match node with
    | None -> assert false
    | Some (a, (b, c)) ->
        assert_equal a "BBB";
        assert_equal b "AAA";
        assert_equal c "ZZZ";;

let test_can_parse_network _ =
    let lines = [
        "AAA = (BBB, BBB)";
        "BBB = (AAA, ZZZ)";
        "ZZZ = (ZZZ, ZZZ)"
    ] in
    let network = Day08.parse_network lines in
    assert_equal (Day08.NetworkMap.find "AAA" network) ("BBB", "BBB");
    assert_equal (Day08.NetworkMap.find "BBB" network) ("AAA", "ZZZ");
    assert_equal (Day08.NetworkMap.find "ZZZ" network) ("ZZZ", "ZZZ");;

let test_can_solve_part_one _ =
    let result = Day08.part_one sample_input in
    assert_equal result 6;;

let tests =
    "day 8" >::: [
        "can parse directions" >:: test_can_parse_directions;
        "can parse network node" >:: test_can_parse_network_node;
        "can parse network" >:: test_can_parse_network;
        "can solve part one" >:: test_can_solve_part_one;
    ]
