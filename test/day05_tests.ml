open OUnit2
open Aoc23

let test_can_parse_sample_seeds _ =
    let line = "seeds: 79 14 55 13" in
    let seeds = Day05.parse_seeds line in
    assert_equal seeds [79; 14; 55; 13];;

let test_can_apply_first_sample_range _ =
    let range: Day05.range = {dest=50; src=98; length=2} in
    assert_equal (Day05.apply_range range 97) 97;
    assert_equal (Day05.apply_range range 98) 50;
    assert_equal (Day05.apply_range range 99) 51;
    assert_equal (Day05.apply_range range 100) 100;;

let test_can_parse_first_sample_map _ =
    let lines = [
        "seed-to-soil map:";
        "50 98 2";
        "52 50 48";
    ] in
    let maps = Day05.parse_maps lines in
    match maps with
    | [m] ->
        assert_equal m.name "seed-to-soil";
        (match m.ranges with
        | [a; b] ->
            assert_equal a {dest=50; src=98; length=2};
            assert_equal b {dest=52; src=50; length=48}
        | _ -> assert false)
    | _ -> assert false;;

let tests =
    "day 4" >::: [
        "can parse seeds from sample" >:: test_can_parse_sample_seeds;
        "can map seed to soil from sample" >:: test_can_apply_first_sample_range;
        "can parse first sample maps" >:: test_can_parse_first_sample_map;
    ]