open OUnit2
open Aoc23

let sample_input = [
    "seeds: 79 14 55 13";
    "";
    "seed-to-soil map:";
    "50 98 2";
    "52 50 48";
    "";
    "soil-to-fertilizer map:";
    "0 15 37";
    "37 52 2";
    "39 0 15";
    "";
    "fertilizer-to-water map:";
    "49 53 8";
    "0 11 42";
    "42 0 7";
    "57 7 4";
    "";
    "water-to-light map:";
    "88 18 7";
    "18 25 70";
    "";
    "light-to-temperature map:";
    "45 77 23";
    "81 45 19";
    "68 64 13";
    "";
    "temperature-to-humidity map:";
    "0 69 1";
    "1 0 69";
    "";
    "humidity-to-location map:";
    "60 56 37";
    "56 93 4"
]

let test_can_parse_sample_seeds _ =
    let line = "seeds: 79 14 55 13" in
    let seeds = Day05.parse_seeds line in
    assert_equal seeds [79; 14; 55; 13];;

let test_can_apply_first_sample_range _ =
    let range: Day05.range = {dest=50; src=98; length=2} in
    assert_equal (Day05.apply_range range 97) None;
    assert_equal (Day05.apply_range range 98) (Some 50);
    assert_equal (Day05.apply_range range 99) (Some 51);
    assert_equal (Day05.apply_range range 100) None;;

let test_can_apply_first_sample_map _ =
    let map: Day05.map = {
        name="seed-to-soil";
        ranges=[
            {dest=50; src=98; length=2};
            {dest=52; src=50; length=48}
        ]
    } in
    assert_equal (Day05.apply_map map 97) 99;
    assert_equal (Day05.apply_map map 98) 50;
    assert_equal (Day05.apply_map map 99) 51;
    assert_equal (Day05.apply_map map 100) 100;;

let test_can_parse_first_sample_range _ =
    let line = "50 98 2" in
    let range = Day05.parse_range line in
    assert_equal range (Some {dest=50; src=98; length=2});;

let test_can_parse_first_sample_name _ =
    let line = "seed-to-soil map:" in
    let name = Day05.parse_name line in
    assert_equal name "seed-to-soil";;

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

let test_can_solve_part_one _ =
    let result = Day05.part_one sample_input in
    assert_equal result 35;;



let tests =
    "day 5" >::: [
        "can parse seeds from sample" >:: test_can_parse_sample_seeds;
        "can apply first range from sample" >:: test_can_apply_first_sample_range;
        "can apply first map from sample" >:: test_can_apply_first_sample_map;
        "can parse first sample range" >:: test_can_parse_first_sample_range;
        "can parse first sample name" >:: test_can_parse_first_sample_name;
        "can parse first sample map" >:: test_can_parse_first_sample_map;
        "can solve part one with the sample data" >:: test_can_solve_part_one;
    ]