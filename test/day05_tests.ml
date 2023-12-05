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
    let conversion: Day05.conversion = {range=Range.from_start 98 2; transform = -48} in
    assert_equal (Day05.apply_conversion conversion 97) None;
    assert_equal (Day05.apply_conversion conversion 98) (Some 50);
    assert_equal (Day05.apply_conversion conversion 99) (Some 51);
    assert_equal (Day05.apply_conversion conversion 100) None;;

let test_can_apply_first_sample_map _ =
    let map: Day05.map = {
        name="seed-to-soil";
        conversions=[
            {range=Range.from_start 98 2; transform = -48};
            {range=Range.from_start 50 48; transform = 2}
        ]
    } in
    assert_equal (Day05.apply_map map 97) 99;
    assert_equal (Day05.apply_map map 98) 50;
    assert_equal (Day05.apply_map map 99) 51;
    assert_equal (Day05.apply_map map 100) 100;;

let test_can_parse_first_sample_range _ =
    let line = "50 98 2" in
    let conversion = Day05.parse_conversion line in
    assert_equal conversion (Some {range=Range.init 98 99; transform= -48});;

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
    | [s] ->
        assert_equal s.name "seed-to-soil";
        (match s.conversions with
        | [a; b] ->
            assert_equal a {range=Range.from_start 98 2; transform = -48};
            assert_equal b {range=Range.from_start 50 48; transform = 2}
        | _ -> assert false)
    | _ -> assert false;;

let test_can_solve_part_one _ =
    let result = Day05.part_one sample_input in
    assert_equal result 35;;

let test_can_parse_seed_ranges _ =
    let line = "seeds: 79 14 55 13" in
    match Day05.parse_seed_ranges line with
    | [first; second] ->
        assert_equal first {start=79; stop=92};
        assert_equal second {start=55; stop=67}
    | _ -> assert false;;

let test_can_apply_first_map_to_seed_ranges _ =
    let seed_ranges = Day05.parse_seed_ranges "seeds: 79 14 55 13"in
    let maps = Day05.parse_maps [ "seed-to-soil map:"; "50 98 2"; "52 50 48"] in
    let applied = Day05.apply_maps_to_seed_ranges maps seed_ranges in
    assert_equal applied [
        Range.init 81 94;
        Range.init 57 69
    ];;

let test_can_apply_range_conversion_when_no_overlap_left _ =
    let seed_range = Range.init 1 4 in
    let conversion: Day05.conversion = {range=Range.init 5 6; transform=10} in
    let applied = Day05.apply_conversion_to_seed_range conversion seed_range in
    assert_equal applied (None, [seed_range]);;

let test_can_apply_range_conversion_when_no_overlap_right _ =
    let seed_range = Range.init 7 10 in
    let conversion: Day05.conversion = {range=Range.init 5 6; transform=10} in
    let applied = Day05.apply_conversion_to_seed_range conversion seed_range in
    assert_equal applied (None, [seed_range]);;

let test_can_apply_range_conversion_when_seed_range_encompasses_conversion _ =
    let seed_range = Range.init 1 10 in
    let conversion: Day05.conversion = {range=Range.init 4 6; transform=10} in
    let applied = Day05.apply_conversion_to_seed_range conversion seed_range in
    assert_equal applied (
        Some (Range.init 14 16),
        [
            Range.init 1 3;
            Range.init 7 10
        ]
    );;

let test_can_apply_range_conversion_when_conversion_encompasses_seeds _ =
    let seed_range = Range.init 4 6 in
    let conversion: Day05.conversion = {range=Range.init 1 10; transform=10} in
    let applied = Day05.apply_conversion_to_seed_range conversion seed_range in
    assert_equal applied (Some (Range.init 14 16), []);;

let test_can_apply_range_conversion_when_right_side_of_seeds_in_range _ =
    let seed_range = Range.init 1 10 in
    let conversion: Day05.conversion = {range=Range.init 6 10; transform=10} in
    let applied = Day05.apply_conversion_to_seed_range conversion seed_range in
    assert_equal applied (
        Some (Range.init 16 20),
        [Range.init 1 5;]
    );;

let test_can_apply_range_conversion_when_left_side_of_seeds_in_range _ =
    let seed_range = Range.init 1 10 in
    let conversion: Day05.conversion = {range=Range.init 1 5; transform=10} in
    let applied = Day05.apply_conversion_to_seed_range conversion seed_range in
    assert_equal applied (
        Some (Range.init 11 15),
        [Range.init 6 10;]
    );;

let test_can_solve_part_two _ =
    let result = Day05.part_two sample_input in
    assert_equal result 46;;

let tests =
    "day 5" >::: [
        "can parse seeds from sample" >:: test_can_parse_sample_seeds;
        "can apply first range from sample" >:: test_can_apply_first_sample_range;
        "can apply first map from sample" >:: test_can_apply_first_sample_map;
        "can parse first sample range" >:: test_can_parse_first_sample_range;
        "can parse first sample name" >:: test_can_parse_first_sample_name;
        "can parse first sample map" >:: test_can_parse_first_sample_map;
        "can solve part one with the sample data" >:: test_can_solve_part_one;
        "can parse seed ranges " >:: test_can_parse_seed_ranges;
        "can apply first map to sample seed ranges" >:: test_can_apply_first_map_to_seed_ranges;
        "can apply range conversion to seed range when no overlap (left)" >:: test_can_apply_range_conversion_when_no_overlap_left;
        "can apply range conversion to seed range when no overlap (right)" >:: test_can_apply_range_conversion_when_no_overlap_right;
        "can apply range conversion when seeds encompass conversion range" >:: test_can_apply_range_conversion_when_seed_range_encompasses_conversion;
        "can apply range conversion when conversion encompass seed range" >:: test_can_apply_range_conversion_when_conversion_encompasses_seeds;
        "can apply range conversion when right side of seeds in range" >:: test_can_apply_range_conversion_when_right_side_of_seeds_in_range;
        "can apply range conversion when left side of seeds in range" >:: test_can_apply_range_conversion_when_left_side_of_seeds_in_range;
        "can solve part two with the sample data" >:: test_can_solve_part_two;
    ]