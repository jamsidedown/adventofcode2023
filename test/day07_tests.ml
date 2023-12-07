open OUnit2
open Aoc23

let sample_input = [
    "32T3K 765";
    "T55J5 684";
    "KK677 28";
    "KTJJT 220";
    "QQQJA 483"
]

let compare_cards = Day07.compare_cards Day07.card_ranks;;
let compare_all_cards = Day07.compare_all_cards Day07.card_ranks;;

let test_can_parse_first_sample_round _ =
    let line = "32T3K 765" in
    let result = Day07.parse_round line in
    match result with
    | Some round ->
        assert_equal round.cards ['3'; '2'; 'T'; '3'; 'K'];
        assert_equal round.bid 765
    | None -> assert false;;

let test_can_compare_cards _ =
    assert_equal (compare_cards '3' '7') (-1);
    assert_equal (compare_cards 'T' 'T') 0;
    assert_equal (compare_cards 'K' '5') 1;;

let test_can_calculate_hands _ =
    assert_equal (Day07.calculate_hand ['K'; 'K'; 'K'; 'K'; 'K']) Day07.FiveOfAKind;
    assert_equal (Day07.calculate_hand ['K'; 'K'; 'Q'; 'K'; 'K']) Day07.FourOfAKind;
    assert_equal (Day07.calculate_hand ['K'; 'K'; 'Q'; 'K'; 'Q']) Day07.FullHouse;
    assert_equal (Day07.calculate_hand ['K'; 'K'; 'T'; 'K'; 'Q']) Day07.ThreeOfAKind;
    assert_equal (Day07.calculate_hand ['T'; 'K'; 'Q'; 'K'; 'Q']) Day07.TwoPair;
    assert_equal (Day07.calculate_hand ['T'; 'K'; '4'; 'K'; 'Q']) Day07.Pair;
    assert_equal (Day07.calculate_hand ['T'; 'A'; '9'; 'K'; 'Q']) Day07.HighCard;;

let test_can_compare_hands _ =
    assert_equal (Day07.compare_hands Day07.FiveOfAKind Day07.FourOfAKind) 1;
    assert_equal (Day07.compare_hands Day07.ThreeOfAKind Day07.FourOfAKind) (-1);
    assert_equal (Day07.compare_hands Day07.FullHouse Day07.FullHouse) 0;
    assert_equal (Day07.compare_hands Day07.FullHouse Day07.ThreeOfAKind) 1;
    assert_equal (Day07.compare_hands Day07.ThreeOfAKind Day07.TwoPair) 1;
    assert_equal (Day07.compare_hands Day07.TwoPair Day07.Pair) 1;
    assert_equal (Day07.compare_hands Day07.Pair Day07.HighCard) 1;;

let test_can_compare_two_twopair_hands _ =
    let hand_a = String.to_list "KK677" in
    let hand_b = String.to_list "KTJJT" in
    assert_equal (compare_all_cards hand_a hand_b) 1;;

let test_can_solve_part_one _ =
    let result = Day07.part_one sample_input in
    assert_equal result 6440;;

let test_can_solve_part_two _ =
    let result = Day07.part_two sample_input in
    assert_equal result 5905;;

let tests =
    "day 7" >::: [
        "can parse first sample round" >:: test_can_parse_first_sample_round;
        "can compare cards" >:: test_can_compare_cards;
        "can calculate hands" >:: test_can_calculate_hands;
        "can compare hands" >:: test_can_compare_hands;
        "can compare two twopair hands" >:: test_can_compare_two_twopair_hands;
        "can solve part one" >:: test_can_solve_part_one;
        "can solve part two" >:: test_can_solve_part_two;
    ]
