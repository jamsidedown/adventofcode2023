module CardMap = Map.Make (Char)

type round = {
    cards: char list;
    bid: int;
}

type hand = FiveOfAKind | FourOfAKind | FullHouse | ThreeOfAKind | TwoPair | Pair | HighCard

let parse_round (line: string) : round option =
    match String.split_on_char ' ' line with
    | [cs; b] -> Some {cards = String.to_list cs; bid = int_of_string b}
    | _ -> None;;

let hand_rank (hand: hand) : int =
    match hand with
    | HighCard -> 0
    | Pair -> 1
    | TwoPair -> 2
    | ThreeOfAKind -> 3
    | FullHouse -> 4
    | FourOfAKind -> 5
    | FiveOfAKind -> 6;;

let card_ranks =
    ['A'; 'K'; 'Q'; 'J'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2']
    |> List.rev
    |> List.mapi (fun i c -> (c, i))
    |> CardMap.of_list;;

let card_joker_ranks =
    ['A'; 'K'; 'Q'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; 'J']
    |> List.rev
    |> List.mapi (fun i c -> (c, i))
    |> CardMap.of_list;;

let compare_cards (ranks: int CardMap.t) (a: char) (b: char) : int =
    let a_rank = CardMap.find a ranks in
    let b_rank = CardMap.find b ranks in
    compare a_rank b_rank;;

let rec compare_all_cards (ranks: int CardMap.t) (a: char list) (b: char list) : int =
    match a, b with
    | ah :: at, bh :: bt ->
        (match compare_cards ranks ah bh with
        | 0 -> compare_all_cards ranks at bt
        | c -> c)
    | _ -> 0;;

let calculate_hand (cards: char list) : hand =
    let rec recurse (current: hand) (remaining: char list) : hand =
        match remaining with
        | [a; b; c; d; e] when a = b && a = c && a = d && a = e -> FiveOfAKind
        | a :: b :: c :: d :: _ when a = b && a = c && a = d -> FourOfAKind
        | a :: b :: c :: tail when a = b && a = c ->
            (match current with
            | Pair -> FullHouse
            | _ -> recurse ThreeOfAKind tail)
        | a :: b :: tail when a = b ->
            (match current with
            | ThreeOfAKind -> FullHouse
            | Pair -> TwoPair
            | _ -> recurse Pair tail)
        | _ :: tail -> recurse current tail
        | [] -> current
    in
    List.sort compare cards
    |> recurse HighCard;;

let compare_hands (a: hand) (b: hand) : int =
    let a_rank = hand_rank a in
    let b_rank = hand_rank b in
    compare a_rank b_rank;;

let compare_rounds (a: round) (b: round) : int =
    let a_hand = calculate_hand a.cards in
    let b_hand = calculate_hand b.cards in
    match compare_hands a_hand b_hand with
    | 0 -> compare_all_cards card_ranks a.cards b.cards
    | c -> c;;

let rec upgrade_hand (hand: hand) (jokers: int) : hand =
    if jokers < 1 then hand else
        match hand with
        | FourOfAKind -> FiveOfAKind
        | ThreeOfAKind -> upgrade_hand FourOfAKind (jokers - 1)
        | TwoPair -> FullHouse
        | Pair -> upgrade_hand ThreeOfAKind (jokers - 1)
        | HighCard -> upgrade_hand Pair (jokers - 1)
        | FiveOfAKind | FullHouse -> assert false;;

let compare_joker_rounds (a: round) (b: round) : int =
    let a_hand_pre_jokers = a.cards |> List.filter (fun c -> c <> 'J') |> calculate_hand in
    let b_hand_pre_jokers = b.cards |> List.filter (fun c -> c <> 'J') |> calculate_hand in

    let a_jokers = a.cards |> List.filter (fun c -> c = 'J') |> List.length in
    let b_jokers = b.cards |> List.filter (fun c -> c = 'J') |> List.length in

    let a_hand = upgrade_hand a_hand_pre_jokers a_jokers in
    let b_hand = upgrade_hand b_hand_pre_jokers b_jokers in

    match compare_hands a_hand b_hand with
    | 0 -> compare_all_cards card_joker_ranks a.cards b.cards
    | c -> c;;

let part_one (lines: string list) : int =
    lines
    |> List.map parse_round
    |> List.choose
    |> List.sort compare_rounds
    |> List.mapi (fun i round -> (i + 1) * round.bid)
    |> List.sum;;

let part_two (lines: string list) : int =
    lines
    |> List.map parse_round
    |> List.choose
    |> List.sort compare_joker_rounds
    |> List.mapi (fun i round -> (i + 1) * round.bid)
    |> List.sum;;

let run () =
    let input = File.read_lines "day07.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
