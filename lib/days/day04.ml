module IntSet = Set.Make (Int);;
module CardMap = Map.Make (Int);;

type scratch_card = {
    card: int;
    winning: IntSet.t;
    chosen: IntSet.t;
}

let parse_numbers (s: string) : int list =
    let rec recurse (parts: string list) : int list =
        match parts with
        | "" :: tail -> recurse tail
        | head :: tail -> int_of_string head :: recurse tail
        | [] -> []
    in
    s
    |> String.split_on_char ' '
    |> recurse;;

let parse_line (line: string) : scratch_card option =
    match String.split_on_char ':' line with
    | [card; numbers] ->
        let card_number = Scanf.sscanf card "Card %i" (fun x -> x) in
        let w, c =
            numbers
            |> String.trim
            |> String.split_on_char '|'
            |> function
            | [winning; chosen] ->
                let winning_set = parse_numbers winning |> IntSet.of_list in
                let chosen_set = parse_numbers chosen |> IntSet.of_list in
                winning_set, chosen_set
            | _ -> IntSet.empty, IntSet.empty
        in
        Some {card=card_number; winning=w; chosen=c}
    | _ -> None;;

let winning_count scratch_card : int =
    IntSet.inter scratch_card.winning scratch_card.chosen
    |> IntSet.elements
    |> List.length;;

let calculate_points scratch_card : int =
    winning_count scratch_card
    |> fun x -> x - 1
    |> Math.pow 2;;

let part_one (lines: string list) : int =
    lines
    |> List.map parse_line
    |> List.choose
    |> List.map calculate_points
    |> List.sum;;

let part_two (lines: string list) : int =
    let cards =
        lines
        |> List.map parse_line
        |> List.choose in
    let cards_map =
        cards
        |> List.map (fun card -> (card.card, card))
        |> CardMap.of_list in
    let cache = Hashtbl.create 1024 in
    let rec recurse (card_number: int) : int =
        match Hashtbl.find_opt cache card_number with
        | Some value -> value
        | None ->
            let value =
                (match CardMap.find_opt card_number cards_map with
                | None -> 0
                | Some card ->
                    let next_cards = 
                        winning_count card
                        |> List.range 1
                        |> List.map (fun x -> x + card_number) in
                    next_cards
                    |> List.map recurse
                    |> List.sum
                    |> fun x -> x + 1)
                in
            Hashtbl.add cache card_number value;
            value
    in
    cards
    |> List.map (fun c -> c.card)
    |> List.map recurse
    |> List.sum;;

let run () =
    let input = File.read_lines "day04.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
