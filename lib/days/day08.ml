module NetworkMap = Map.Make (String)

type direction = Left |  Right

let parse_directions (line: string) : direction list =
    let rec recurse (chars: char list) : direction option list =
        match chars with
        | [] -> []
        | 'L' :: tail -> Some Left :: recurse tail
        | 'R' :: tail -> Some Right :: recurse tail
        | _ -> [None]
    in
    line
    |> String.to_list
    |> recurse
    |> List.choose;;

let node_pattern = Str.regexp "[A-Z]+";;

let parse_network_node (line: string) : (string * (string * string)) option =
    match String.get_all_matches node_pattern line with
    | [a; b; c] -> Some (a, (b, c))
    | ss ->
        ss |> List.iter (fun s -> print_endline s);
        None;;

let parse_network (lines: string list) : (string*string) NetworkMap.t =
    lines
    |> List.map parse_network_node
    |> List.choose
    |> NetworkMap.of_list;;

let navigate_to (network: (string*string) NetworkMap.t) (directions: direction list) (start: string) (goal: string) : int =
    let rec recurse (current: string) (steps: int) (remaining: direction list) : int =
        if current = goal then steps else
        match remaining with
        | [] -> recurse current steps directions
        | Left :: tail ->
            let (left, _) = NetworkMap.find current network in
            recurse left (steps + 1) tail
        | Right :: tail ->
            let (_, right) = NetworkMap.find current network in
            recurse right (steps + 1) tail
    in
    recurse start 0 directions;;

let part_one (lines: string list) : int =
    match lines with
    | dirs :: _ :: net ->
        let directions = parse_directions dirs in
        let network = parse_network net in
        navigate_to network directions "AAA" "ZZZ"
    | _ -> 0;;

let part_two (_: string list) : int =
    0;;

let run () =
    let input = File.read_lines "day08.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
