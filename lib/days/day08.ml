module NetworkMap = Map.Make (String)
type network = (string*string) NetworkMap.t

type direction = Left | Right

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

let node_pattern = Str.regexp "[A-Z0-9]+";;

let parse_network_node (line: string) : (string * (string * string)) option =
    match String.get_all_matches node_pattern line with
    | [a; b; c] -> Some (a, (b, c))
    | _ -> None;;

let parse_network (lines: string list) : network =
    lines
    |> List.map parse_network_node
    |> List.choose
    |> NetworkMap.of_list;;

let step (network: network) (direction: direction)  (current: string) : string =
    let (left, right) = NetworkMap.find current network in
    match direction with
    | Left -> left
    | Right -> right

let navigate_until (network: network) (directions: direction list) (predicate: string -> bool) (start: string) : int =
    let rec recurse (current: string) (steps: int) (remaining: direction list) : int =
        if predicate current then steps else
        match remaining with
        | [] -> recurse current steps directions
        | dir :: tail ->
            let nexts = step network dir current in
            recurse nexts (steps + 1) tail
    in
    recurse start 0 directions;;

let part_one (lines: string list) : int =
    match lines with
    | dirs :: _ :: net ->
        let directions = parse_directions dirs in
        let network = parse_network net in
        navigate_until network directions (fun s -> s = "ZZZ") "AAA"
    | _ -> 0;;

let part_two (lines: string list) : int =
    match lines with
    | dirs :: _ :: net ->
        let directions = parse_directions dirs in
        let network = parse_network net in
        let predicate = String.ends_with ~suffix:"Z" in
        let starts =
            NetworkMap.to_list network
            |> List.map (fun (key, _) -> key)
            |> List.filter (String.ends_with ~suffix:"A") in
        starts
        |> List.map (navigate_until network directions predicate)
        |> Math.lcm_of_list
    | _ -> 0;;

let run () =
    let input = File.read_lines "day08.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
