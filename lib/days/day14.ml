type space =
    | Round
    | Cube
    | Empty

type platform = space list list

let parse_space (c: char) : space =
    match c with
    | 'O' -> Round
    | '#' -> Cube
    | _ -> Empty;;

let parse_platform (lines: string list) : platform =
    lines
    |> List.map String.to_list
    |> List.map (List.map parse_space)

let rec tilt_row (row: space list) : space list =
    let rec recurse (spaces: space list) : space list =
        match spaces with
        | [] -> []
        | Empty :: Round :: tail -> Round :: recurse (Empty :: tail)
        | head :: tail -> head :: recurse tail
    in
    let tilted = recurse row in
    if tilted <> row then tilt_row tilted else row;;

let rec tilt_left (platform: platform) : platform =
    match platform with
    | row :: tail -> tilt_row row :: tilt_left tail
    | [] -> [];;

let rec count_right_inclusive (row: space list) : int =
    match row with
    | Round :: tail -> (List.length tail + 1) + count_right_inclusive tail
    | _ :: tail -> count_right_inclusive tail
    | [] -> 0;;

let part_one (lines: string list) : int =
    lines
    |> parse_platform
    |> List.transpose
    |> tilt_left
    |> List.map count_right_inclusive
    |> List.sum;;

let part_two (_: string list) : int =
    0;;

let run () =
    let input = File.read_lines "day14.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
