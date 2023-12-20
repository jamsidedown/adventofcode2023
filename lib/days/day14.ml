type space =
    | Round
    | Cube
    | Empty

type platform = space list list

let print_row (row: space list) : unit =
    row
    |> List.map (fun space ->
        match space with
        | Round -> 'O'
        | Cube -> '#'
        | Empty -> '.')
    |> String.of_list
    |> print_endline;;

let print_platform (platform: platform) : unit =
    print_endline "";
    platform
    |> List.iter print_row;; 

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

let cycle (platform: platform) : platform =
    platform
    |> tilt_left
    |> List.rotate_cw
    |> tilt_left
    |> List.rotate_cw
    |> tilt_left
    |> List.rotate_cw
    |> tilt_left
    |> List.rotate_cw;;

let part_one (lines: string list) : int =
    lines
    |> parse_platform
    |> List.transpose
    |> tilt_left
    |> List.map count_right_inclusive
    |> List.sum;;

let part_two (lines: string list) : int =
    let rec recurse (count: int) (platform: platform) : platform =
        if count mod 100_000 = 0 then Printf.printf "%i\n%!" count;
        if count > 0 then recurse (count - 1) (cycle platform) else platform
    in
    lines
    |> parse_platform
    |> List.rotate_ccw
    |> recurse 1_000_000_000
    |> List.map count_right_inclusive
    |> List.sum;;

let run () =
    let input = File.read_lines "day14.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;