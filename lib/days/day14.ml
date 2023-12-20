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

(* let tilt_row (row: space list) : space list =
    let rec recurse (spaces: space list) : space list =
        match spaces with
        | [] -> []
        | Empty :: Round :: tail -> Round :: recurse (Empty :: tail)
        | head :: tail -> head :: recurse tail
    and tilt_section (section: space list) : space list =
        let tilted = recurse section in
        if tilted <> section then tilt_section tilted else section
    and split_sections (curr: space list) (spaces: space list) : space list =
        match spaces with
        | [] -> tilt_section (List.rev curr)
        | Cube :: tail ->
            let section = List.rev (Cube :: curr) in
            tilt_section section @ split_sections [] tail
        | head :: tail -> split_sections (head :: curr) tail
    in
    split_sections [] row;; *)

let rec tilt_left (platform: platform) : platform =
    match platform with
    | row :: tail -> tilt_row row :: tilt_left tail
    | [] -> [];;

let count_right_inclusive (row: space list) : int =
    let length = List.length row in
    row
    |> List.mapi (fun i space ->
        if space = Round then (length - i) else 0)
    |> List.sum;;

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
    input |> part_two |> Printf.printf "Part two (solved in ./dotnet/DayFourteen): %i\n";;
