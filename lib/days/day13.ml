let rec parse_line ?shift:(shift=0) (line: char list) : int =
    match line with
    | [] -> 0
    | '#' :: tail -> Int.shift_left 1 shift + parse_line ~shift:(shift + 1) tail
    | _ :: tail -> parse_line ~shift:(shift + 1) tail;;

let rec split_groups ?current:(current=[]) (lines: string list) : string list list =
    match lines with
    | [] -> [List.rev current]
    | "" :: tail -> (List.rev current) :: split_groups tail
    | head :: tail -> split_groups ~current:(head :: current) tail;;

let mostly_equal (a: int list) (b: int list) : bool =
    let rec recurse (a: int list) (b: int list) : bool =
        match a, b with
        | [], _
        | _, [] -> true
        | ha :: ta, hb :: tb when ha = hb -> recurse ta tb
        | _ -> false
    in
    match a, b with
    | [], _
    | _, [] -> false
    | _ -> recurse a b;;

let is_symmetrical (values: int list) : int option =
    let rec recurse (seen: int list) (remaining: int list) : int option =
        match remaining with
        | [] -> None
        | head :: tail ->
            if mostly_equal seen remaining then Some (List.length seen) else recurse (head :: seen) tail
    in
    recurse [] values;;

let find_symmetry (group: string list) : int =
    let chars = group |> List.map (String.to_list) in
    let horizontal = chars |> List.map parse_line in
    let vertical = chars |> List.transpose |> List.map parse_line in
    (* horizontal |> List.map string_of_int |> String.concat ", " |> Printf.printf "\nhorizontal\n%s\n";
    vertical |> List.map string_of_int |> String.concat ", " |> Printf.printf "\nvertical\n%s\n"; *)
    match is_symmetrical vertical with
    | Some x -> x
    | None ->
        match is_symmetrical horizontal with
        | Some x -> x * 100
        | None -> 0;;

let part_one (lines: string list) : int =
    let groups = split_groups lines in
    groups
    |> List.map find_symmetry
    |> List.sum;;

let part_two (_: string list) : int =
    0;;

let run () =
    let input = File.read_lines "day13.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
