let is_int (c: char) : bool =
    c >= '0' && c <= '9';;

let int_of_chars (chars: char list) : int =
    match chars with
    | [] -> 0
    | lst ->
        lst
        |> List.map (Printf.sprintf "%c")
        |> String.concat ""
        |> int_of_string;;

let rec parse_string_ints (chars: char list) : char list =
    match chars with
    | [] -> []
    | c :: tail when is_int c -> c :: parse_string_ints tail
    | 'o' :: 'n' :: 'e' :: tail -> '1' :: parse_string_ints ('e' :: tail) (* the 'e' could be part of eight *)
    | 't' :: 'w' :: 'o' :: tail -> '2' :: parse_string_ints ('o' :: tail)
    | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: tail -> '3' :: parse_string_ints ('e' :: tail)
    | 'f' :: 'o' :: 'u' :: 'r' :: tail -> '4' :: parse_string_ints tail
    | 'f' :: 'i' :: 'v' :: 'e' :: tail -> '5' :: parse_string_ints ('e' :: tail)
    | 's' :: 'i' :: 'x' :: tail -> '6' :: parse_string_ints tail
    | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: tail -> '7' :: parse_string_ints ('n' :: tail)
    | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: tail -> '8' :: parse_string_ints ('t' :: tail)
    | 'n' :: 'i' :: 'n' :: 'e' :: tail -> '9' :: parse_string_ints ('e' :: tail)
    | _ :: tail -> parse_string_ints tail;;

let first_and_last (lst: char list) : int option =
    let rec recurse (first: char) (last: char) (remaining: char list) : int =
        match remaining with
        | [] ->
            Printf.sprintf "%c%c" first last
            |> int_of_string
        | head :: tail -> recurse first head tail
    in
    match lst with
    | [] -> None
    | head :: tail -> Some (recurse head head tail);;

let part_one (lines: string list) : int =
    lines
    |> List.map (fun line ->
        line
        |> List_helpers.to_chars
        |> List.filter is_int
        |> first_and_last)
    |> List_helpers.choose
    |> List_helpers.sum;;

let part_two (lines: string list) : int =
    lines
    |> List.map (fun line ->
        line
        |> List_helpers.to_chars
        |> parse_string_ints
        |> first_and_last)
    |> List_helpers.choose
    |> List_helpers.sum;;

let run () =
    let input = Parsing.read_lines "day01.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
