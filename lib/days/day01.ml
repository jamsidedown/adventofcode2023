open Operators

let rec parse_string_ints (chars: char list) : char list =
    match chars with
    | [] -> []
    | c :: tail when Char.is_int c -> c :: parse_string_ints tail
    | 'o' :: 'n' :: 'e' :: tail -> '1' :: parse_string_ints ('e' :: tail) (* the 'e' could be the start of eight *)
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
    match lst, List.last lst with
    | head :: _, Some last -> Some (Int.of_chars [head; last])
    | _ -> None;;

let part_one (lines: string list) : int =
    lines
    |> List.map (
        String.to_list
        >> (List.filter Char.is_int)
        >> first_and_last)
    |> List.choose
    |> List.sum;;

let part_two (lines: string list) : int =
    lines
    |> List.map (
        String.to_list
        >> parse_string_ints
        >> first_and_last)
    |> List.choose
    |> List.sum;;

let run () =
    let input = File.read_lines "day01.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
