open Operators

let parse (line: string) : int list =
    line
    |> String.split_on_char ' '
    |> List.map int_of_string;;

let rec get_differences (values: int list) : int list =
    match values with
    | a :: b :: tail -> (b - a) :: get_differences (b :: tail)
    | _ -> [];;

let extrapolate (values: int list) : int =
    let rec recurse (current: int list) : int =
        match current |> List.for_all (fun x -> x = 0) with
        | true -> 0
        | false ->
            match List.rev current with
            | head :: _ -> head + (get_differences current |> recurse)
            | [] -> 0
    in
    recurse values;;

let part_one (lines: string list) : int =
    lines
    |> List.map (parse >> extrapolate)
    |> List.sum;;

let part_two (lines: string list) : int =
    lines
    |> List.map (parse >> List.rev >> extrapolate)
    |> List.sum;;

let run () =
    let input = File.read_lines "day09.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
