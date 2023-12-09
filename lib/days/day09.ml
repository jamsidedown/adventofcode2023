open Operators

let parse (line: string) : int list =
    line
    |> String.split_on_char ' '
    |> List.map int_of_string;;

let rec get_differences (values: int list) : int list =
    match values with
    | a :: b :: tail -> (b - a) :: get_differences (b :: tail)
    | _ -> [];;

let rec extrapolate (values: int list) : int =
    if List.for_all (fun x -> x = 0) values then 0
    else match List.rev values with
    | head :: _ -> head + (get_differences values |> extrapolate)
    | [] -> 0;;

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
