open Operators

type range = {
    dest: int;
    src: int;
    length: int;
}

type map = {
    name: string;
    ranges: range list;
}

let parse_seeds (line: string) : int list =
    match String.split_on_char ' ' line with
    | [] -> []
    | _ :: tail ->
        tail |> List.map (String.trim >> int_of_string);;

let parse_maps (_: string list) : map list =
    [];;

let apply_range (range: range) (value: int) : int =
    let diff = value - range.src in
    if diff >= 0 && diff < range.length then range.dest + diff else value;;

let part_one (_: string list) : int =
    0;;

let part_two (_: string list) : int =
    0;;

let run () =
    print_endline "day 5";;
