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

let parse_ints (line: string) : int list =
    match String.split_on_char ' ' line with
    | [] -> []
    | lst -> lst |> List.map (String.trim >> int_of_string);;

let parse_seeds (line: string) : int list =
    match String.split_on_char ':' line with
    | [_; tail] -> tail |> String.trim |> parse_ints
    | _ -> [];;

let parse_name (line: string) : string =
    Scanf.sscanf line "%s map:" (fun x -> x);;

let parse_range (line: string) : range option =
    match parse_ints line with
    | [dest; src; length] -> Some {dest=dest; src=src; length=length}
    | _ -> None

let parse_maps (lines: string list) : map list =
    let rec recurse (name: string) (ranges: range list) (lines: string list) : map list =
        match lines with
        | "" :: tail ->
            (match ranges with
            | [] -> recurse "" [] tail
            | _ -> {name=name; ranges=List.rev ranges} :: recurse "" [] tail)
        | head :: tail when name = "" -> recurse (parse_name head) [] tail
        | head :: tail ->
            let range = parse_range head in
            (match range with
            | Some range -> recurse name (range :: ranges) tail
            | None -> recurse name ranges tail)
        | [] when name = "" -> []
        | _ -> {name=name; ranges=List.rev ranges} :: []
    in
    recurse "" [] lines;;

let apply_range (range: range) (value: int) : int option =
    let diff = value - range.src in
    if diff >= 0 && diff < range.length then Some (range.dest + diff) else None;;

let apply_map (map: map) (value: int) : int =
    let rec recurse (ranges: range list) : int =
        match ranges with
        | [] -> value
        | head :: tail ->
            match apply_range head value with
            | Some x -> x
            | None -> recurse tail
    in
    recurse map.ranges;;

let rec apply_maps (maps: map list) (value: int) : int =
    match maps with
    | map :: tail -> apply_map map value |> apply_maps tail
    | [] -> value;;

let part_one (lines: string list) : int =
    match lines with
    | [] -> 0
    | head :: tail ->
        let seeds = parse_seeds head in
        let maps = parse_maps tail in
        seeds
        |> List.map (fun seed -> apply_maps maps seed)
        |> List.sort compare
        |> List.hd;;

let part_two (_: string list) : int =
    0;;

let run () =
    let input = File.read_lines "day05.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
