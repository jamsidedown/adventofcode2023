open Operators

type conversion = {
    range: Range.range;
    transform: int;
}

type map = {
    name: string;
    conversions: conversion list;
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

let parse_conversion (line: string) : conversion option =
    match parse_ints line with
    | [dest; src; length] -> Some {range=Range.from_start src length; transform = dest - src}
    | _ -> None

let parse_maps (lines: string list) : map list =
    let rec recurse (name: string) (conversions: conversion list) (lines: string list) : map list =
        match lines with
        | "" :: tail ->
            (match conversions with
            | [] -> recurse "" [] tail
            | _ -> {name=name; conversions=List.rev conversions} :: recurse "" [] tail)
        | head :: tail when name = "" -> recurse (parse_name head) [] tail
        | head :: tail ->
            let range = parse_conversion head in
            (match range with
            | Some range -> recurse name (range :: conversions) tail
            | None -> recurse name conversions tail)
        | [] when name = "" -> []
        | _ -> {name=name; conversions=List.rev conversions} :: []
    in
    recurse "" [] lines;;

let apply_conversion (conversion: conversion) (value: int) : int option =
    if Range.contains value conversion.range
    then Some (value + conversion.transform)
    else None;;

let apply_map (map: map) (value: int) : int =
    let rec recurse (ranges: conversion list) : int =
        match ranges with
        | [] -> value
        | head :: tail ->
            match apply_conversion head value with
            | Some x -> x
            | None -> recurse tail
    in
    recurse map.conversions;;

let rec apply_maps (maps: map list) (value: int) : int =
    match maps with
    | map :: tail -> apply_map map value |> apply_maps tail
    | [] -> value;;

let part_one (lines: string list) : int =
    match lines with
    | [] -> 0
    | head :: tail ->
        let seeds = parse_seeds head in
        let conversions = parse_maps tail in
        seeds
        |> List.map (fun seed -> apply_maps conversions seed)
        |> List.sort compare
        |> List.hd;;

let parse_seed_ranges (line: string) : Range.range list =
    let rec recurse (ints: int list) : Range.range list =
        match ints with
        | start :: length :: tail -> {start=start; stop=(start + length -1)} :: recurse tail
        | _ -> []
    in
    parse_seeds line |> recurse;;

let apply_conversion_to_seed_range (conversion: conversion) (seed: Range.range) : (Range.range option) * (Range.range list) =
    let diff = conversion.transform in
    match conversion.range, seed with
    (* no overlap *)
    | c, s when s.stop < c.start || s.start > c.stop ->
        (None, [seed])
    (* overlaps range *)
    | c, s when s.start < c.start && s.stop > c.stop ->
        (Some (Range.init (s.start + diff) (s.stop + diff)),
        [
            Range.init s.start (s.start -1);
            Range.init (c.stop + 1) s.stop
        ])
    (* completely within range *)
    | c, s when s.start >= c.start && s.stop <= c.stop ->
        (Some (Range.init (s.start + diff) (s.stop + diff)), [])
    (* right side within range *)
    | c, s when s.start < c.start && s.stop <= c.stop ->
        (Some (Range.init (c.start + diff) (s.stop + diff)),
        [Range.init s.start (c.start - 1)]
    )
    (* left side within range *)
    | c, s when s.start <= c.stop && s.stop > c.stop ->
        (Some (Range.init (s.start + diff) (c.stop + diff)),
        [Range.init (c.stop + 1) s.stop]
    )
    | _ -> (None, []);;

let apply_map_to_seed_range (map: map) (seed: Range.range) : Range.range list =
    let rec recurse (conversions: conversion list) (seed: Range.range) : Range.range list =
        match conversions with
        | conv :: tail ->
            (match apply_conversion_to_seed_range conv seed with
            | (Some converted, seeds) -> converted :: (seeds |> List.map (recurse tail) |> List.concat)
            | None, seeds -> seeds |> List.map (recurse tail) |> List.concat)
        | [] -> [seed]
    in
    recurse map.conversions seed;;

let rec apply_maps_to_seed_ranges (maps: map list) (seeds: Range.range list) : Range.range list =
    match maps with
    | map :: tail ->
        seeds
        |> List.map (apply_map_to_seed_range map)
        |> List.concat
        |> apply_maps_to_seed_ranges tail
    | [] -> seeds;;

let part_two (lines: string list) : int =
    match lines with
    | [] -> 0
    | head :: tail ->
        let seed_ranges = parse_seed_ranges head in
        (* seed_ranges |> List.iter Range.print; *)
        let maps = tail |> parse_maps in
        apply_maps_to_seed_ranges maps seed_ranges
        |> List.map (fun (r: Range.range) -> r.start)
        |> List.sort compare
        |> List.hd;;

let run () =
    let input = File.read_lines "day05.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
