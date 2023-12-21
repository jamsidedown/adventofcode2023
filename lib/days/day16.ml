open Coord

type coord = Coord.t
type contraption = (Coord.t, char) Hashtbl.t

module CoordSet = Set.Make(Coord)

type single_or_double =
    | Single of coord
    | Double of coord*coord

let parse (lines: string list) : contraption =
    let height = List.length lines in
    let width = List.hd lines |> String.length in
    let contraption = Hashtbl.create (width * height) in
    lines
    |> List.mapi (fun y line ->
        line
        |> String.to_list
        |> List.mapi (fun x c ->
            let coord: coord = {x=x; y=y} in
            (coord, c)))
    |> List.concat
    |> List.iter (fun (coord, c) ->
        Hashtbl.add contraption coord c);
    contraption;;

let encounter (cell: char) (vector: coord) : single_or_double =
    match cell, vector with
    | '|', {x=1; y=0}
    | '|', {x=(-1); y=0} -> Double ({x=0; y=1}, {x=0; y=(-1)})
    | '-', {x=0; y=1}
    | '-', {x=0; y=(-1)} -> Double ({x=1; y=0}, {x=(-1); y=0})
    | '/', {x=1; y=0}
    | '\\', {x=(-1); y=0} -> Single {x=0; y=(-1)}
    | '/', {x=0; y=1}
    | '\\', {x=0; y=(-1)} -> Single {x=(-1); y=0}
    | '/', {x=(-1); y=0}
    | '\\', {x=1; y=0} -> Single {x=0; y=1}
    | '/', {x=0; y=(-1)}
    | '\\', {x=0; y=1} -> Single {x=1; y=0}
    | _ -> Single vector;;

let trace_beam (start: coord) (start_vector: coord) (contraption: contraption) : coord list =
    let seen : (coord, CoordSet.t) Hashtbl.t = Hashtbl.create 1024 in
    let rec recurse (coord: coord) (vector: coord) : unit =
        match Hashtbl.find_opt contraption coord with
        | None -> ()
        | Some cell ->
            let stop =
                match Hashtbl.find_opt seen coord with
                | None ->
                    Hashtbl.add seen coord (CoordSet.add vector CoordSet.empty);
                    false
                | Some set when CoordSet.mem vector set -> true
                | Some set ->
                    Hashtbl.replace seen coord (CoordSet.add vector set);
                    false in
            if stop then () else
            match encounter cell vector with
            | Single v -> recurse (Coord.add coord v) v
            | Double (v1, v2) ->
                recurse (Coord.add coord v1) v1;
                recurse (Coord.add coord v2) v2
    in
    recurse start start_vector;
    seen
    |> Hashtbl.to_seq_keys
    |> List.of_seq;;

let part_one (lines: string list) : int =
    let contraption = parse lines in
    trace_beam Coord.origin {x=1; y=0} contraption
    |> List.length;;

let part_two (lines: string list) : int =
    let contraption = parse lines in
    let height = List.length lines in
    let width = List.hd lines |> String.length in
    let max_top =
        List.range 0 (width - 1)
        |> List.map (fun x ->
            trace_beam {x=x; y=0} {x=0; y=1} contraption
            |> List.length)
        |> List.max in
    let max_right =
        List.range 0 (height - 1)
        |> List.map (fun y ->
            trace_beam {x=(width - 1); y=y} {x=(-1); y=0} contraption
            |> List.length)
        |> List.max in
    let max_bottom =
        List.range 0 (width - 1)
        |> List.map (fun x ->
            trace_beam {x=x; y=(height - 1)} {x=0; y=(-1)} contraption
            |> List.length)
        |> List.max in
    let max_left =
        List.range 0 (height - 1)
        |> List.map (fun y ->
            trace_beam {x=0; y=y} {x=1; y=0} contraption
            |> List.length)
        |> List.max in
    [max_top; max_right; max_bottom; max_left]
    |> List.choose
    |> List.max
    |> Option.get;;

let run () : unit =
    let input = File.read_lines "day16.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
