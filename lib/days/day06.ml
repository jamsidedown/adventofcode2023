let parse_numbers (line: string) : int list =
    let rec recurse (parts: string list) : int list =
        match parts with
        | "" :: tail -> recurse tail
        | head :: tail -> (int_of_string head) :: recurse tail
        | [] -> []
    in
    match String.split_on_char ':' line with
    | [_; ns]-> ns |> String.split_on_char ' ' |> recurse
    | _ -> [];;


let parse_single_number (line: string) : int option =
    match String.split_on_char ':' line with
    | [_; ns]->
        ns
        |> String.split_on_char ' '
        |> String.concat ""
        |> int_of_string_opt
    | _ -> None;;

let race (time_limit: int) (distance_record: int) : Range.range =
    let t = float_of_int time_limit in
    let d = float_of_int distance_record in
    let root = sqrt ((Float.pow t 2.) -. (4. *. d)) in
    let x1 = (t +. root) /. 2. in
    let x2 = (t -. root) /. 2. in
    let start = x2 |> int_of_float |> (fun x -> x + 1) in
    let stop = Float.ceil x1 |> int_of_float |> (fun x -> x - 1) in
    Range.init start stop;;

let part_one (lines: string list) : int =
    let rec recurse (times: int list) (distances: int list) : int =
        match times, distances with
        | (ht :: tt, hd :: td) ->
            let winning = race ht hd |> Range.length in
            winning * recurse tt td
        | _ -> 1
    in
    match lines with
    | [time_str; distance_str] ->
        let times = parse_numbers time_str in
        let distances = parse_numbers distance_str in
        recurse times distances
    | _ -> 0;;

let part_two (lines: string list) : int =
    match lines with
    | [time_str; distance_str] ->
        let time = parse_single_number time_str in
        let distance = parse_single_number distance_str in
        (match time, distance with
        | Some t, Some d -> race t d |> Range.length
        | _ -> 0)
    | _ -> 0;;

let run () =
    let input = File.read_lines "day06.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
