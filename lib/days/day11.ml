open Coord

type coord = Coord.t

let parse_line (y: int) (line: string) : coord list =
    line
    |> String.to_list
    |> List.mapi (fun x c ->
        match c with
        | '#' ->
            let coord: coord = {x=x; y=y} in
            Some coord
        | _ -> None)
    |> List.choose;;

let parse_galaxies (lines: string list) : coord list =
    lines
    |> List.mapi parse_line
    |> List.concat;;

let shift_x (factor: int) (after: int) (galaxies: coord list) : coord list =
    galaxies
    |> List.map (fun (c: coord) ->
        if c.x > after then {c with x=(c.x + factor)} else c);;

let shift_y (factor: int) (after: int) (galaxies: coord list) : coord list =
    galaxies
    |> List.map (fun (c: coord) ->
        if c.y > after then {c with y=(c.y + factor)} else c);;

let rec expand_x (factor: int) (x: int) (max_x: int) (galaxies: coord list) =
    if x >= max_x then galaxies else
    if List.for_all (fun (c: coord) -> c.x <> x) galaxies
        then expand_x factor (x + factor + 1) (max_x + factor) (shift_x factor x galaxies)
        else expand_x factor (x + 1) max_x galaxies;;

let rec expand_y (factor: int) (y: int) (max_y: int) (galaxies: coord list) =
    if y >= max_y then galaxies else
    if List.for_all (fun (c: coord) -> c.y <> y) galaxies
        then expand_y factor (y + factor + 1) (max_y + factor) (shift_y factor y galaxies)
        else expand_y factor (y + 1) max_y galaxies;;

let expand_galaxies ?factor:(factor=2) (galaxies: coord list) : coord list =
    let max_x = galaxies |> List.max_of (fun (c: coord) -> c.x) in
    let max_y = galaxies |> List.max_of (fun (c: coord) -> c.y) in
    match (max_x, max_y) with
    | Some x, Some y ->
        let expanded_x = expand_x (factor - 1) 0 x galaxies in
        let expanded_y = expand_y (factor - 1) 0 y expanded_x in
        expanded_y
    | _ -> galaxies;;
 
let part_one (lines: string list) : int =
    let rec recurse (galaxies: coord list) : int =
        match galaxies with
        | [] -> 0
        | head :: tail ->
            let sum_from_head =
                tail
                |> List.map (Coord.distance head)
                |> List.sum in
            sum_from_head + recurse tail
    in
    lines
    |> parse_galaxies
    |> expand_galaxies
    |> recurse;;

let part_two ?factor:(factor=1_000_000) (lines: string list) : int =
    let rec recurse (galaxies: coord list) : int =
        match galaxies with
        | [] -> 0
        | head :: tail ->
            let sum_from_head =
                tail
                |> List.map (Coord.distance head)
                |> List.sum in
            sum_from_head + recurse tail
    in
    lines
    |> parse_galaxies
    |> expand_galaxies ~factor:factor
    |> recurse;;

let run () =
    let input = File.read_lines "day11.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
