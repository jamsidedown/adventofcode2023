type spring = Operational | Damaged | Unknown

let print_spring (springs: spring list) : unit =
    let rec to_chars (remaining: spring list) : char list =
        match remaining with
        | Operational :: tail -> '.' :: to_chars tail
        | Damaged :: tail -> '#' :: to_chars tail
        | Unknown :: tail -> '?' :: to_chars tail
        | [] -> []
    in
    to_chars springs
    |> String.of_list
    |> print_endline;;

let parse_springs (springs: string) : spring list =
    springs
    |> String.to_list
    |> List.map (fun c ->
        match c with
        | '.' -> Some Operational
        | '#' -> Some Damaged
        | '?' -> Some Unknown
        | _ -> None)
    |> List.choose;;

let parse_groups (groups: string) : int list =
    groups
    |> String.split_on_char ','
    |> List.map int_of_string;;

let parse_line (line: string) : ((spring list) * (int list)) =
    match String.split_on_char ' ' line with
    | [springs; groups] -> (parse_springs springs, parse_groups groups)
    | _ -> [], [];;

let rec matches_springs (truth: spring list) (to_test: spring list) : bool =
    match truth, to_test with
    | Operational :: truth_tail, Operational :: to_test_tail -> matches_springs truth_tail to_test_tail
    | Damaged :: truth_tail, Damaged :: to_test_tail -> matches_springs truth_tail to_test_tail
    | Unknown :: truth_tail, _ :: to_test_tail -> matches_springs truth_tail to_test_tail
    | [], [] -> true
    | _ -> false;;

let rec matches_groups ?current:(current=0) (groups: int list) (springs: spring list) : bool =
    match current, springs with
    | 1, Damaged :: Operational :: tail
    | 1, Unknown :: Operational :: tail -> matches_groups groups tail
    | 1, [Damaged]
    | 1, [Unknown] -> groups = []
    | 1, _ -> false
    | curr, Damaged :: tail
    | curr, Unknown :: tail when curr > 1 -> matches_groups ~current:(current - 1) groups tail
    | curr, _ when curr > 1 -> false
    | 0, _ ->
        (match groups, springs with
        | [], [] -> true
        | [], st when List.for_all (fun s -> s = Operational) st -> true
        | [1], [Damaged] -> true
        | 1 :: gt, Damaged :: Operational :: st -> matches_groups gt st
        | g :: gt, Damaged :: st when g > 1 -> matches_groups ~current:(g - 1) gt st
        | _ :: _, Operational :: st -> matches_groups groups st
        | _, [] -> false
        | _ -> false)
    | _ -> false;;

let generate_matches (template: spring list) (groups: int list) : spring list list =
    let rec recurse (springs: spring list) (remaining: int) : spring list list =
        match springs with
        | Operational :: tail -> recurse tail remaining |> List.map (fun lst -> Operational :: lst)
        | Damaged :: _ when remaining < 1 -> []
        | Damaged :: tail -> recurse tail (remaining - 1) |> List.map (fun lst -> Damaged :: lst)
        | Unknown :: tail ->
            let op = recurse tail remaining |> List.map (fun lst -> Operational :: lst) in
            let dmg = recurse tail (remaining - 1) |> List.map (fun lst -> Damaged :: lst) in
            op @ dmg
        | [] -> [[]]
    in
    recurse template (List.sum groups)
    |> List.filter (matches_groups groups);;

let part_one (lines: string list) : int =
    lines
    |> List.map (fun line ->
        let springs, groups = parse_line line in
        generate_matches springs groups)
    |> List.concat
    |> List.length;;

let part_two (_: string list) : int =
    0;;

let run () =
    let input = File.read_lines "day12.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
