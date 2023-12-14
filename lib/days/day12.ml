type spring = Operational | Damaged | Unknown
type springs = spring list

let print_spring (springs: springs) : unit =
    let rec to_chars (remaining: springs) : char list =
        match remaining with
        | Operational :: tail -> '.' :: to_chars tail
        | Damaged :: tail -> '#' :: to_chars tail
        | Unknown :: tail -> '?' :: to_chars tail
        | [] -> []
    in
    to_chars springs
    |> String.of_list
    |> print_endline;;

let parse_springs (springs: string) : springs =
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

let parse_line ?repeat:(repeat=1) (line: string) : springs * (int list) =
    match String.split_on_char ' ' line with
    | [springs; groups] ->
        let springs_rep = String.repeat repeat "?" springs in
        let groups_rep = String.repeat repeat "," groups in
        (parse_springs springs_rep, parse_groups groups_rep)
    | _ -> [], [];;

let rec matches_springs (truth: springs) (to_test: springs) : bool =
    match truth, to_test with
    | Operational :: truth_tail, Operational :: to_test_tail -> matches_springs truth_tail to_test_tail
    | Damaged :: truth_tail, Damaged :: to_test_tail -> matches_springs truth_tail to_test_tail
    | Unknown :: truth_tail, _ :: to_test_tail -> matches_springs truth_tail to_test_tail
    | [], [] -> true
    | _ -> false;;

let rec matches_groups ?current:(current=0) (groups: int list) (springs: springs) : bool =
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
        | _ -> false)
    | _ -> false;;

let generate_matches (template: springs) (groups: int list) : springs list =
    let rec recurse (springs: springs) ?group:(group=0) (groups: int list) : springs list =
        if group < 1 then
            if springs = [] then
                if groups = [] then [[]] else []
            else
                match groups with
                | [] ->
                    if List.for_all (fun s -> s = Operational || s = Unknown) springs then
                        [springs |> List.map (fun _ -> Operational)]
                    else []
                | g :: gt ->
                    (match springs with
                    | Operational :: st -> recurse st groups |> List.map (fun lst -> Operational :: lst)
                    | Damaged :: st -> recurse st ~group:(g-1) gt |> List.map (fun lst -> Damaged :: lst)
                    | Unknown :: st ->
                        let op = recurse st groups |> List.map (fun lst -> Operational :: lst) in
                        let dmg = recurse st ~group:(g-1) gt |> List.map (fun lst -> Damaged :: lst) in
                        op @ dmg
                    | _ ->
                        print_endline "Shouldn't be here 1";
                        [])
        else
            if springs = [] then [] else
                match group with
                | 1 ->
                    (match springs with
                    | Damaged :: Operational :: st
                    | Damaged :: Unknown :: st
                    | Unknown :: Operational :: st
                    | Unknown :: Unknown :: st -> recurse st groups |> List.map (fun lst -> Damaged :: Operational :: lst)
                    | [Damaged] | [Unknown] when groups = []-> [[Damaged]]
                    | _ -> [])
                | g ->
                    (match springs with
                    | Damaged :: st
                    | Unknown :: st -> recurse st ~group:(g-1) groups |> List.map (fun lst -> Damaged :: lst)
                    | _ -> [])
    in
    recurse template groups
    |> List.filter (matches_groups groups);;

let part_one (lines: string list) : int =
    lines
    |> List.map (fun line ->
        let springs, groups = parse_line line in
        generate_matches springs groups)
    |> List.concat
    |> List.length;;

let part_two (lines: string list) : int =
    lines
    |> List.mapi (fun i line ->
        let springs, groups = parse_line ~repeat:5 line in
        let result = generate_matches springs groups in
        let length = List.length result in
        Printf.printf "Finished line %i, %i arrangements\n%!" i length;
        length)
    |> List.sum;;

let run () =
    let input = File.read_lines "day12.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
