let rec parse_line ?shift:(shift=0) (line: char list) : int =
    match line with
    | [] -> 0
    | '#' :: tail -> Int.shift_left 1 shift + parse_line ~shift:(shift + 1) tail
    | _ :: tail -> parse_line ~shift:(shift + 1) tail;;

let rec split_groups ?current:(current=[]) (lines: string list) : string list list =
    match lines with
    | [] -> [List.rev current]
    | "" :: tail -> (List.rev current) :: split_groups tail
    | head :: tail -> split_groups ~current:(head :: current) tail;;

let mostly_equal (a: int list) (b: int list) : bool =
    let rec recurse (a: int list) (b: int list) : bool =
        match a, b with
        | [], _
        | _, [] -> true
        | ha :: ta, hb :: tb when ha = hb -> recurse ta tb
        | _ -> false
    in
    match a, b with
    | [], _
    | _, [] -> false
    | _ -> recurse a b;;

let is_symmetrical ?prev:(prev=(-1)) (values: int list) : int option =
    let rec recurse (seen: int list) (remaining: int list) : int option =
        match remaining with
        | [] -> None
        | head :: tail ->
            let len = List.length seen in
            if mostly_equal seen remaining && (len <> prev)
                then Some len
                else recurse (head :: seen) tail
    in
    recurse [] values;;

let find_symmetry (group: string list) : int =
    let chars = group |> List.map String.to_list in
    let horizontal = chars |> List.map parse_line in
    let vertical = chars |> List.transpose |> List.map parse_line in
    match is_symmetrical vertical with
    | Some x -> x
    | None ->
        match is_symmetrical horizontal with
        | Some x -> x * 100
        | None -> 0;;

let toggle (row: int) (col: int) (lst: int list) : int list =
    let rec recurse (seen: int list) (remaining: int list) : int list =
        if List.length seen = row then
            (List.rev seen) @ (Int.logxor (List.hd remaining) (Int.shift_left 1 col)) :: (List.tl remaining)
        else
            recurse (List.hd remaining :: seen) (List.tl remaining)
    in
    recurse [] lst;;

let toggle_bits (prev: int) (width: int) (height: int) (ints: int list) : int option =
    List.range 0 (width - 1)
    |> List.map (fun col ->
        List.range 0 (height - 1)
        |> List.map (fun row ->
            toggle row col ints
            |> is_symmetrical ~prev)
        |> List.choose)
    |> List.concat
    |> function
        | [] -> None
        | h :: _ -> Some h;;

let find_alternate_symmetry (group: string list) : int =
    let chars = group |> List.map String.to_list in
    let transposed = chars |> List.transpose in
    let width = chars |> List.hd |> List.length in
    let height = transposed |> List.hd |> List.length in
    let horizontal = chars |> List.map parse_line in
    let vertical = transposed |> List.map parse_line in
    let prev_vertical =
        match is_symmetrical vertical with
        | Some x -> x
        | None -> -1 in
    match toggle_bits prev_vertical height width vertical with
    | Some x -> x
    | None ->
        let prev_horizontal =
            match is_symmetrical horizontal with
            | Some x -> x
            | None -> -1 in
        match toggle_bits prev_horizontal width height horizontal with
        | Some x -> x * 100
        | None -> 0;;

let part_one (lines: string list) : int =
    let groups = split_groups lines in
    groups
    |> List.map find_symmetry
    |> List.sum;;

let part_two (lines: string list) : int =
    let groups = split_groups lines in
    groups
    |> List.map find_alternate_symmetry
    |> List.sum;;
    (* 22067 too low *)

let run () =
    let input = File.read_lines "day13.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
