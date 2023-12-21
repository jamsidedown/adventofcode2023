type step =
    | Add of string * int
    | Remove of string

let hash (s: string) : int =
    let rec recurse (curr: int) (remaining: char list) : int =
        match remaining with
        | head :: tail ->
            let next = (curr + int_of_char head) * 17 in
            recurse (next mod 256) tail
        | [] -> curr
    in
    String.to_list s
    |> recurse 0;;

let part_one (sequence: string list) : int =
    sequence
    |> List.map hash
    |> List.sum;;

let parse (step: string) : step option =
    match String.split_on_char '=' step with
    | s :: i :: _ -> Some (Add (s, int_of_string i))
    | _ -> match String.split_on_char '-' step with
        | s :: _ -> Some (Remove s)
        | _ -> None;;

let replace (step: string*int) (sequence: (string*int) list) : (string*int) list =
    let key, _ = step in
    let rec recurse (seen: (string*int) list) (remaining: (string*int) list) : (string*int) list option =
        match remaining with
        | [] -> None
        | (s, _) :: tail when s = key -> Some ((List.rev seen) @ (step :: tail))   
        | head :: tail -> recurse (head :: seen) tail
    in
    match recurse [] sequence with
    | Some s -> s
    | None -> step :: sequence;;

let remove (key: string) (sequence: (string*int) list) : (string*int) list =
    let rec recurse (remaining: (string*int) list) : (string*int) list =
        match remaining with
        | [] -> []
        | (s, _) :: tail when s = key -> tail
        | head :: tail -> head :: recurse tail
    in
    recurse sequence;;

let part_two (sequence: string list) : int =
    let boxes = Hashtbl.create 256 in
    List.range 0 255 |> List.iter (fun i -> Hashtbl.add boxes i []);
    let rec recurse (sequence: step list) : unit =
        match sequence with
        | [] -> ()
        | head :: tail ->
            (match head with
            | Add (key, value) ->
                let hashed = hash key in
                let values = Hashtbl.find boxes hashed in
                Hashtbl.replace boxes hashed (replace (key, value) values)
            | Remove key ->
                let hashed = hash key in
                let values = Hashtbl.find boxes hashed in
                Hashtbl.replace boxes hashed (remove key values));
            recurse tail
    in
    sequence
    |> List.map parse
    |> List.choose
    |> recurse;

    List.range 0 255
    |> List.map (fun box_number ->
        let box = Hashtbl.find boxes box_number in
        box
        |> List.rev
        |> List.mapi (fun i (_, value) -> (i + 1) * value)
        |> List.sum
        |> ( * ) (box_number + 1))
    |> List.sum;;

let run () =
    let input = File.read_lines "day15.txt" |> List.hd |> String.split_on_char ',' in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
