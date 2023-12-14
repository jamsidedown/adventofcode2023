include Stdlib.String

let to_list (s: string) : char list =
    s
    |> to_seq
    |> List.of_seq;;

let of_list (cs: char list) : string =
    cs
    |> List.to_seq
    |> Stdlib.String.of_seq;;

let get_all_matches (pattern: Str.regexp) (input: string) : string list =
    let rec recurse (start: int) : string list =
        match Str.search_forward pattern input start with
        | i ->
            let matched = Str.matched_string input in
            let length = Stdlib.String.length matched in
            matched :: recurse (i + length)
        | exception Not_found -> []
    in
    recurse 0;;

let repeat (times: int) (connector: string) (s: string) : string =
    List.range 1 times
    |> List.map (fun _ -> s)
    |> Stdlib.String.concat connector;;
