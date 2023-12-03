include Stdlib.String

let to_list (s: string) : char list =
    s
    |> to_seq
    |> List.of_seq;;

let of_list (cs: char list) : string =
    cs
    |> List.to_seq
    |> Stdlib.String.of_seq;;