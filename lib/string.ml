include Stdlib.String

let to_list (s: string) : char list =
    s
    |> to_seq
    |> List.of_seq;;