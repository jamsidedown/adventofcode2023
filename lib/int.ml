include Stdlib.Int

let of_chars (chars: char list) : int =
    chars
    |> List.filter Char.is_int
    |> function
        | [] -> 0
        | lst ->
            lst
            |> List.to_seq
            |> String.of_seq
            |> int_of_string;;

let print_list (lst: int list) : unit =
    lst
    |> List.map string_of_int
    |> String.concat ", "
    |> print_endline;;
