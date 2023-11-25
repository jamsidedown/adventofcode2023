open Aoc23;;

let run () =
    let data = Parsing.read_lines "day01.txt" in
    data |> String.concat "\n" |> print_endline;;
