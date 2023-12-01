let part_one (lines: string list) : int =
    List.length lines;;

let run () =
    let input = File.read_lines "day02.txt" in
    input |> List.iter (print_endline);;
