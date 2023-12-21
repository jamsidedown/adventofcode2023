open Aoc23

module SolutionMap = Map.Make (String)

let solution_list = [
    ("1", Day01.run);
    ("2", Day02.run);
    ("3", Day03.run);
    ("4", Day04.run);
    ("5", Day05.run);
    ("6", Day06.run);
    ("7", Day07.run);
    ("8", Day08.run);
    ("9", Day09.run);
    ("10", Day10.run);
    ("11", Day11.run);
    ("12", Day12.run);
    ("13", Day13.run);
    ("14", Day14.run);
    ("15", Day15.run);
];;

let solutions =
    solution_list
    |> List.to_seq
    |> SolutionMap.of_seq;;

let () =
    (match Array.to_list Sys.argv with
    | [] -> []
    | _ :: [] ->
        (* run most recent day *)
        solution_list
        |> List.last
        |> (function
            | Some recent -> [fst recent]
            | None -> [])
    | _ :: ["all"] ->
        solution_list
        |> List.map fst
    | _ :: tail -> tail)
    |> List.iter (fun day ->
        match SolutionMap.find_opt day solutions with
        | Some day_module ->
            Printf.printf "Day %s\n" day;
            day_module()
        | None -> Printf.printf "No match for day %s\n" day)
