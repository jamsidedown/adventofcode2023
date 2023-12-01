open Aoc23

module SolutionMap = Map.Make (String)

let solutions = [
    ("1", Day01.run);
    ("2", Day02.run);
]
|> List.to_seq
|> SolutionMap.of_seq

let () =
    (match Array.to_list Sys.argv with
    | [] -> []
    | _ :: [] ->
        (* run most recent day *)
        SolutionMap.to_list solutions
        |> List.last
        |> (function
            | Some recent -> [fst recent]
            | None -> [])
    | _ :: ["all"] ->
        SolutionMap.to_list solutions
        |> List.map fst
    | _ :: tail -> tail)
    |> List.iter (fun day ->
        match SolutionMap.find_opt day solutions with
        | Some day_module -> day_module()
        | None -> Printf.printf "No match for day %s\n" day)
