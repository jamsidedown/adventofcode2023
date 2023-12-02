open Operators

type round = {
    red: int;
    green: int;
    blue: int
}

type game = {
    game_id: int;
    rounds: round list
}

let parse_round (s: string) : round =
    let rec recurse (current: round) (parts: string list) : round =
        match parts with
        | [] -> current
        | head :: tail ->
            (match head |> String.trim |> String.split_on_char ' ' with
            | [ count; "red" ] -> recurse {current with red=(int_of_string count)} tail
            | [ count; "green" ] -> recurse {current with green=(int_of_string count)} tail
            | [ count; "blue" ] -> recurse {current with blue=(int_of_string count)} tail
            | _ -> recurse current tail)
    in
    String.split_on_char ',' s
    |> List.map String.trim
    |> recurse {red=0; green=0; blue=0};;

let parse_game (line: string) : game option =
    match String.split_on_char ':' line with
    | [ game_info; rounds_info ] ->
        let game_id = Scanf.sscanf game_info "Game %i" (fun x -> x) in
        (match String.split_on_char ';' rounds_info with
        | [] -> None
        | rounds_str ->
            let rounds = rounds_str |> List.map parse_round in
            Some { game_id=game_id; rounds=rounds })
    | _ -> None;;

let is_possible_game (game: game) (max_round: round) : bool =
    game.rounds
    |> List.for_all (fun round ->
        round.red <= max_round.red
        && round.green <= max_round.green
        && round.blue <= max_round.blue);;

let part_one (lines: string list) : int =
    let game_limit = { red=12; green=13; blue=14 } in
    lines
    |> List.map parse_game
    |> List.choose
    |> List.filter (fun game -> is_possible_game game game_limit)
    |> List.map (fun game -> game.game_id)
    |> List.sum;;

let max_round (first: round) (second: round) : round =
    {
        red=max first.red second.red;
        green=max first.green second.green;
        blue=max first.blue second.blue;
    }

let get_minumum_cubes (game: game) : round =
    game.rounds
    |> List.fold_left max_round { red=0; green=0; blue=0; };;

let get_power (round: round) : int =
    round.red * round.green * round.blue;;

let part_two (lines: string list) : int =
    lines
    |> List.map parse_game
    |> List.choose
    |> List.map (get_minumum_cubes >> get_power)
    |> List.sum;;

let run () =
    let input = File.read_lines "day02.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
