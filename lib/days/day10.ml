open Coord

module CoordSet = Set.Make (Coord)

type coord = Coord.t
type direction = N | E | S | W
type pipe = NE | NS | NW | SE | EW | SW
type cell = Empty | Start | Pipe of pipe
type grid = (coord, cell) Hashtbl.t

let to_delta : direction -> coord =
    function
    | N -> {x=0; y=(-1)}
    | E -> {x=1; y=0}
    | S -> {x=0; y=1}
    | W -> {x=(-1); y=0};;

let parse_cell (c: char) : cell =
    match c with
    | 'L' -> Pipe NE
    | '|' -> Pipe NS
    | 'J' -> Pipe NW
    | 'F' -> Pipe SE
    | '-' -> Pipe EW
    | '7' -> Pipe SW
    | 'S' -> Start
    |_ -> Empty

let parse_grid (lines: string list) : grid =
    let grid = Hashtbl.create (Math.pow(List.length lines) 2) in
    lines
    |> List.iteri (fun y line ->
        line
        |> String.to_list
        |> List.iteri (fun x c ->
            let cell = parse_cell c in
            let coord: coord = {x=x; y=y} in
            Hashtbl.add grid coord cell));
    grid;;

let find_in_grid (grid: grid) (cell: cell) : coord option =
    let matches =
        Hashtbl.to_seq grid
        |> Seq.filter (fun (_, c) -> c = cell)
        |> List.of_seq in
    match matches with
    | [] -> None
    | (coord, _) :: _ -> Some coord;;

let next_direction (src: direction) (dest: pipe) : direction option =
    match (src, dest) with
    | N, SE -> Some E
    | N, NS -> Some N
    | N, SW -> Some W
    | E, NW -> Some N
    | E, SW -> Some S
    | E, EW -> Some E
    | S, NS -> Some S
    | S, NE -> Some E
    | S, NW -> Some W
    | W, NE -> Some N
    | W, EW -> Some W
    | W, SE -> Some S
    | _ -> None;;

let step (grid: grid) (start: coord) (direction: direction) : coord option * direction option =
    let delta = to_delta direction in
    let next_coord = Coord.add start delta in
    let start_cell = Hashtbl.find grid start in
    if start_cell = Empty then None, None else
    match Hashtbl.find_opt grid next_coord with
    | None | Some Empty -> None, None
    | Some Start -> Some next_coord, None
    | Some (Pipe pipe) ->
        (match next_direction direction pipe with
        | None -> None, None
        | Some dir -> Some next_coord, Some dir);;

let rec follow (grid: grid) (current: coord) (dir: direction) : coord list =
    match step grid current dir with
    | Some start, None -> [start]
    | Some coord, Some direction -> coord :: follow grid coord direction
    | _ -> [];;

let count_cw (a: direction) (b: direction) : int =
    match a, b with
    | N, E | E, S | S, W | W, N -> 1
    | N, W | E, N | S, E | W, S -> -1
    | _ -> 0;;

let rec count_turns (grid: grid) (current: coord) (dir: direction) : int =
    match step grid current dir with
    | Some coord, Some next_dir -> count_cw dir next_dir + count_turns grid coord next_dir
    | _ -> 0;;

let turn_cw : direction -> direction = function
    | N -> E
    | E -> S
    | S -> W
    | W -> N;;

let get_cw_inside_turn (coord: coord) (from_dir: direction) (to_dir: direction) : coord list =
    match from_dir, to_dir with
    | N, N -> Coord.add (to_delta E) coord :: []
    | E, E -> Coord.add (to_delta S) coord :: []
    | S, S -> Coord.add (to_delta W) coord :: []
    | W, W -> Coord.add (to_delta N) coord :: []
    | N, W -> Coord.add (to_delta N) coord :: Coord.add (to_delta E) coord :: []
    | E, N -> Coord.add (to_delta E) coord :: Coord.add (to_delta S) coord :: []
    | S, E -> Coord.add (to_delta S) coord :: Coord.add (to_delta W) coord :: []
    | W, S -> Coord.add (to_delta N) coord :: Coord.add (to_delta W) coord :: []
    | _ -> []

let get_potential_cw_inside (grid: grid) (start: coord) (dir: direction) : coord list =
    let rec recurse (current: coord) (dir: direction) (prev_dir: direction) : coord list =
        match step grid current dir with
        | Some next_coord, Some next_dir ->
            (match get_cw_inside_turn current prev_dir dir with
            | [c] -> c :: recurse next_coord next_dir dir
            | [a; b] -> a :: b :: recurse next_coord next_dir dir
            | _ -> recurse next_coord next_dir dir)
        | _ -> []
    in
    recurse start dir dir;;

let neighbours (coord: coord) : coord list =
    [
        {x=coord.x + 1; y=coord.y};
        {x=coord.x; y=coord.y + 1};
        {x=coord.x - 1; y=coord.y};
        {x=coord.x; y=coord.y - 1};
    ];;

let find_area (grid: grid) (loop: coord list) (start: coord) : coord list =
    (* mutable values are easier in F# *)
    let area = Hashtbl.create 1024 in
    let loop_set = CoordSet.of_list loop in
    let rec recurse (current: coord) : unit =
        let in_grid = Hashtbl.mem grid current in
        let not_in_loop = CoordSet.mem current loop_set |> not in
        let not_in_area = Hashtbl.mem area current |> not in
        match in_grid && not_in_loop && not_in_area with
        | true ->
            Hashtbl.add area current ();
            neighbours current
            |> List.iter recurse
        | false -> ()
    in
    recurse start;
    Hashtbl.to_seq_keys area
    |> List.of_seq;;

let rec find_loop ?direction:(direction=N) (grid: grid) (start: coord) : coord list =
    match direction, follow grid start direction with
    | W, [] -> []
    | _, [] -> find_loop ~direction:(turn_cw direction) grid start
    | _, coords -> coords;;

let find_start_direction (start: coord) (next: coord) : direction =
    match Coord.minus next start with
    | {x=0; y=1} -> S
    | {x=0; y=(-1)} -> N
    | {x=1; y=0} -> E
    | _ -> W;;

let rec find_cw_loop ?direction:(direction=N) (grid: grid) (start: coord) : coord list =
    match direction, follow grid start direction with
    | W, [] -> []
    | _, [] -> find_cw_loop ~direction:(turn_cw direction) grid start
    | _, coords ->
        let turns = count_turns grid start direction in
        if turns > 0 then coords
        else
            (* Start is at the end, that needs to continue to be true *)
            let reversed = List.rev coords in
            List.tl reversed @ [List.hd reversed];;

let part_one (lines: string list) : int =
    let grid = parse_grid lines in
    let start = find_in_grid grid Start |> Option.get in
    find_loop grid start
    |> List.length
    |> fun x -> x / 2;;

let part_two (lines: string list) : int =
    let grid = parse_grid lines in
    let start = find_in_grid grid Start |> Option.get in
    let loop = find_cw_loop grid start in

    let initial_dir = find_start_direction start (List.hd loop) in
    let potential_inside = get_potential_cw_inside grid start initial_dir in

    let potential_set = CoordSet.of_list potential_inside in
    let grid_set = CoordSet.of_seq (Hashtbl.to_seq_keys grid) in
    let potential_set_in_grid = CoordSet.inter potential_set grid_set in
    let inside_set = CoordSet.diff potential_set_in_grid (CoordSet.of_list loop) in

    let inside_areas =
        inside_set
        |> CoordSet.to_list
        |> List.map (find_area grid loop)
        |> List.concat
        |> CoordSet.of_list
        |> CoordSet.to_list in
    List.length inside_areas;;

let run () =
    let input = File.read_lines "day10.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
