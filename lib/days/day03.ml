type number = {
    value: int;
    line: int;
    start: int;
    stop: int;
}

type symbol = {
    symbol: char;
    line: int;
    position: int;
}

let read_numbers (line: string) (line_number: int) : number list =
    let rec recurse (numbers: number list) (current: char list) (position: int) (start: int) (remaining: char list) : number list =
        match remaining with
        | [] ->
            (match current with
            | [] -> List.rev numbers
            | _ ->
                let number = {
                    value=(current |> List.rev |> Int.of_chars);
                    line=line_number;
                    start=start;
                    stop=(position - 1)
                } in
                List.rev (number :: numbers))
        | head :: tail when Char.is_int head ->
            (match current with
            | [] -> recurse numbers [head] (position + 1) position tail
            | _ -> recurse numbers (head :: current) (position + 1) start tail)
        | _ :: tail ->
            (match current with
            | [] -> recurse numbers current (position + 1) 0 tail
            | _ ->
                let number = {
                    value=(current |> List.rev |> Int.of_chars);
                    line=line_number;
                    start=start;
                    stop=(position - 1)
                } in
                recurse (number :: numbers) [] (position + 1) 0 tail)
    in
    recurse [] [] 0 0 (String.to_list line);;

let read_symbols (line: string) (line_number: int) : symbol list =
    let rec recurse (symbols: symbol list) (position: int) (remaining: char list) : symbol list =
        match remaining with
        | [] -> List.rev symbols
        | head :: tail when Char.is_int head -> recurse symbols (position + 1) tail
        | '.' :: tail -> recurse symbols (position + 1) tail
        | head :: tail ->
            let sym = {symbol=head; line=line_number; position=position} in
            recurse (sym :: symbols) (position + 1) tail
    in
    recurse [] 0 (String.to_list line);;

let coords_of_number (number: number) : Coord.coord list =
    List.range number.start number.stop
    |> List.map (fun x ->
        let c: Coord.coord = {x=x; y=number.line} in
        c);;

let coord_of_symbol (symbol: symbol) : Coord.coord =
    {x=symbol.position; y=symbol.line};;

let sum_numbers (numbers: number list) : int =
    numbers
    |> List.map (fun n -> n.value)
    |> List.sum;;

let not_adjacent (symbols: symbol list) (numbers: number list) : number list =
    let symbol_coords = symbols |> List.map coord_of_symbol in
    numbers
    |> List.filter (fun n ->
        coords_of_number n
        |> List.exists (fun nc ->
            symbol_coords
            |> List.exists (fun sc -> Coord.adjacent nc sc))
        |> not);;

let part_one (lines: string list) : int =
    let rec recurse (prev_numbers: number list) (prev_symbols: symbol list) (sum: int) (line_number: int) (remaining_lines: string list) : int =
        match remaining_lines with
        | [] ->
            let remaining_sum = sum_numbers prev_numbers in
            sum - remaining_sum
        | line :: tail ->
            let curr_numbers = read_numbers line line_number in
            let curr_symbols = read_symbols line line_number in
            let next_numbers =
                curr_numbers
                |> not_adjacent prev_symbols
                |> not_adjacent curr_symbols in

            let line_sum = sum_numbers curr_numbers in
            let prev_not_part_sum = not_adjacent curr_symbols prev_numbers |> sum_numbers in

            recurse next_numbers curr_symbols (sum + line_sum - prev_not_part_sum) (line_number + 1) tail
        in
    recurse [] [] 0 0 lines;;

let get_gear_value (gear: symbol) (numbers: number list) : int =
    let gear_coord = coord_of_symbol gear in
    let adjacent_numbers = 
        numbers
        |> List.filter (fun n ->
            coords_of_number n
            |> List.exists (fun nc -> Coord.adjacent gear_coord nc))
    in
    match adjacent_numbers with
    | [a; b] -> a.value * b.value
    | _ -> 0;;

let find_gears (numbers: number list) (symbols: symbol list) : int list =
    symbols
    |> List.filter (fun s -> s.symbol = '*')
    |> List.map (fun s -> get_gear_value s numbers);;

let part_two (lines: string list) : int =
    let numbers =
        lines
        |> List.mapi (fun i line -> read_numbers line i)
        |> List.concat in
    let symbols =
        lines
        |> List.mapi (fun i line -> read_symbols line i)
        |> List.concat in
    find_gears numbers symbols
    |> List.sum;;

let run () =
    let input = File.read_lines "day03.txt" in
    input |> part_one |> Printf.printf "Part one: %i\n";
    input |> part_two |> Printf.printf "Part two: %i\n";;
