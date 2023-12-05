type range = {
    start: int;
    stop: int;
}

let init (start: int) (stop: int) : range =
    {start=start; stop=stop};;

let init_opt (start: int) (stop: int) : range option =
    if start <= stop
    then Some (init start stop)
    else None;;

let from_start (start: int) (length: int) : range =
    {start=start; stop=(start + length - 1)};;

let length (range: range) : int =
    range.stop - range.start + 1;;

let contains (value: int) (range: range) : bool =
    value >= range.start && value <= range.stop;;

let overlaps (first: range) (second: range) : bool =
    (first.start >= second.start && first.start <= second.stop)
    || (second.start >= first.start && second.start <= first.stop);;

let neighbours (first: range) (second: range) : bool =
    first.stop = (second.start - 1) || second.stop = (first.start - 1);;

let combine (first: range) (second: range) : range option =
    if overlaps first second || neighbours first second
    then Some {start=(min first.start second.start); stop=(max first.stop second.stop)}
    else None;;

let range_compare (a: range) (b: range) : int =
    compare a.start b.start;;

let rec reduce (ranges: range list) : range list =
    let initial_length = List.length ranges in

    let rec recurse (remaining: range list) : range list =
        match remaining with
        | a :: b :: tail ->
            (match combine a b with
            | Some c -> c :: recurse tail
            | None -> a :: recurse (b :: tail))
        | _ -> remaining
    in
    let reduced =
        ranges
        |> List.sort range_compare
        |> recurse in
    if List.length reduced = initial_length
    then reduced
    else reduce reduced;;

let to_list (range: range) : int list =
    List.range range.start range.stop;;

let print (range: range) : unit =
    Printf.printf "Range %i->%i\n" range.start range.stop;;
