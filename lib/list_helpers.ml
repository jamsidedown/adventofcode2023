let rec last (lst: 'a list) : 'a option =
    match lst with
    | [] -> None
    | [ last' ] -> Some last'
    | _ :: t -> last t;;

let to_chars (s: string) : char list =
    List.init (String.length s) (String.get s);;

let rec choose (lst: 'a option list) : 'a list =
    match lst with
    | [] -> []
    | Some head :: tail -> head :: choose tail
    | None :: tail -> choose tail;;

let rec sum (lst: int list) : int =
    match lst with
    | [] -> 0
    | head :: tail -> head + sum tail;;
