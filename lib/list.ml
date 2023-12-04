include Stdlib.List

let id (x: 'a) : 'a = x;;

let rec last (lst: 'a list) : 'a option =
    match lst with
    | [] -> None
    | [ last' ] -> Some last'
    | _ :: t -> last t;;

let rec choose ?f:(f=id) (lst: 'a option list) : 'a list =
    match lst with
    | [] -> []
    | Some head :: tail -> f head :: choose ~f:f tail
    | None :: tail -> choose ~f:f tail;;

let sum (lst: int list) : int =
    let rec recurse (acc: int) (remaining: int list) : int =
        match remaining with
        | [] -> acc
        | head :: tail -> recurse (acc + head) tail
    in
    recurse 0 lst;;

let[@tail_mod_cons] rec range (start: int) (stop: int) : int list =
    if start > stop then [] else start :: range (start + 1) stop;;
