include Stdlib.List

let id (x: 'a) : 'a = x;;

let rec last (lst: 'a list) : 'a option =
    match lst with
    | [] -> None
    | [ last' ] -> Some last'
    | _ :: t -> last t;;

let rec take (count: int) (lst: 'a list) : 'a list =
    if count < 1 then [] else
    match lst with
    | [] -> []
    | head :: tail -> head :: take (count - 1) tail;;

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

let min (lst: int list) : int option =
    let rec recurse (curr: int) (remaining: int list) : int =
        match remaining with
        | [] -> curr
        | head :: tail -> recurse (min head curr) tail
    in
    match lst with
    | [] -> None
    | head :: tail -> Some (recurse head tail);;

let min_of (transform: 'a -> int) (lst: 'a list) : int option =
    lst
    |> map transform
    |> min;;

let max (lst: int list) : int option =
    let rec recurse (curr: int) (remaining: int list) : int =
        match remaining with
        | [] -> curr
        | head :: tail -> recurse (max head curr) tail
    in
    match lst with
    | [] -> None
    | head :: tail -> Some (recurse head tail);;

let max_of (transform: 'a -> int) (lst: 'a list) : int option =
    lst
    |> map transform
    |> max;;

let transpose (lst: 'a list list) : 'a list list =
    let rec recurse (col: 'a list) (seen: 'a list list) (remaining: 'a list list) : 'a list list =
        match remaining with
        | [] -> rev col :: recurse [] [] (rev seen)
        | [] :: _ -> []
        | (head :: tail) :: rem -> recurse (head :: col) (tail :: seen) rem
    in
    recurse [] [] lst;;

let rotate_cw (lst: 'a list list) : 'a list list =
    let rec recurse (col: 'a list) (seen: 'a list list) (remaining: 'a list list) : 'a list list =
        match remaining with
        | [] -> col :: recurse [] [] (rev seen)
        | [] :: _ -> []
        | (head :: tail) :: rem -> recurse (head :: col) (tail :: seen) rem
    in
    recurse [] [] lst;;

let rotate_ccw (lst: 'a list list) : 'a list list =
    lst
    |> transpose
    |> rev;;

(*
   1 2 3
   4 5 6
   7 8 9

   3 6 9
   2 5 8
   1 4 7
*)