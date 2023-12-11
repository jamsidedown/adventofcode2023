module Coord = struct
    type t = {x: int; y: int}

    let origin = {x=0; y=0};;

    let compare (a: t) (b: t) =
        match compare a.x b.x with
        | 0 -> compare a.y b.y
        | c -> c;;

    let adjacent (a: t) (b: t) : bool =
        let distance = max (abs (a.x - b.x)) (abs (a.y - b.y)) in
        distance <= 1;;

    let add (a: t) (b: t) : t =
        {x=a.x + b.x; y=a.y + b.y};;

    let minus (a: t) (b: t) =
        {x=a.x - b.x; y=a.y - b.y};;

    let to_string (c: t) : string =
        Printf.sprintf "(%i, %i)" c.x c.y;;
end
