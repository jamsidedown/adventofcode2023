type coord = {
    x: int;
    y: int;
}

let adjacent (a: coord) (b: coord) : bool =
    let distance = max (abs (a.x - b.x)) (abs (a.y - b.y)) in
    distance <= 1;;
