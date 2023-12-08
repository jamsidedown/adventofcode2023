module IntSet = Set.Make (Int)

let rec pow (x: int) (exponent: int) : int =
    match exponent with
    | 0 -> 1
    | n when n > 0 -> x * (pow x (n - 1))
    | _ -> 0;;

let rec factors ?factor:(factor=2) (current: int) : int list =
    match current with
    | c when c <= 1 -> []
    | c when c mod factor = 0 -> factor :: factors ~factor:factor (c / factor)
    | c -> factors ~factor:(factor + 1) c;;

let lcm (a: int) (b: int) : int =
    let a_factors = factors a |> IntSet.of_list in
    let b_factors = factors b |> IntSet.of_list in
    let common_factors = IntSet.inter a_factors b_factors in
    let rec recurse (current: int) (remaining_factors: int list) : int =
        match remaining_factors with
        | [] -> current
        | head :: tail when current mod head = 0 ->
            let next = current / head in
            if next mod a = 0 && next mod b = 0
            then recurse next remaining_factors
            else recurse current tail
        | _ :: tail -> recurse current tail
    in
    common_factors
    |> IntSet.to_list
    |> List.sort compare
    |> recurse (a * b);;

let lcm_of_list (lst: int list) : int =
    let rec recurse (current: int) (remaining: int list) : int =
        match remaining with
        | [] -> current
        | head :: tail -> recurse (lcm current head) tail
    in
    match lst with
    | [] -> 0
    | head :: tail -> recurse head tail;;
