let rec pow (x: int) (exponent: int) : int =
    match exponent with
    | 0 -> 1
    | n when n > 0 -> x * (pow x (n - 1))
    | _ -> 0;;
