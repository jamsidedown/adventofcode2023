let (>>) (left: 'a -> 'b) (right: 'b -> 'c) (value: 'a) : 'c =
    left value |> right;;
