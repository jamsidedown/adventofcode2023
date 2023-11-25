let rec last (lst: 'a list) : 'a option =
  match lst with
  | [] -> None
  | [ last' ] -> Some last'
  | _ :: t -> last t;;
