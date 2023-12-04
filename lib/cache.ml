let memoise f =
    let dict = Hashtbl.create 1024 in
    fun (x: 'a) : 'b ->
        match Hashtbl.find_opt dict x with
        | Some value -> value
        | None ->
            let value = f x in
            Hashtbl.replace dict x value;
            value;;

let memoise_rec f x =
    let fref = ref (fun _ -> assert false) in
    let f' = memoise (fun x -> f !fref x) in
    fref := f';
    f' x;;
