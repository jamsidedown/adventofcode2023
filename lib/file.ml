let read_lines (filename: string) : string list =
    let file =
        filename
        |> Printf.sprintf "input/%s"
        |> open_in in
    let try_read () : string option =
        try Some (input_line file)
        with End_of_file -> None in
    let[@tail_mod_cons] rec loop () : string list =
        match try_read() with
        | Some s -> s :: loop()
        | None ->
            close_in file;
            [] in
    loop();;
