module Board = struct
  type t = char list list

  let create board_size =
    let row = List.init board_size (fun _ -> ' ') in
    List.init board_size (fun _ -> row)

  let print board =
    let rec print_row row =
      match row with
      | [] -> print_newline ()
      | cell :: rest ->
        Printf.printf "[%c]" cell;
        print_row rest
    in

    let rec print_rows board row_number =
      match board with
      | [] -> ()
      | row :: rest ->
        Printf.printf "%2d " (row_number + 1);
        print_row row;
        print_rows rest (row_number + 1)
    in

    print_string "    ";
    for i = 0 to List.length board - 1 do
      Printf.printf "%2d " (i + 1)
    done;
    print_newline ();
    print_rows board 0

  let update board x y value =
    if x < 0 || x >= List.length board || y < 0 || y >= List.length board then
      board
    else
      let rec update_row row col value =
        match row with
        | [] -> []
        | cell :: rest ->
          if col = 0 then
            value :: rest
          else
            cell :: update_row rest (col - 1) value
      in
      let rec update_rows rows row value =
        match rows with
        | [] -> []
        | r :: rest ->
          if row = 0 then
            (update_row r y value) :: rest
          else
            r :: update_rows rest (row - 1) value
      in
      update_rows board x value
end