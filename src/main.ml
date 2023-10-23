let board_size = 16

let initial_board =
  let row = List.init board_size (fun _ -> ' ') in
  List.init board_size (fun _ -> row)

let print_board board =
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
  for i = 0 to board_size - 1 do
    Printf.printf "%2d " (i + 1)
  done;
  print_newline ();
  print_rows board 0

let update_board board x y value =
  if x < 0 || x >= board_size || y < 0 || y >= board_size then
    board  (* Coordinates out of bounds, return the original board *)
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

let rec game_loop board =
  print_board board;
  print_string "Enter the row and column (e.g., '2 2') to place 'B': ";
  match read_line () with
  | exception End_of_file -> ()
  | input ->
    match String.split_on_char ' ' input with
    | [row_str; col_str] ->
      begin
        try
          let row = int_of_string row_str in
          let col = int_of_string col_str in
          let updated_board = update_board board (row - 1) (col - 1) 'B' in
          game_loop updated_board
        with
        | Failure _ ->
          print_string "Invalid input. Please enter valid coordinates.\n";
          game_loop board
      end
    | _ ->
      print_string "Invalid input format. Please enter coordinates as 'row col'.\n";
      game_loop board

let () = game_loop initial_board