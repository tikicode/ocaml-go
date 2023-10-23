open Board

let board_size = 16

let initial_board = Board.create board_size

let rec game_loop board current_player =
  Board.print board;
  Printf.printf "Player %c, enter the row and column (e.g., '2 2') to place your piece: " current_player;
  match read_line () with
  | exception End_of_file -> ()
  | input ->
    match String.split_on_char ' ' input with
    | [row_str; col_str] ->
      begin
        try
          let row = int_of_string row_str in
          let col = int_of_string col_str in
          let updated_board = Board.update board (row - 1) (col - 1) current_player in
          let next_player = if current_player = 'B' then 'W' else 'B' in
          game_loop updated_board next_player
        with
        | Failure _ ->
          print_string "Invalid input. Please enter valid coordinates.\n";
          game_loop board current_player
      end
    | _ ->
      print_string "Invalid input format. Please enter coordinates as 'row col'.\n";
      game_loop board current_player

let () = game_loop initial_board 'B'