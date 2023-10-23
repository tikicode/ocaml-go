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
      Printf.printf "%2d " row_number; 
      print_row row;
      print_rows rest (row_number + 1) 
  in

  print_string "    "; (* Add extra space for alignment *)
  for i = 0 to board_size - 1 do
    Printf.printf "%2d " (i + 1) (* Print the column number starting from 1 *)
  done;
  print_newline ();
  print_rows board 0


(* Main game loop *)
let game_loop board =
  print_board board

let () = game_loop initial_board
  
