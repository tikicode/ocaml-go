open Players
open Board
open Rules
open Core

module Game_controller = struct
  type t = {
    bd : Board.t;
    player : Go_players.t;
    black_slots : int;
    white_slots : int;
  }

  let init_game (size : int) (player : Go_players.t) : t =
    {
      bd = Board.init_board size;
      player;
      black_slots = size * size;
      white_slots = size * size;
    }

  let return_black_slots { black_slots; _ } : int = black_slots
  let return_white_slots { white_slots; _ } : int = white_slots

  let game_done (bd : Board.t) (white_handi : int) (black_handi : int) : unit =
    let black_score = Rules.game_done_black_score bd + black_handi in
    let white_score = Rules.game_done_white_score bd + white_handi in
    Printf.printf "\nThe score of player Black is: %d\n" black_score;
    Printf.printf "The score of player White is: %d\n" white_score;

    if black_score > white_score then Printf.printf "Player Black wins!\n"
    else if black_score < white_score then Printf.printf "Player White wins!\n"
    else Printf.printf "It's a tie!\n"

  let return_board { bd; _ } : Board.t = bd
  let return_player { player; _ } : string = Go_players.to_string player

  let pass_turn { bd; player; black_slots; white_slots } : t =
    let new_player = Go_players.opposite player in
    { bd; player = new_player; black_slots; white_slots }

  let rec next_move_ai { bd; player; black_slots; white_slots } : string =
    let random_coordinate =
      (Random.int (Board.get_size bd), Random.int (Board.get_size bd))
    in
    if not (Rules.check_coords bd random_coordinate) then
      next_move_ai { bd; player; black_slots; white_slots }
    else
      match random_coordinate with
      | x, y -> string_of_int x ^ " " ^ string_of_int y
  (* modify this to use the Game_ai funciton *)
  (* or move the random into this? need to figure out a better way to
     do this for the front end bc we need to pass the current game state back
     to do predictions later.
     May also need to make mutable tree on front end, fk i should've done
     this earlier lmao
  *)

  let conv_string_to_pair move =
    match String.split move ~on:' ' with
    | [ row; col ] -> (int_of_string row - 1, int_of_string col - 1)
    | _ -> failwith "Incorrect input"

  let update_game (bd : Board.t) (player : Go_players.t) (black_slots : int)
      (white_slots : int) (pieces : int) : t =
    let new_player = Go_players.opposite player in
    if pieces = 0 then
      {
        bd;
        player = new_player;
        black_slots = black_slots - 1;
        white_slots = white_slots - 1;
      }
    else if Go_players.is_white player then
      {
        bd;
        player = new_player;
        black_slots = black_slots + pieces - 2;
        white_slots = white_slots + pieces - 1;
      }
    else
      {
        bd;
        player = new_player;
        black_slots = black_slots + pieces - 1;
        white_slots = white_slots + pieces - 2;
      }

  let take_pieces (player : Go_players.t) (bd : Board.t) : Board.t * int =
    let coords = Board.get_board bd in
    let holden = Go_players.hold player in
    let deads =
      List.filter coords ~f:(fun coord -> not (Rules.is_alive bd player coord))
    in
    let new_board =
      List.fold deads ~init:bd ~f:(fun bd coord ->
          Board.update_board bd coord holden)
    in
    (new_board, List.length deads)

  let return_dead (player : Go_players.t) (bd : Board.t) : (int * int) list =
    let coords = Board.get_board bd in
    List.filter coords ~f:(fun coord -> not (Rules.is_alive bd player coord))

  let play_ai ({ bd; player; black_slots; white_slots } : t) ~ai : t =
    ai bd player black_slots white_slots

  let get_dead_pieces { bd; player; _ } (inputs : string) : (int * int) list =
    match String.split_on_chars inputs ~on:[ ' ' ] with
    | [ s1; s2 ] -> (
        match (int_of_string_opt s1, int_of_string_opt s2) with
        | Some row, Some col ->
            let coord = (row - 1, col - 1) in
            if Rules.check_coords bd coord then
              let new_board = Board.update_board bd coord player in
              return_dead player new_board
            else [ (21, 21) ]
        | _ -> [ (22, 22) ])
    | _ -> [ (23, 23) ]

  let get_white_slots { white_slots; _ } = white_slots

  let run ({ bd; player; black_slots; white_slots } : t) ~ai (uses_ai : bool)
      (input : string) : t =
    match String.split_on_chars input ~on:[ ' ' ] with
    | [ s1; s2 ] -> (
        match (int_of_string_opt s1, int_of_string_opt s2) with
        | Some row, Some col ->
            let coord = (row - 1, col - 1) in
            if Rules.check_coords bd coord then
              let new_board = Board.update_board bd coord player in
              let occupied_board, pieces = take_pieces player new_board in
              if Rules.check_move occupied_board player coord then
                let updated =
                  update_game occupied_board player black_slots white_slots
                    pieces
                in
                if uses_ai then
                  let ai_play = play_ai updated ~ai in
                  ai_play
                else updated
              else init_game 1 Go_players.black
            else init_game 1 Go_players.black
        | _ -> init_game 1 Go_players.black)
    | _ -> init_game 1 Go_players.black

  let run_two_player ({ bd; player; black_slots; white_slots } : t)
      (inputs : string) : t =
    run
      { bd; player; black_slots; white_slots }
      ~ai:(fun _ _ _ _ -> { bd; player; black_slots; white_slots })
      false inputs

  let run_player_vs_ai ({ bd; player; black_slots; white_slots } : t) ~ai
      (inputs : string) : t =
    run (play_ai { bd; player; black_slots; white_slots } ~ai) ~ai true inputs

  let rec run_console ({ bd; player; black_slots; white_slots } : t) ~ai
      (uses_ai : bool) : unit =
    if Rules.check_done player black_slots white_slots then game_done bd 0 0
    else (
      Board.print_board bd;
      Printf.printf
        "Player %s, enter the row and column (e.g. '2 2')\n\
         to place your piece (ctrl-d to quit): "
        (Go_players.to_string player);
      Out_channel.(flush stdout);
      match In_channel.(input_line stdin) with
      | None -> game_done bd 0 0
      | Some input -> (
          match String.split_on_chars input ~on:[ ' ' ] with
          | [ s1; s2 ] -> (
              match (int_of_string_opt s1, int_of_string_opt s2) with
              | Some row, Some col ->
                  let coord = (row - 1, col - 1) in
                  if Rules.check_coords bd coord then
                    let new_board = Board.update_board bd coord player in
                    let occupied_board, pieces = take_pieces player new_board in
                    if Rules.check_move occupied_board player coord then
                      let new_board =
                        update_game occupied_board player black_slots
                          white_slots pieces
                      in
                      if uses_ai then
                        run_console (play_ai new_board ~ai) ~ai uses_ai
                      else run_console new_board ~ai uses_ai
                    else (
                      print_string "The position will make your piece(s) dead\n";
                      run_console
                        { bd; player; black_slots; white_slots }
                        ~ai uses_ai)
                  else
                    run_console
                      { bd; player; black_slots; white_slots }
                      ~ai uses_ai
              | _ ->
                  print_string "Invalid input.\n";
                  run_console
                    { bd; player; black_slots; white_slots }
                    ~ai uses_ai)
          | _ ->
              print_string
                "Invalid input format. Please enter coordinates as 'row col'.\n";
              run_console { bd; player; black_slots; white_slots } ~ai uses_ai))

  let run_two_player_console ({ bd; player; black_slots; white_slots } : t) :
      unit =
    run_console
      { bd; player; black_slots; white_slots }
      ~ai:(fun _ _ _ _ -> { bd; player; black_slots; white_slots })
      false

  let run_player_vs_ai_console ({ bd; player; black_slots; white_slots } : t)
      ~ai : unit =
    run_console (play_ai { bd; player; black_slots; white_slots } ~ai) ~ai true
end
