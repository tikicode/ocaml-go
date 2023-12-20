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
  let return_board { bd; _ } : Board.t = bd
  let return_player_name { player; _ } : string = player |> Go_players.to_string
  let return_player { player; _ } : Go_players.t = player

  let pass_turn { bd; player; black_slots; white_slots } : t =
    { bd; player = player |> Go_players.opposite; black_slots; white_slots }

  let update_game (bd : Board.t) (player : Go_players.t) (black_slots : int)
      (white_slots : int) (pieces : int) : t =
    let new_player = player |> Go_players.opposite in
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

  let return_dead (player : Go_players.t) (bd : Board.t) : (int * int) list =
    let coords = bd |> Board.get_board in
    List.filter coords ~f:(fun coord -> not (Rules.is_alive bd player coord))

  let get_dead_pieces { bd; player; _ } (inputs : string) : (int * int) list =
    match String.split_on_chars inputs ~on:[ ' ' ] with
    | [ s1; s2 ] -> (
        match (int_of_string_opt s1, int_of_string_opt s2) with
        | Some row, Some col ->
            let coord = (row - 1, col - 1) in
            if Rules.check_coords bd coord then
              let new_board = Board.update_board bd coord player in
              return_dead player new_board
            else failwith "Incorrect input"
        | _ -> failwith "Incorrect input")
    | _ -> failwith "Incorrect input"

  let run ({ bd; player; black_slots; white_slots } : t) (input : string) : t =
    match String.split_on_chars input ~on:[ ' ' ] with
    | [ s1; s2 ] -> (
        match (int_of_string_opt s1, int_of_string_opt s2) with
        | Some row, Some col ->
            let coord = (row - 1, col - 1) in
            if Rules.check_coords bd coord then
              let new_board = Board.update_board bd coord player in
              let occupied_board, pieces = Rules.take_pieces player new_board in
              if Rules.check_move occupied_board player coord then
                update_game occupied_board player black_slots white_slots pieces
              else init_game 1 Go_players.black
            else init_game 1 Go_players.black
        | _ -> init_game 1 Go_players.black)
    | _ -> init_game 1 Go_players.black

  [@@@coverage off]

  let game_done (bd : Board.t) (white_handi : int) (black_handi : int) : unit =
    let black_score = Rules.game_done_black_score bd + black_handi in
    let white_score = Rules.game_done_white_score bd + white_handi in
    Printf.printf "\nThe score of player Black is: %d\n" black_score;
    Printf.printf "The score of player White is: %d\n" white_score;

    if black_score > white_score then Printf.printf "Player Black wins!\n"
    else if black_score < white_score then Printf.printf "Player White wins!\n"
    else Printf.printf "It's a tie!\n"

  let rec run_console ({ bd; player; black_slots; white_slots } : t) ~ai
      (uses_ai : bool) : unit =
    if Rules.check_done player black_slots white_slots then game_done bd 0 0
    else bd |> Board.print_board;
    Printf.printf
      "Player %s, enter the row and column (e.g. '2 2')\n\
       to place your piece (ctrl-d to quit): "
      (Go_players.to_string player);
    Out_channel.(flush stdout);
    let coord =
      if Go_players.is_black player && uses_ai then
        Rules.string_to_move (ai bd player)
      else
        match In_channel.(input_line stdin) with
        | None -> (-1, -1)
        | Some input -> input |> Rules.string_to_move
    in
    if Rules.compare_tuples coord (-1, -1) then (
      print_string "Invalid input.\n";
      run_console { bd; player; black_slots; white_slots } ~ai uses_ai)
    else if Rules.check_coords bd coord then
      let new_board = Board.update_board bd coord player in
      let occupied_board, pieces = Rules.take_pieces player new_board in
      if Rules.check_move occupied_board player coord then
        let new_board =
          update_game occupied_board player black_slots white_slots pieces
        in
        run_console new_board ~ai uses_ai
      else (
        print_string "The position will make your piece(s) dead\n";
        run_console { bd; player; black_slots; white_slots } ~ai uses_ai)
    else run_console { bd; player; black_slots; white_slots } ~ai uses_ai

  let run_two_player_console ({ bd; player; black_slots; white_slots } : t) :
      unit =
    run_console
      { bd; player; black_slots; white_slots }
      ~ai:(fun _ _ -> "")
      false

  let run_player_vs_ai_console ({ bd; player; black_slots; white_slots } : t)
      ~ai : unit =
    run_console { bd; player; black_slots; white_slots } ~ai true
end
