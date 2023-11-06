open Core

(* board, first player and empty slots for black and white player *)
type t = {
  board : Board.t;
  player : Players.t;
  black_slots : int;
  white_slots : int;
}

(* create a game *)
let init size player =
  {
    board = Board.init size;
    player;
    black_slots = size * size;
    white_slots = size * size;
  }

(* run DFS on the player, to check whether piece is alive *)
let rec dfs board player visited stack =
  match stack with
  (* dfs over, the player is dead *)
  | [] -> false
  (* skip visited coord *)
  | coord :: st when Set.mem visited coord -> dfs board player visited st
  (* process unvisited coord *)
  | coord :: st ->
      (* get the p at coord *)
      let p = Board.get_player board coord in
      (* find a blank pos *)
      if Players.is_blank p then true
        (* p is not the same side with player, skip it *)
      else if not (Players.is_same player p) then dfs board player visited st
        (* p is same side with player, check all p's neighbours *)
      else
        let neighbours = Board.get_neighbours board coord in
        (* only process unvisited *)
        let unvisited =
          List.filter neighbours ~f:(fun c -> not (Set.mem visited c))
        in
        (* add coord to visited, add unvistied to stack *)
        dfs board player (Set.add visited coord) (unvisited @ st)

(* check whether a coord is alive *)
let is_alive board player coord =
  (* get the player at coord *)
  let p = Board.get_player board coord in
  (* blank player, alive *)
  if Players.is_blank p then true (* skip same side *)
  else if Players.is_same player p then true
  else
    (* run dfs from coord *)
    let set = Set.empty (module Tuple.Comparator (Int) (Int)) in
    dfs board p set [ coord ]

(* take all dead pieces *)
let take_pieces player board =
  (* get all coordinates *)
  let coords = Board.all_coordinates board in
  let holden = Players.hold player in
  (* get all dead coordiniates *)
  let deads =
    List.filter coords ~f:(fun coord -> not (is_alive board player coord))
  in
  (* replace dead coordinites with proper pieces *)
  let new_board =
    List.fold deads ~init:board ~f:(fun bd coord ->
        Board.update_board bd coord holden)
  in
  (new_board, List.length deads)

(* game is done, print score *)
let game_done board =
  let black_score =
    Board.count board ~f:(Players.is_consistent Players.black)
  in
  let white_score =
    Board.count board ~f:(Players.is_consistent Players.white)
  in
  Printf.printf "\nThe score of player Black is: %d\n" black_score;
  Printf.printf "The score of player White is: %d\n" white_score

(* check whether the coordniate can be used to place piece *)
let check_coords board coord =
  (* invalid coord *)
  if not (Board.valid_coordinate board coord) then (
    print_string "Invalid coordinitate.\n";
    false)
  else
    let p = Board.get_player board coord in
    if Players.is_blank p then true
    else (
      (* occupied *)
      print_string "The position has already been occupied.\n";
      false)

let check_done player black_slots white_slots =
  if Players.is_white player then white_slots <= 0 else black_slots <= 0

let check_move board player coord =
  is_alive board (Players.opposite player) coord

let update_game board player black_slots white_slots pieces =
  let new_player = Players.opposite player in
  if pieces = 0 then
    {
      board;
      player = new_player;
      black_slots = black_slots - 1;
      white_slots = white_slots - 1;
    }
  else if Players.is_white player then
    {
      board;
      player = new_player;
      black_slots = black_slots + pieces - 2;
      white_slots = white_slots + pieces - 1;
    }
  else
    {
      board;
      player = new_player;
      black_slots = black_slots + pieces - 1;
      white_slots = white_slots + pieces - 2;
    }

(* run the game *)
let rec run { board; player; black_slots; white_slots } =
  (* no free slot, done *)
  if check_done player black_slots white_slots then game_done board
  else (
    (* print board *)
    Board.print board;
    (* get input from player *)
    Printf.printf
      "Player %s, enter the row and column (e.g. '2 2')\n\
       to place your piece (ctrl-d to quit): " (Players.to_string player);
    Out_channel.(flush stdout);
    (* check input *)
    match In_channel.(input_line stdin) with
    (* no input, done *)
    | None -> game_done board
    (* has input *)
    | Some input -> (
        (* parse input *)
        match String.split_on_chars input ~on:[ ' ' ] with
        | [ s1; s2 ] -> (
            (* parse coordinate *)
            match (int_of_string_opt s1, int_of_string_opt s2) with
            | Some row, Some col ->
                (* get coord *)
                let coord = (row - 1, col - 1) in
                (* check coord *)
                if check_coords board coord then
                  (* place piece *)
                  let new_board = Board.update_board board coord player in
                  (* take dead pieces *)
                  let occupied_board, pieces = take_pieces player new_board in
                  (* check whether move is valid *)
                  if check_move occupied_board player coord then
                    (* run new game*)
                    run
                      (update_game occupied_board player black_slots white_slots
                         pieces)
                  else (
                    print_string "The position will make your piece(s) dead\n";
                    run { board; player; black_slots; white_slots })
                  (* invalid integer, try again *)
                else run { board; player; black_slots; white_slots }
            | _ ->
                print_string "Invalid input.\n";
                run { board; player; black_slots; white_slots })
        (* invalid format, try again *)
        | _ ->
            print_string
              "Invalid input format. Please enter coordinates as 'row col'.\n";
            run { board; player; black_slots; white_slots }))
