open Core
open Players
open Board

module Rules = struct
  let game_done_white_score (bd : Board.t) : int =
    Board.count bd ~f:(Go_players.is_consistent Go_players.white) + 6

  let game_done_black_score (bd : Board.t) : int =
    Board.count bd ~f:(Go_players.is_consistent Go_players.black)

  let check_done (player : Go_players.t) (black_slots : int) (white_slots : int)
      : bool =
    if player |> Go_players.is_white then white_slots <= 0 else black_slots <= 0

  let rec dfs (board : Board.t) (player : Go_players.t)
      (visited : (int * int, 'a) Set.t) (stack : (int * int) list) : bool =
    match stack with
    | [] -> false
    | coord :: st when Set.mem visited coord -> dfs board player visited st
    | coord :: st ->
        let p = Board.get_player board coord in
        if p |> Go_players.is_blank then true
        else if not (Go_players.is_same player p) then
          dfs board player visited st
        else
          let neighbours = Board.get_neighbours board coord in
          let unvisited =
            List.filter neighbours ~f:(fun c -> not (Set.mem visited c))
          in
          dfs board player (Set.add visited coord) (unvisited @ st)

  let compare_tuples ((x1, y1) : int * int) ((x2, y2) : int * int) : bool =
    if x1 = x2 && y1 = y2 then true else false
  
  let string_to_move (move : string) : int * int =
    match String.split move ~on:' ' with
    | [ row; col ] -> (int_of_string row - 1, int_of_string col - 1)
    | _ -> (-1, -1)

  let move_to_string ((x, y) : int * int) : string =
    string_of_int (x + 1) ^ " " ^ string_of_int (y + 1)

  let is_alive (bd : Board.t) (player : Go_players.t) (coord : int * int) : bool
      =
    let p = Board.get_player bd coord in
    if p |> Go_players.is_blank then true
    else if Go_players.is_same player p then true
    else
      let set = Set.empty (module Tuple.Comparator (Int) (Int)) in
      dfs bd p set [ coord ]

  let check_move (bd : Board.t) (player : Go_players.t) (coord : int * int) :
      bool =
    is_alive bd (Go_players.opposite player) coord

  let check_coords (board : Board.t) (coord : int * int) : bool =
    if not (Board.valid_coordinate board coord) then (
      print_string "Invalid coordinitate.\n";
      false)
    else
      let p = Board.get_player board coord in
      if Go_players.is_blank p then true
      else (
        print_string "The position has already been occupied.\n";
        false)

  let take_pieces (player : Go_players.t) (bd : Board.t) : Board.t * int =
    let coords = Board.get_board bd in
    let holden = Go_players.hold player in
    let deads =
      List.filter coords ~f:(fun coord -> not (is_alive bd player coord))
    in
    let new_board =
      List.fold deads ~init:bd ~f:(fun bd coord ->
          Board.update_board bd coord holden)
    in
    (new_board, List.length deads)
end
