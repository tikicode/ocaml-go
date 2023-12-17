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
    if Go_players.is_white player then white_slots <= 0 else black_slots <= 0

  