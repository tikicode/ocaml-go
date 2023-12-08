open Core
open Game_controller
open Board
open Players

let rec random_player (bd : Board.t) (player : Go_players.t) (black_slots : int) (white_slots : int) : Game_controller.t = 
  let next_move = Random.int (Board.get_size bd), Random.int (Board.get_size bd)
  in
  if not (Board.valid_coordinate bd next_move) then 
    random_player bd player black_slots white_slots
  else
    let p = Board.get_player bd next_move in
      if Go_players.is_blank p then
        let new_board = Board.update_board bd next_move player in
        let occupied_board, pieces = Game_controller.take_pieces player new_board in
        if Game_controller.check_move occupied_board player next_move then
          Game_controller.update_game occupied_board player black_slots white_slots pieces
        else 
          random_player bd player black_slots white_slots 
      else random_player bd player black_slots white_slots