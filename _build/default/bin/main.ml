open Go
open Players
open Game_controller

let () =
  let game = Game_controller.init_game 19 Go_players.black in
  Game_controller.run game 
