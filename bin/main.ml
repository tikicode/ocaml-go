open Go
open Players
open Game_controller
open Game_ai

let () =
  let game = Game_controller.init_game 2 Go_players.black in
  Game_controller.run_player_vs_ai_console game ~ai:random_player
