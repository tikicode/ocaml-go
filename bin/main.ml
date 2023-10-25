open Go

let () =
  (* create a game and run *)
  let game = Game_controller.init 16 Players.black in
  Game_controller.run game
