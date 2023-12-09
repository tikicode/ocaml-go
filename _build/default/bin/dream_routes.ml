(* let () =
  Dream.run ~interface:"0.0.0.0" ~port:3000 (fun _ ->
    Dream.html "Hello world!") *)
open Go
open Players
open Game_controller

type data = (int*int)
[@@deriving yojson]

type data_list = data list [@@deriving yojson]

(* let test_data_list : data_list = [(1, 2); (2,3); (1,5)] *)

type game_state = {
  mutable game: Game_controller.t; 
}
  
let game_state : game_state = {
  game = Game_controller.init_game 20 Go_players.black;
}

let move_handler req =
  let%lwt move = Dream.body req in
  let headers = [
    "Access-Control-Allow-Origin", "*";
    "Access-Control-Allow-Methods", "GET, POST, OPTIONS";
    "Access-Control-Allow-Headers", "Content-Type";
  ] in
  let removed_pieces = Game_controller.get_dead_pieces (game_state.game) move in 
  let old_state = game_state.game in
  game_state.game <- (Game_controller.run_two_player game_state.game move);
  let remove = 
    (match Game_controller.get_white_slots game_state.game = 1 with 
      | true -> game_state.game <- old_state; (Game_controller.conv_string_to_pair_list move)
      | false -> removed_pieces) in 
     
  Dream.json ~headers (Yojson.Safe.to_string (data_list_to_yojson remove))

  (* (Printf.sprintf "Received POST request with body: %s" body) *)


let () =
  Dream.run 
  @@ Dream.logger
  @@ Dream.router [
    
    Dream.post "/move" move_handler;

  ]
