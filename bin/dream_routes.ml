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
  game_state.game <- (Game_controller.run3 game_state.game move);
  Dream.json ~headers (Yojson.Safe.to_string (data_list_to_yojson removed_pieces))

  (* (Printf.sprintf "Received POST request with body: %s" body) *)

let start_handler _ = 
  let headers = [
    "Access-Control-Allow-Origin", "*";
    "Access-Control-Allow-Methods", "GET, POST, OPTIONS";
    "Access-Control-Allow-Headers", "Content-Type";
  ] in
  game_state.game <- Game_controller.run3 game_state.game "21 21";
  let data = (1,2) in
  Dream.json ~headers (Yojson.Safe.to_string (data_to_yojson data))

let () =
  Dream.run 
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/start" start_handler;
    
    Dream.post "/move" move_handler;

  ]
