(* let () =
   Dream.run ~interface:"0.0.0.0" ~port:3000 (fun _ ->
     Dream.html "Hello world!") *)

open Go
open Players
open Rules
open Game_controller

type data = int * int [@@deriving yojson]
type data_list = data list [@@deriving yojson]
type turn = string [@@deriving yojson]
type game_state = { mutable game : Game_controller.t }

let game_state : game_state =
  { game = Game_controller.init_game 20 Go_players.black }

let two_player_move_handler req =
  let%lwt move = Dream.body req in
  let headers =
    [
      ("Access-Control-Allow-Origin", "*");
      ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
      ("Access-Control-Allow-Headers", "Content-Type");
    ]
  in
  let removed_pieces = Game_controller.get_dead_pieces game_state.game move in
  let old_state = game_state.game in
  game_state.game <- Game_controller.run_two_player game_state.game move;
  let remove =
    match Game_controller.get_white_slots game_state.game = 1 with
    | true ->
        game_state.game <- old_state;
        Game_controller.conv_string_to_pair_list move
    | false -> removed_pieces
  in
  Dream.json ~headers (Yojson.Safe.to_string (data_list_to_yojson remove))

let ai_move_handler _ =
  let headers =
    [
      ("Access-Control-Allow-Origin", "*");
      ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
      ("Access-Control-Allow-Headers", "Content-Type");
    ]
  in
  let move = Game_controller.next_move_ai game_state.game in
  Dream.json ~headers (Yojson.Safe.to_string (turn_to_yojson move))

let turn_handler _ =
  let headers =
    [
      ("Access-Control-Allow-Origin", "*");
      ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
      ("Access-Control-Allow-Headers", "Content-Type");
    ]
  in
  Dream.json ~headers
    (Yojson.Safe.to_string
       (turn_to_yojson (Game_controller.return_player game_state.game)))

let score_handler _ =
  let headers =
    [
      ("Access-Control-Allow-Origin", "*");
      ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
      ("Access-Control-Allow-Headers", "Content-Type");
    ]
  in
  let final_board = Game_controller.return_board game_state.game in
  let white_score = Rules.game_done_white_score final_board in
  let black_score = Rules.game_done_black_score final_board in
  Dream.json ~headers
    (Yojson.Safe.to_string (data_to_yojson (white_score, black_score)))

let reset_game_handler _ =
  let headers =
    [
      ("Access-Control-Allow-Origin", "*");
      ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
      ("Access-Control-Allow-Headers", "Content-Type");
    ]
  in
  game_state.game <- Game_controller.init_game 20 Go_players.black;
  Dream.json ~headers (Yojson.Safe.to_string (turn_to_yojson "Begin New Game"))

let pass_turn_handler _ =
  let headers =
    [
      ("Access-Control-Allow-Origin", "*");
      ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
      ("Access-Control-Allow-Headers", "Content-Type");
    ]
  in
  game_state.game <- Game_controller.pass_turn game_state.game;
  Dream.json ~headers (Yojson.Safe.to_string (turn_to_yojson "Changed"))

let () =
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.post "/move" two_player_move_handler;
         Dream.get "/player_turn" turn_handler;
         Dream.get "/reset_game" reset_game_handler;
         Dream.get "/get_score" score_handler;
         Dream.get "/move_ai" ai_move_handler;
         Dream.get "/pass_turn" pass_turn_handler;
       ]
