open OUnit2
open Go
open Board
open Players
open Game_controller
open Game_ai
open Rules

let test_init_board _ =
  let bd = Board.return_list (Board.init_board 3) in
  let expected_board =
    [
      [ Go_players.empty; Go_players.empty; Go_players.empty ];
      [ Go_players.empty; Go_players.empty; Go_players.empty ];
      [ Go_players.empty; Go_players.empty; Go_players.empty ];
    ]
  in
  assert_equal expected_board bd;

  let bd2 = Board.return_list (Board.init_board 5) in 
  let expected_board2 = 
    [
      [Go_players.empty; Go_players.empty; Go_players.empty; Go_players.empty; Go_players.empty];
      [Go_players.empty; Go_players.empty; Go_players.empty; Go_players.empty; Go_players.empty];
      [Go_players.empty; Go_players.empty; Go_players.empty; Go_players.empty; Go_players.empty];
      [Go_players.empty; Go_players.empty; Go_players.empty; Go_players.empty; Go_players.empty];
      [Go_players.empty; Go_players.empty; Go_players.empty; Go_players.empty; Go_players.empty];
    ]
  in 
  assert_equal expected_board2 bd2

let test_get_neighbours _ =
  let board_size = 3 in
  let test_board = Board.init_board board_size in
  let center_coord = (1, 1) in
  let neighbors = Board.get_neighbours test_board center_coord in
  let expected_neighbors = [(0, 1); (2, 1); (1, 0); (1, 2)] in
  assert_equal expected_neighbors neighbors

let test_update_board _ =
  let initial_size = 3 in
  let initial_board = Board.init_board initial_size in
  let updated_board = Board.update_board initial_board (1, 1) Go_players.white in
  let updated_board2 = Board.update_board initial_board (1, 1) Go_players.black in

  (* Check that the player at (1, 1) is now player1 *)
  let result_player = Board.get_player updated_board (1, 1) in
  assert_equal Go_players.white result_player;

  let result_player2 = Board.get_player updated_board2 (1, 1)in
  assert_equal Go_players.black result_player2;

  (* Check that other positions remain unchanged *)
  for i = 0 to initial_size - 1 do
    for j = 0 to initial_size - 1 do
      if (i, j) <> (1, 1) then
        let original_player = Board.get_player initial_board (i, j) in
        let updated_player = Board.get_player updated_board (i, j) in
        assert_equal original_player updated_player;
    done;
  done

let test_valid_coordinate _ =
  let board_size = 5 in
  let test_board = Board.init_board board_size in

  let valid = Board.valid_coordinate test_board (2, 3) in
  assert_equal ~msg:"Valid coordinate (2, 3)" true valid;

  let invalid_row = Board.valid_coordinate test_board (-1, 3) in
  assert_equal ~msg:"Invalid coordinate (-1, 3)" false invalid_row;

  let invalid_col = Board.valid_coordinate test_board (2, 6) in
  assert_equal ~msg:"Invalid coordinate (2, 6)" false invalid_col;

  let invalid_both = Board.valid_coordinate test_board (-1, 6) in
  assert_equal ~msg:"Invalid coordinate (-1, 6)" false invalid_both

let test_get_board _ =
  let board_size = 3 in
  let test_board = Board.init_board board_size in
  let expected_board =
    [(2, 2); (2, 1); (2, 0); (1, 2); (1, 1); (1, 0); (0, 2); (0, 1); (0, 0)]
  in
  let result_board = Board.get_board test_board in
  assert_equal ~msg:"Incorrect board" ~printer:(fun lst ->
      Printf.sprintf "[%s]" (String.concat "; " (List.map (fun (x, y) -> Printf.sprintf "(%d, %d)" x y) lst)))
    expected_board
    result_board

let test_print_board _ =
  let board_size = 3 in
  let test_board = Board.init_board board_size in
  let () = Board.print_board test_board in
  assert_bool "Manually verify board is correct" true


let test_is_empty _ =
  assert_equal true (Go_players.is_empty Go_players.empty);
  assert_equal false (Go_players.is_empty Go_players.white);
  assert_equal false (Go_players.is_empty Go_players.black);
  assert_equal false (Go_players.is_empty Go_players.blackhold);
  assert_equal false (Go_players.is_empty Go_players.whitehold)

let test_is_blank _ =
  assert_equal true (Go_players.is_blank Go_players.empty);
  assert_equal true (Go_players.is_blank Go_players.whitehold);
  assert_equal true (Go_players.is_blank Go_players.blackhold);
  assert_equal false (Go_players.is_blank Go_players.white);
  assert_equal false (Go_players.is_blank Go_players.black)

let test_is_white _ =
  assert_equal true (Go_players.is_white Go_players.white);
  assert_equal false (Go_players.is_white Go_players.black);
  assert_equal false (Go_players.is_white Go_players.empty);
  assert_equal false (Go_players.is_white Go_players.whitehold);
  assert_equal false (Go_players.is_white Go_players.blackhold)

let test_is_same _ =
  assert_equal true (Go_players.is_same Go_players.white Go_players.white);
  assert_equal true (Go_players.is_same Go_players.black Go_players.black);
  assert_equal true (Go_players.is_same Go_players.empty Go_players.empty);
  assert_equal true (Go_players.is_same Go_players.whitehold Go_players.whitehold);
  assert_equal true (Go_players.is_same Go_players.blackhold Go_players.blackhold);
  assert_equal false (Go_players.is_same Go_players.black Go_players.blackhold);
  assert_equal false (Go_players.is_same Go_players.blackhold Go_players.black);
  assert_equal false (Go_players.is_same Go_players.white Go_players.black);
  assert_equal false (Go_players.is_same Go_players.empty Go_players.blackhold)

let test_is_consistent _ =
  assert_equal true (Go_players.is_consistent Go_players.white Go_players.white);
  assert_equal true (Go_players.is_consistent Go_players.black Go_players.black);
  assert_equal true (Go_players.is_consistent Go_players.empty Go_players.empty);
  assert_equal true (Go_players.is_consistent Go_players.whitehold Go_players.whitehold);
  assert_equal true (Go_players.is_consistent Go_players.blackhold Go_players.blackhold);
  assert_equal true (Go_players.is_consistent Go_players.black Go_players.blackhold);
  assert_equal true (Go_players.is_consistent Go_players.blackhold Go_players.black);
  assert_equal false (Go_players.is_consistent Go_players.white Go_players.black);
  assert_equal false (Go_players.is_consistent Go_players.empty Go_players.blackhold)

let test_to_char _ =
  assert_equal 'W' (Go_players.to_char Go_players.white);
  assert_equal 'B' (Go_players.to_char Go_players.black);
  assert_equal ' ' (Go_players.to_char Go_players.empty);
  assert_equal ' ' (Go_players.to_char Go_players.whitehold);
  assert_equal ' ' (Go_players.to_char Go_players.blackhold)

let test_opposite _ =
  assert_equal Go_players.black (Go_players.opposite Go_players.white);
  assert_equal Go_players.white (Go_players.opposite Go_players.black)

let test_hold _ =
  assert_equal Go_players.whitehold (Go_players.hold Go_players.white);
  assert_equal Go_players.blackhold (Go_players.hold Go_players.black)

let test_to_string _ =
  assert_equal "Black" (Go_players.to_string Go_players.black);
  assert_equal "White" (Go_players.to_string Go_players.white);
  assert_equal "" (Go_players.to_string Go_players.empty);
  assert_equal "Black" (Go_players.to_string Go_players.blackhold);
  assert_equal "White" (Go_players.to_string Go_players.whitehold)

let test_init _ = 
  let game = Game_controller.init_game 3 Go_players.black 
  in assert_equal (Board.get_board(Game_controller.return_board game)) [(2, 2); (2, 1); (2, 0); (1, 2); (1, 1); (1, 0); (0, 2); (0, 1); (0, 0)];
  assert_equal (Game_controller.return_player_name game) "Black";
  assert_equal (Game_controller.return_black_slots game) 9;
  assert_equal (Game_controller.return_white_slots game) 9

let test_check_board _ = 
  let test_board = Board.init_board 3 in 
  assert_equal (Rules.check_coords test_board (19,19)) @@ false

let init_board = Board.init_board 5 
let updated_board = Board.update_board init_board (1, 1) Go_players.white 
let updated_board = Board.update_board updated_board (0, 1) Go_players.black 
let updated_board = Board.update_board updated_board (1, 0) Go_players.black 
let updated_board2 = Board.update_board updated_board (1, 2) Go_players.black 
let updated_board = Board.update_board updated_board2 (2, 1) Go_players.black 

let picked_board = Board.update_board init_board (0, 1) Go_players.black 
let picked_board = Board.update_board picked_board (1, 0) Go_players.black 
let picked_board = Board.update_board picked_board (1, 2) Go_players.black 
let picked_board = Board.update_board picked_board (2, 1) Go_players.black 
let picked_board = Board.update_board picked_board (1, 1) Go_players.blackhold

let test_is_alive _ = 
  let dead_pieces =  (Game_controller.return_dead Go_players.black updated_board) in 
  assert_equal dead_pieces [(1,1)]

let test_take_pieces _ = 
  let (new_board, dead_len) = (Rules.take_pieces Go_players.black updated_board) in 
  assert_equal dead_len 1;
  assert_equal new_board picked_board

let test_scoring _ = 
  let white_score = Rules.game_done_white_score picked_board in 
  let black_score = Rules.game_done_black_score picked_board in 
  assert_equal white_score 6;
  assert_equal black_score 5

let test_passturn _ = 
  let new_state = Game_controller.pass_turn (Game_controller.init_game 3 Go_players.black) in 
  let new_player = Game_controller.return_player_name new_state in 
  let same_board = Game_controller.return_board new_state in
  assert_equal new_player "White";
  assert_equal same_board (Board.init_board 3)

let test_conv_string_to_pair _ = 
  let new_coord = Game_controller.conv_string_to_pair ("12 12") in 
  assert_equal new_coord (11,11);

  assert_raises (Failure "Incorrect input") (fun () ->
      let _ = Game_controller.conv_string_to_pair "invalid" in
      ()
    )
let test_check_done _ = 
  assert_equal (Rules.check_done Go_players.white 10 10) false;
  assert_equal (Rules.check_done Go_players.white 0 10) false;
  assert_equal (Rules.check_done Go_players.black 10 10) false;
  assert_equal (Rules.check_done Go_players.black 10 0) false;
  assert_equal (Rules.check_done Go_players.black 0 0) true;
  assert_equal (Rules.check_done Go_players.white 0 0) true

let test_update_game _ = 
  let new_game = Game_controller.update_game updated_board Go_players.white 5 5 2 in 
  let new_game2 = Game_controller.update_game updated_board Go_players.black 5 5 0 in 
  let new_game3 = Game_controller.update_game updated_board Go_players.black 5 5 2 in 
  assert_equal (Game_controller.return_white_slots new_game) 6;
  assert_equal (Game_controller.return_black_slots new_game) 5;
  assert_equal (Game_controller.return_white_slots new_game2) 4;
  assert_equal (Game_controller.return_black_slots new_game2) 4;
  assert_equal (Game_controller.return_black_slots new_game3) 6;
  assert_equal (Game_controller.return_white_slots new_game3) 5

let test_get_dead_pieces _ = 
  let mock_state = Game_controller.init_game 19 Go_players.black in
  let removed_state = (Game_controller.run mock_state "2 2") in
  let removed_state = (Game_controller.run removed_state "1 2") in
  let removed_state = (Game_controller.run removed_state "10 10") in
  let removed_state = (Game_controller.run removed_state "2 1") in
  let removed_state = (Game_controller.run removed_state "10 11") in
  let removed_state = (Game_controller.run removed_state "2 3") in
  let removed_state = (Game_controller.run removed_state "10 12") in
  let res_list = Game_controller.get_dead_pieces removed_state "3 2" in 
  assert_equal res_list [(1,1)];

  assert_raises (Failure "Incorrect input") (fun () ->
    let _ = Game_controller.get_dead_pieces removed_state "invalid" in
    ()
  )

let test_run _ = 
  let mock_state = Game_controller.init_game 5 Go_players.black in
  let removed_state = (Game_controller.run mock_state "1 1") in
  let removed_state = (Game_controller.run removed_state "2 1") in
  let removed_state = (Game_controller.run removed_state "4 4") in 
  let removed_state = (Game_controller.run removed_state "1 2") in
  let init_board = Board.init_board 5 in
  let updated_board = Board.update_board init_board (0, 0) Go_players.whitehold in
  let updated_board = Board.update_board updated_board (0, 1) Go_players.white in
  let updated_board = Board.update_board updated_board (1, 0) Go_players.white in
  let updated_board = Board.update_board updated_board (3, 3) Go_players.black in
  let failed_state = (Game_controller.run mock_state "Failed") in
  let failed_state2 = (Game_controller.run mock_state "t s") in
  let failed_state3 = (Game_controller.run mock_state "999 999") in 
  let failed_state4 = (Game_controller.run removed_state "1 2") in
  assert_equal (Game_controller.return_player_name removed_state) "Black";
  assert_equal (Game_controller.return_white_slots removed_state) 22;
  assert_equal (Game_controller.return_white_slots removed_state) 22;
  assert_equal (Game_controller.return_board removed_state) updated_board;
  assert_equal (Game_controller.return_white_slots failed_state) 1;
  assert_equal (Game_controller.return_white_slots failed_state2) 1;
  assert_equal (Game_controller.return_white_slots failed_state3) 1;
  assert_equal (Game_controller.return_white_slots failed_state4) 1


let test_open_center_positions _ = 
  let board_size = 4 in
  let test_board = Board.init_board board_size in
  let updated_board1 = Board.update_board test_board (1, 1) Go_players.white in
  let updated_board2 = Board.update_board updated_board1 (1, 2) Go_players.black in
  let updated_board3 = Board.update_board updated_board2 (2, 1) Go_players.white in
  let updated_board4 = Board.update_board updated_board3 (2, 2) Go_players.black in
  assert_equal (open_center_positions test_board) @@ [(1, 1); (1, 2); (2, 1); (2, 2)];
  assert_equal (open_center_positions updated_board1) @@ [(1, 2); (2,1); (2,2)];
  assert_equal (open_center_positions updated_board2) @@ [(2,1); (2,2)];
  assert_equal (open_center_positions updated_board3) @@ [(2,2)];
  assert_equal (open_center_positions updated_board4) @@ []

let test_random_player _ = 
  let board_size = 2 in
  let test_board = Board.init_board board_size in
  let updated_board1 = Board.update_board test_board (0, 0) Go_players.white in
  let updated_board2 = Board.update_board updated_board1 (0, 1) Go_players.black in
  let updated_board3 = Board.update_board updated_board2 (1, 0) Go_players.white in
  let random_move = random_player updated_board3 Go_players.black in 
  let updated_board4 = Board.update_board updated_board3 (1, 1) Go_players.black in
  let updated_board5, _ = Rules.take_pieces Go_players.black updated_board4 in
  let updated_board6 = Board.update_board updated_board5 (1, 0) Go_players.white in
  let random_move2 = random_player updated_board6 Go_players.white in
  assert_equal random_move @@ "2 2"; (* String coordinates *)
  assert_equal random_move2 @@ "1 1"

let test_return_black_slots _ = 
  let game = Game_controller.init_game 19 Go_players.black in 
  assert_equal (Game_controller.return_black_slots game) 361

let test_return_white_slots _ =
  let game = Game_controller.init_game 19 Go_players.black in 
  assert_equal (Game_controller.return_white_slots game) 361

let suite =
  "Go Test Suite" >:::
  [
    "Board Test Suite" >:::
    [
      "test_get_neighbours" >:: test_get_neighbours;
      "test_valid_coordinate" >:: test_valid_coordinate;
      "test_get_board" >:: test_get_board;
      "test_print_board" >:: test_print_board;
      "test_init_board" >:: test_init_board;
      "test_update_board" >:: test_update_board;
    ];
    "Go_players Test Suite" >:::
    [
      "test_empty" >:: test_is_empty;
      "test_is_blank" >:: test_is_blank;
      "test_is_white" >:: test_is_white;
      "test_is_same" >:: test_is_same;
      "test_is_consistent" >:: test_is_consistent;
      "test_to_char" >:: test_to_char;
      "test_opposite" >:: test_opposite;
      "test_hold" >:: test_hold;
      "test_to_string" >:: test_to_string;
    ];
    "Game Controller Test Suite" >:::
    [
      "init game" >:: test_init;
      "check coords" >:: test_check_board;
      "check alive" >:: test_is_alive;
      "check picked pieces" >:: test_take_pieces;
      "check scoring" >:: test_scoring;
      "check pass turn" >:: test_passturn;
      "check done" >:: test_check_done;
      "test_conv_string_to_pair" >:: test_conv_string_to_pair;
      "test_update_game" >:: test_update_game;
      "test_run" >:: test_run;
      "test_return_black" >:: test_return_black_slots;
      "test_return_white" >:: test_return_white_slots;
      "test_dead_pieces" >:: test_get_dead_pieces;
    ];
    "Game AI Test Suite" >::: [
      "open_center_positions" >:: test_open_center_positions;
      "random_player" >:: test_random_player;
    ];
  ]

let () =
  run_test_tt_main suite