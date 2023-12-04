open OUnit2
open Go
open Board
open Players

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
  ]

let () =
  run_test_tt_main suite