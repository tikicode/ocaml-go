open OUnit2
open Go
open Board

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

let suite =
  "Board Test Suite" >:::
  [
    "test_get_neighbours" >:: test_get_neighbours;
    "test_valid_coordinate" >:: test_valid_coordinate;
    "test_get_board" >:: test_get_board;
  ]

let () =
  run_test_tt_main suite