open Core

(* board tow-dim list and size *)
type t = { board : Players.t list list; size : int }

let init size =
  let board =
    (* create an all empty board *)
    List.init size ~f:(fun _ -> List.init size ~f:(fun _ -> Players.empty))
  in
  { board; size }

let valid_coordinate bd (row, col) =
  (* check index range *)
  row >= 0 && col >= 0 && row < bd.size && col < bd.size

let get_neighbours board (x, y) =
  (* generate all neighbours *)
  let coords = [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ] in
  (* filter, keep only valid coordinates *)
  List.filter coords ~f:(fun coord -> valid_coordinate board coord)

let print { board; size } =
  (* helper function to print rows *)
  let rec print_rows bd n =
    match bd with
    | [] -> ()
    | row :: rs ->
        (* print row number *)
        Printf.printf "%2d " n;
        (* print row elements *)
        List.iter row ~f:(fun p -> Printf.printf "[%c]" (Players.to_char p));
        Out_channel.newline stdout;
        (* print left rows *)
        print_rows rs (n + 1)
  in
  (* print column numbers *)
  print_string "   ";
  for i = 1 to size do
    Printf.printf "%2d " i
  done;
  Out_channel.newline stdout;
  (* print rows *)
  print_rows board 1

(* the caller must ensure coordiniate is valid *)
let get_player bd (x, y) = List.nth_exn (List.nth_exn bd.board x) y

let update_board { board; size } (x, y) p =
  (* rows before the target row *)
  let part1 = List.take board x in
  (* rows start with the target row *)
  let part = List.drop board x in
  (* the target row *)
  let row = List.hd_exn part in
  (* rows after the target row *)
  let part2 = List.tl_exn part in
  (* columns before the target column *)
  let col_part1 = List.take row y in
  (* columns start with the target column *)
  let col_part = List.drop row y in
  (* columns after the target column *)
  let col_part2 = List.tl_exn col_part in
  (* create new row *)
  let new_row = col_part1 @ (p :: col_part2) in
  (* create new board *)
  { board = part1 @ (new_row :: part2); size }

let all_coordinates bd =
  (* helper function to get all coordinates *)
  let rec aux x y coords =
    (* check boundary *)
    match (x = bd.size, y = bd.size) with
    (* done *)
    | true, _ -> coords
    (* update x coord *)
    | false, true -> aux (x + 1) 0 coords
    (* update y coord *)
    | _ -> aux x (y + 1) ((x, y) :: coords)
  in
  aux 0 0 []

let count bd ~f =
  (* helper function to count row *)
  let count_row row =
    List.fold row ~init:0 ~f:(fun acc p -> if f p then acc + 1 else acc)
  in
  (* count all rows *)
  List.fold bd.board ~init:0 ~f:(fun acc row -> acc + count_row row)
