open Core
open Players

module Board = struct
  type t = { board : Go_players.t list list; size : int }

  let init_board (size : int) : t =
    let board =
      List.init size ~f:(fun _ -> List.init size ~f:(fun _ -> Go_players.empty))
    in
    { board; size }

  let return_list (bd : t) : Go_players.t list list = bd.board

  let print_board (bd : t) : unit =
    let rec print_rows (board : Go_players.t list list) (n : int) : unit =
      match board with
      | [] -> ()
      | row :: rs ->
          Printf.printf "%2d " n;
          List.iter row ~f:(fun p ->
              Printf.printf "[%c]" (Go_players.to_char p));
          Out_channel.newline stdout;
          print_rows rs (n + 1)
    in
    print_string "   ";
    for i = 1 to bd.size do
      Printf.printf "%2d " i
    done;
    Out_channel.newline stdout;
    print_rows bd.board 1

  let valid_coordinate (bd : t) ((row, col) : int * int) : bool =
    row >= 0 && col >= 0 && row < bd.size && col < bd.size

  let get_neighbours (bd : t) ((x, y) : int * int) : (int * int) list =
    let coords = [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ] in
    List.filter coords ~f:(fun coord -> valid_coordinate bd coord)

  let get_player (bd : t) ((x, y) : int * int) : Go_players.t =
    List.nth_exn (List.nth_exn bd.board x) y

  let update_board ({ board; size } : t) ((x, y) : int * int) (p : Go_players.t)
      : t =
    let rows_before = List.take board x in
    let rows_cur_after = List.drop board x in
    let target_row = List.hd_exn rows_cur_after in
    let rows_after = List.tl_exn rows_cur_after in
    let cols_before = List.take target_row y in
    let cols_cur_after = List.drop target_row y in
    let cols_after = List.tl_exn cols_cur_after in
    let new_row = cols_before @ (p :: cols_after) in
    { board = rows_before @ (new_row :: rows_after); size }

  let get_board (bd : t) : (int * int) list =
    let rec aux (x : int) (y : int) (coords : (int * int) list) :
        (int * int) list =
      match (x = bd.size, y = bd.size) with
      | true, _ -> coords
      | false, true -> aux (x + 1) 0 coords
      | _ -> aux x (y + 1) ((x, y) :: coords)
    in
    aux 0 0 []

  let get_size (bd : t) : int = bd.size

  let count (bd : t) ~f : int =
    let count_row (row : Go_players.t list) =
      List.fold row ~init:0 ~f:(fun acc p -> if f p then acc + 1 else acc)
    in
    List.fold bd.board ~init:0 ~f:(fun acc row -> acc + count_row row)
end
