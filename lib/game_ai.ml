open Core
open Board
open Players
open Rules

let open_center_positions (bd : Board.t) : (int * int) list =
  let n = bd |> Board.get_size in
  let center_start = n / 4 in
  let center_end = (3 * (n / 4)) - 1 in
  let add_if_blank (coord : int * int) : bool =
    let p = Board.get_player bd coord in
    if p |> Go_players.is_blank then true else false
  in
  let create_pairs range =
    List.concat_map ~f:(fun x -> List.map ~f:(fun y -> (x, y)) range) range
  in
  let range =
    List.init (center_end - center_start + 1) ~f:(fun i -> i + center_start)
  in
  List.filter ~f:add_if_blank (create_pairs range)

let ints_to_string ((x, y) : int * int) : string =
  string_of_int (x + 1) ^ " " ^ string_of_int (y + 1)

let random_player (bd : Board.t) (player : Go_players.t) : string =
  let open_center_pos = bd |> open_center_positions in
  let rec check_move (bd : Board.t) (player : Go_players.t) : string =
    let next_move =
      match open_center_pos with
      | [] ->
          ( bd |> Board.get_size |> Random.int,
            bd |> Board.get_size |> Random.int )
      | lst -> lst |> List.length |> Random.int |> List.nth_exn lst
    in
    if not (Rules.check_coords bd next_move) then check_move bd player
    else
      let p = Board.get_player bd next_move in
      if p |> Go_players.is_blank then
        let new_board = Board.update_board bd next_move player in
        let occupied_board, _ = Rules.take_pieces player new_board in
        if Rules.check_move occupied_board player next_move then
          next_move |> ints_to_string
        else check_move bd player
      else check_move bd player
  in
  check_move bd player

module MCTS = struct
  type t = {
    bd : Board.t;
    player : Go_players.t;
    black_slots : int;
    white_slots : int;
    move : int * int;
    mutable visits : int;
    mutable wins : int;
    mutable children : t list;
  }

  let init_state (size : int) : t =
    {
      bd = Board.init_board size;
      player = Go_players.black;
      black_slots = 0;
      white_slots = 0;
      move = (-1, -1);
      visits = 0;
      wins = 0;
      children = [];
    }

  let update_slots (player : Go_players.t) (black_slots : int)
      (white_slots : int) (pieces : int) : int * int =
    if pieces = 0 then (black_slots - 1, white_slots - 1)
    else if Go_players.is_white player then
      (black_slots + pieces - 2, white_slots + pieces - 1)
    else (black_slots + pieces - 1, white_slots + pieces - 2)

  let rec random_move (bd : Board.t) (player : Go_players.t) (black_slots : int)
      (white_slots : int) : Board.t * (int * int) * int * int =
    let next_move =
      (bd |> Board.get_size |> Random.int, bd |> Board.get_size |> Random.int)
    in
    let p = Board.get_player bd next_move in
    if p |> Go_players.is_blank then
      let new_board = Board.update_board bd next_move player in
      let occupied_board, pieces = Rules.take_pieces player new_board in
      let bs, ws = update_slots player black_slots white_slots pieces in
      if Rules.check_move occupied_board player next_move then
        (occupied_board, next_move, bs, ws)
      else random_move bd player black_slots white_slots
    else random_move bd player black_slots white_slots

  let result (node : t) : int =
    let b_res = node.bd |> Rules.game_done_black_score in
    let w_res = node.bd |> Rules.game_done_white_score in
    if b_res > w_res then 1 else if b_res < w_res then -1 else 0

  let rec backpropagate (stack : t list) (result : int) (prev_node : t) : t =
    match stack with
    | [] -> prev_node (* return head *)
    | cur_node :: rest ->
        cur_node.visits <- cur_node.visits + 1;
        cur_node.wins <- cur_node.wins + result;
        backpropagate rest result cur_node

  let create_new_node (node : t) : t =
    let bd, move, black_slots, white_slots =
      random_move node.bd node.player node.black_slots node.white_slots
    in
    let player = node.player |> Go_players.opposite in
    {
      bd;
      player;
      black_slots;
      white_slots;
      move;
      visits = 0;
      wins = 0;
      children = [];
    }

  let expand_node (n : t) : t =
    let rec simulate (node : t) (stack : t list) (depth : int) =
      let child_node = node |> create_new_node in
      node.children <- child_node :: node.children;
      let new_stack = child_node :: stack in
      if
        depth = 250
        || Rules.check_done node.player node.black_slots node.white_slots
      then backpropagate new_stack (child_node |> result) child_node
      else simulate child_node new_stack (depth + 1)
    in
    simulate n [ n ] 1

  let select_child (node : t) : t option =
    match node.children with
    | [] -> None
    | child :: rest ->
        let best_child =
          List.fold
            ~f:(fun acc cur ->
              let acc_score =
                float_of_int acc.wins /. float_of_int acc.visits
              in
              let cur_score =
                float_of_int cur.wins /. float_of_int cur.visits
              in
              if Float.( > ) cur_score acc_score then cur else acc)
            ~init:child rest
        in
        Some best_child

  let rec tree_policy (node : t) : t =
    match select_child node with
    | Some child -> if child.visits = 0 then child else child |> tree_policy
    | None -> node

  let monte_carlo_tree_search (root : t) (num_simulations : int) :
      t * (int * int) =
    let rec run_sims (node : t) (iter : int) : t * (int * int) =
      if iter < num_simulations then
        let new_root = node |> tree_policy |> expand_node in
        run_sims new_root (iter + 1)
      else
        match select_child root with
        | Some child -> (child, child.move)
        | None ->
            let child = node |> create_new_node in
            (child, child.move)
    in
    run_sims root 0
end

(* ideas for impl
   * when running expand, only expand n number of moves (keep it constant, say 250)
     or expand until the game ends
   * at the end of the search, return a tuple of the necessary move and of the root
   of the tree (at this point, the human player's last move) so we can use it to expand
   the next set and retain any memory we may have
   * at each step, try to explore every child of the root at least once
   * determine best move with uct from child
   * if equivalents, randomly select
   * see if we can train on some games already played first
   * Figure out async, can we just wait until the games are played out? Will take
   longer at the start of the game and shorter at the end
   * compatibility with both local CL variants and posted variants of the game?
   perhaps we change ai function to only return a move rather than the entire
   game state
   * bing bong i am sleepy *)
