open Core
open Game_controller
open Board
open Players
open Rules

let open_center_positions (bd : Board.t) : (int * int) list =
  let n = Board.get_size bd in
  let center_start = n / 4 in
  let center_end = (3 * (n / 4)) - 1 in
  let add_if_blank (coord : int * int) : bool =
    let p = Board.get_player bd coord in
    if Go_players.is_blank p then true else false
  in
  let create_pairs range =
    List.concat_map ~f:(fun x -> List.map ~f:(fun y -> (x, y)) range) range
  in
  let range =
    List.init (center_end - center_start + 1) ~f:(fun i -> i + center_start)
  in
  List.filter ~f:add_if_blank (create_pairs range)

let rec random_player (bd : Board.t) (player : Go_players.t) (black_slots : int)
    (white_slots : int) : Game_controller.t =
  let next_move =
    match open_center_positions bd with
    | [] ->
        (bd |> Board.get_size |> Random.int, bd |> Board.get_size |> Random.int)
    | lst -> lst |> List.length |> Random.int |> List.nth_exn lst
  in
  if not (Board.valid_coordinate bd next_move) then
    random_player bd player black_slots white_slots
  else
    let p = Board.get_player bd next_move in
    if Go_players.is_blank p then
      let new_board = Board.update_board bd next_move player in
      let occupied_board, pieces =
        Game_controller.take_pieces player new_board
      in
      if Rules.check_move occupied_board player next_move then
        Game_controller.update_game occupied_board player black_slots
          white_slots pieces
      else random_player bd player black_slots white_slots
    else random_player bd player black_slots white_slots

(* module MCTS = struct
  type t = {
    bd : Board.t;
    player : Go_players.t;
    black_slots : int;
    white_slots : int;
    move : (int * int);
    mutable visits : int;
    mutable wins : int;
    mutable children : t list;
  }

  let get_board (state : t) : Board.t = state.bd
  let get_player (state : t) : Go_players.t = state.player
  let get_black_slots (state : t) : int = state.black_slots
  let get_white_slots (state : t) : int = state.white_slots
  let get_move (state : t) : (int * int)= state.move
  let get_visits (state : t) : int = state.visits
  let get_wins (state : t) : int = state.wins
  let get_children (state : t) : t list = state.children

  let rec random_move (bd : Board.t) (player : Go_players.t) : int * int =
    let next_move =
      (bd |> Board.get_size |> Random.int, bd |> Board.get_size |> Random.int)
    in
    let p = Board.get_player bd next_move in
    if Go_players.is_blank p then
      let new_board = Board.update_board bd next_move player in
      let occupied_board, _ = Game_controller.take_pieces player new_board in
      if Game_controller.check_move occupied_board player next_move then
        next_move
      else random_move bd player
    else random_move bd player

  let rec expand_node (node : t) =
    let move = random_move cur_game player in
    let child_state = Board.update_board cur_game move player in
    let child_node = { bd = child_state; player; black_slots = 0; white_slots = 0; move; visits = 0; wins = 0; children = [] } in
    node.children <- child_node :: node.children;
    simulate cur_game child_state;
    backpropagate node (result cur_game child_state)

     and simulate initial_state state =
       (* pseudo for game state*)
       let rec simulate_until_end state =
         let legal_moves = legal_moves state in
         (* pseudo for checking legal move, may create func *)
         match legal_moves with
         | [] -> result initial_state state
         | _ ->
             let move = random_move legal_moves in
             make_move state move;
             simulate_until_end state
       in
       simulate_until_end state

     and backpropagate node result =
       node.visits <- node.visits + 1;
       node.wins <- node.wins + result;
       backpropagate (List.hd node.children) result

     and result initial_state state =
       let size = Array.length initial_state.board in
       (* array for mutability *)
       let score =
         if has_winner state then if state.player = Black then 1 else -1
         else if is_board_full state then 0
         else simulate initial_state state
       in
       if state.player = Black then score else -score

     and has_winner state =
       (* TODO *)
       false

     and is_board_full state =
       (* TODO *)
       false

     let mcts state iterations =
       let root = { move = None; visits = 0; wins = 0; children = [] } in
       (* make move optional ? *)
       for _ = 1 to iterations do
         let node = select_node state root in
         expand_node state node
       done;
       let best_move = select_best_move root in
       best_move.move

     and select_node state node =
       if node.children = [] then node
       else if Random.float 1.0 < 0.7 then
         (* selection criterion *)
         select_node state (List.hd node.children)
       else select_node state (uct_best_child node)

     and uct_best_child node =
       let log_total_visits = log (float_of_int node.visits) in
       let uct_value child =
         let exploitation_term =
           float_of_int child.wins /. float_of_int child.visits
         in
         let exploration_term =
           sqrt (2. *. log_total_visits /. float_of_int child.visits)
         in
         exploitation_term +. exploration_term
       in
       List.max_by uct_value node.children

     and select_best_move root =
       let best_child =
         List.max_by (fun child -> float_of_int child.visits) root.children
       in
       match best_child with
       | None -> failwith "No valid moves found"
       | Some child -> child

     let train size iterations =
       (* very rudimentary training code *)
       (* let initial_state = { board = create_board size; player_turn = Black } in
       let best_move = mcts initial_state iterations in
       Printf.printf "Best Move: (%d, %d)\n" (fst best_move + 1) (snd best_move + 1) *)
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
   * bing bong i am sleepy *) *)