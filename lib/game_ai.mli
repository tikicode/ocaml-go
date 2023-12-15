open Game_controller
open Board
open Players

(* module MCTS : sig
     type t = {
       move: (int * int) option;
       mutable visits: int;
       mutable wins: int;
       mutable children: t list;
     }
     (* game_state needs to be converted to node game type  TODO *)
     val random_move : Board.t -> Go_players.t -> int -> int -> int * int
     val expand_node : Board.t -> Go_players.t -> int -> int  -> t -> unit
     val simulate : game_state -> game_state -> int
     val backpropagate : t -> int -> unit
     val result : game_state -> game_state -> int
     val has_winner : game_state -> bool
     val is_board_full : game_state -> bool
     val mcts : game_state -> int -> move
     val select_node : game_state -> t -> t
     val uct_best_child : t -> t
     val select_best_move : t -> t
     val play_game : int -> int -> unit
   end *)

val open_center_positions : Board.t -> (int * int) list
val random_player : Board.t -> Go_players.t -> int -> int -> Game_controller.t
