open Board
open Players

module MCTS : sig
  type t

  val init_state : int -> t
  val update_slots : Go_players.t -> int -> int -> int -> int * int

  val random_move :
    Board.t -> Go_players.t -> int -> int -> Board.t * (int * int) * int * int

  val init_node : Board.t -> Go_players.t -> int -> int -> string -> t
  val result : t -> int
  val backpropagate : t list -> int -> t -> t
  val expand_node : t -> t
  val select_child : t -> t option
  val tree_policy : t -> t
  val monte_carlo_tree_search : t -> int -> t * (int * int)
end

val open_center_positions : Board.t -> (int * int) list
val random_player : Board.t -> Go_players.t -> string
