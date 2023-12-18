open Board
open Players
open Core

module Rules : sig
  val game_done_black_score : Board.t -> int
  val game_done_white_score : Board.t -> int

  val dfs :
    Board.t -> Go_players.t -> (int * int, 'a) Set.t -> (int * int) list -> bool

  val is_alive : Board.t -> Go_players.t -> int * int -> bool
  val check_move : Board.t -> Go_players.t -> int * int -> bool
  val check_done : Go_players.t -> int -> int -> bool
  val check_coords : Board.t -> int * int -> bool
end
