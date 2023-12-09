open Players
open Board
open Core

module Game_controller : sig
  type t
  val get_dead_pieces : t -> string -> (int * int) list
  val init_game : int -> Go_players.t -> t
  val game_done : Board.t -> int -> int -> unit
  val check_done : Go_players.t -> int -> int -> bool
  val update_game : Board.t -> Go_players.t -> int -> int -> int -> t
  val check_coords : Board.t -> int * int -> bool

  val dfs : Board.t -> Go_players.t -> (int * int, 'a) Set.t -> (int * int) list -> bool

  val is_alive : Board.t -> Go_players.t -> int * int -> bool
  val check_move : Board.t -> Go_players.t -> int * int -> bool
  val take_pieces : Go_players.t -> Board.t -> Board.t * int
  val return_dead : Go_players.t -> Board.t -> (int * int) list
  val play_ai : t -> ai: (Board.t -> Go_players.t -> int -> int -> t) -> t

  val run : t  -> ai: (Board.t -> Go_players.t -> int -> int -> t) -> bool -> string -> t
  val run_two_player : t -> string -> t
  val run_player_vs_ai : t -> ai: (Board.t -> Go_players.t -> int -> int -> t) -> string -> t

  val run_console : t  -> ai: (Board.t -> Go_players.t -> int -> int -> t) -> bool -> unit
  val run_two_player_console : t -> unit
  val run_player_vs_ai_console : t -> ai: (Board.t -> Go_players.t -> int -> int -> t) -> unit
end

