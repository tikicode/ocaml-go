open Players
open Board

module Game_controller : sig
  type t

  val init_game : int -> Go_players.t -> t
  val get_dead_pieces : t -> string -> (int * int) list
  val game_done : Board.t -> int -> int -> unit
  val update_game : Board.t -> Go_players.t -> int -> int -> int -> t
  val conv_string_to_pair_list : string -> (int * int) list
  val return_black_slots : t -> int
  val return_white_slots : t -> int
  val return_board : t -> Board.t
  val return_player : t -> string
  val next_move_ai : t -> string
  val take_pieces : Go_players.t -> Board.t -> Board.t * int
  val return_dead : Go_players.t -> Board.t -> (int * int) list
  val play_ai : t -> ai:(Board.t -> Go_players.t -> int -> int -> t) -> t
  val pass_turn : t -> t
  val get_white_slots : t -> int

  val run :
    t -> ai:(Board.t -> Go_players.t -> int -> int -> t) -> bool -> string -> t

  val run_two_player : t -> string -> t

  val run_player_vs_ai :
    t -> ai:(Board.t -> Go_players.t -> int -> int -> t) -> string -> t

  val run_console :
    t -> ai:(Board.t -> Go_players.t -> int -> int -> t) -> bool -> unit

  val run_two_player_console : t -> unit

  val run_player_vs_ai_console :
    t -> ai:(Board.t -> Go_players.t -> int -> int -> t) -> unit
end
