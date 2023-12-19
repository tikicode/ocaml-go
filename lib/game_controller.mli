open Players
open Board

module Game_controller : sig
  type t

  val init_game : int -> Go_players.t -> t
  val get_dead_pieces : t -> string -> (int * int) list
  val game_done : Board.t -> int -> int -> unit
  val update_game : Board.t -> Go_players.t -> int -> int -> int -> t
  val conv_string_to_pair : string -> int * int
  val return_black_slots : t -> int
  val return_white_slots : t -> int
  val return_board : t -> Board.t
  val return_player : t -> Go_players.t
  val return_player_name : t -> string
  val return_dead : Go_players.t -> Board.t -> (int * int) list
  val pass_turn : t -> t
  val run : t -> string -> t
  val run_console : t -> ai:(Board.t -> Go_players.t -> string) -> bool -> unit
  val run_two_player_console : t -> unit

  val run_player_vs_ai_console :
    t -> ai:(Board.t -> Go_players.t -> string) -> unit
end
