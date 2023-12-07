open Players

module Board : sig
  type t

  val init_board : int -> t
  val return_list : t -> Players.Go_players.t list list
  val print_board : t -> unit
  val valid_coordinate : t -> int * int -> bool
  val get_neighbours : t -> int * int -> (int * int) list
  val get_player : t -> int * int -> Go_players.t
  val update_board : t -> int * int -> Go_players.t -> t
  val get_board : t -> (int * int) list
  val count : t -> f:(Go_players.t -> bool) -> int
end
