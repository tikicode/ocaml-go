(* board type *)
type t

(* create a board *)
val init : int -> t

(* print the board *)
val print : t -> unit

(* check whether a coordinate is valid *)
val valid_coordinate : t -> int * int -> bool

(* get all neighbous of a coordinate *)
val get_neighbours : t -> int * int -> (int * int) list

(* get the player at coordinate (must be valid) *)
val get_player : t -> int * int -> Players.t

(* update the player at coordinate *)
val update_board : t -> int * int -> Players.t -> t

(* get all cordinates of board *)
val all_coordinates : t -> (int * int) list

(* count the player number satisfy function f *)
val count : t -> f:(Players.t -> bool) -> int
