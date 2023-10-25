(* game state *)
type t

(* create a game *)
val init : int -> Players.t -> t

(* run the game *)
val run : t -> unit
