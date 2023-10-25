(* player type *)
type t

(* empty player *)
val empty : t

(* white player *)
val white : t

(* black player *)
val black : t

(* check whether a player is empty *)
val is_empty : t -> bool

(* check whether a player is blank *)
val is_blank : t -> bool

(* check whether is white piece *)
val is_white : t -> bool

(* check whether the second player is an alive issue for first player *)
(* val is_alive : t -> t -> bool *)

(* check whether two players are same *)
val is_same : t -> t -> bool

(* check whether two players are consistent *)
val is_consistent : t -> t -> bool

(* convert a player to char *)
val to_char : t -> char

(* get the opposite player *)
val opposite : t -> t

(* hold the position *)
val hold : t -> t

(* convert a player to string *)
val to_string : t -> string
