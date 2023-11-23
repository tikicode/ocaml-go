module Go_players : sig
  type t

  val empty : t
  val white : t
  val black : t
  val is_empty : t -> bool
  val is_blank : t -> bool
  val is_white : t -> bool
  val is_same : t -> t -> bool
  val is_consistent : t -> t -> bool
  val to_char : t -> char
  val opposite : t -> t
  val hold : t -> t
  val to_string : t -> string
end
