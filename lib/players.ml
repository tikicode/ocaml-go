(* player type: white, black, empty, hold by white and hold by black *)
type t = White | Black | Empty | WhiteHold | BlackHold [@@deriving eq]

let empty = Empty
let white = White
let black = Black
let is_empty p = p = Empty

(* blank player means player invisible (empty or hold by player ) *)
let is_blank p =
  match p with Empty | WhiteHold | BlackHold -> true | _ -> false

let is_white p = p = White

(*
let is_alive player pos =
  (* the pos is empty or hold by the player *)
  match player with
  | Black -> pos = Empty || pos = BlackHold
  | White -> pos = Empty || pos = WhiteHold
  | _ -> false
*)

let is_consistent p1 p2 =
  (* same player *)
  if p1 = p2 then true
  else
    match (p1, p2) with
    (* hold by itself *)
    | Black, BlackHold | BlackHold, Black -> true
    | White, WhiteHold | WhiteHold, White -> true
    (* not consistent *)
    | _ -> false

let is_same p1 p2 = p1 = p2

let to_char (p : t) : char =
  match p with White -> 'W' | Black -> 'B' | _ -> ' '

let opposite (p : t) : t = if p = White then Black else White

(* hold the position *)
let hold p = if p = White then WhiteHold else BlackHold

let to_string p =
  match p with
  | Black | BlackHold -> "Black"
  | White | WhiteHold -> "White"
  | _ -> ""
