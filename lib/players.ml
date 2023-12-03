module Go_players = struct
  type t = White | Black | Empty | WhiteHold | BlackHold [@@deriving eq]

  let empty = Empty
  let white = White
  let black = Black
  let blackhold = BlackHold
  let whitehold = WhiteHold
  let is_empty (p : t) : bool = p = Empty

  let is_blank (p : t) : bool =
    match p with Empty | WhiteHold | BlackHold -> true | _ -> false

  let is_white (p : t) : bool = p = White

  let is_consistent (p1 : t) (p2 : t) : bool =
    if p1 = p2 then true
    else
      match (p1, p2) with
      | Black, BlackHold | BlackHold, Black -> true
      | White, WhiteHold | WhiteHold, White -> true
      | _ -> false

  let is_same (p1 : t) (p2 : t) : bool = p1 = p2

  let to_char (p : t) : char =
    match p with White -> 'W' | Black -> 'B' | _ -> ' '

  let opposite (p : t) : t = if p = White then Black else White
  let hold (p : t) : t = if p = White then WhiteHold else BlackHold

  let to_string (p : t) : string =
    match p with
    | Black | BlackHold -> "Black"
    | White | WhiteHold -> "White"
    | _ -> ""
end
