module Player = struct
  type t = char

  let switch_turn current_player =
    if current_player = 'B' then 'W' else 'B'

  let print_turn current_player =
    Printf.sprintf "Player %c" current_player
end