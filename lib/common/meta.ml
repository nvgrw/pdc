let pp_position fmt (pos: Lexing.position) = Format.fprintf fmt "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

type meta = 
  | Position of ((Lexing.position [@printer pp_position]) * (Lexing.position [@printer pp_position]))
[@@deriving show]

let dummy_meta = Position ((Lexing.dummy_pos, Lexing.dummy_pos))