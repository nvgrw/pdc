type 'm binop =
  | Or of 'm
  | And of 'm
  | Eq of 'm
  | Neq of 'm
  | Lt of 'm
  | Leq of 'm
  | Geq of 'm
  | Gt of 'm
  | Add of 'm
  | Subtract of 'm
  | Multiply of 'm
  | Divide of 'm
[@@deriving show]

type 'm unop =
  | Negate of 'm
  | Not of 'm
[@@deriving show]

type 'm value =
  | Num of int * 'm
  | Real of float * 'm
  | Bool of bool * 'm
[@@deriving show]

type 'm typ =
  | Array of 'm typ * int * 'm
  | Int of 'm
  | Float of 'm
  | Char of 'm
  | Bool of 'm
[@@deriving show]

type 'm expr =
  | BinOp of 'm expr * 'm binop * 'm expr * 'm
  | UnOp of 'm unop * 'm expr * 'm
  | Const of 'm value * 'm
  | Var of 'm loc * 'm
  | Typed of 'm typ * 'm expr * 'm
and 'm loc =
  | Id of string * 'm
  | Deref of 'm loc * 'm expr * 'm
  | LTyped of 'm typ * 'm loc * 'm
[@@deriving show]

type 'm decl =
  | Decl of 'm typ * string * 'm
[@@deriving show]

let pp_scope (fmt: Format.formatter) (scope: ('m typ) Data.StringMap.t) = 
  let pp_opaque = fun ofmt _ -> Format.pp_print_string ofmt "<opaque>" in
  Data.StringMap.iter (fun k v -> Format.fprintf fmt "%s %s" k (show_typ pp_opaque v)) scope

type 'm stmt =
  | Assign of 'm loc * 'm expr * 'm
  | If of 'm expr * 'm stmt * ('m stmt option) * 'm
  | While of 'm expr * 'm stmt * 'm
  | Do of 'm expr * 'm stmt * 'm
  | Break of 'm
  | BlockStmt of 'm block * 'm
and 'm block =
  | Block of (('m typ) Data.StringMap.t [@printer pp_scope]) * ('m decl list) * ('m stmt list) * 'm
[@@deriving show]


type 'm program = 'm block
[@@deriving show]