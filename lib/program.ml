
type binop =
  | Or
  | And
  | Eq
  | Neq
  | Lt
  | Leq
  | Geq
  | Gt
  | Add
  | Subtract
  | Multiply
  | Divide
[@@deriving show]

type unop =
  | Negate
  | Not
[@@deriving show]

type value =
  | Num of int
  | Real of float
  | Bool of bool
[@@deriving show]

type expr =
  | BinOp of expr * binop * expr
  | UnOp of unop * expr
  | Const of value
  | Var of loc
and loc =
  | Id of string
  | Deref of loc * expr
[@@deriving show]

type typ =
  | Array of typ * int
  | Int
  | Float
  | Char
  | Bool
[@@deriving show]

type decl =
  | Decl of typ * string
[@@deriving show]

type stmt =
  | Assign of loc * expr
  | If of expr * stmt * (stmt option)
  | While of expr * stmt
  | Do of expr * stmt
  | Break
  | BlockStmt of block
and block =
  | Block of (decl list) * (stmt list)
[@@deriving show]

type program = block
[@@deriving show]