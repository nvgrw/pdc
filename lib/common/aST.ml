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

let get_meta_binop = function
  | Or m -> m
  | And m -> m
  | Eq m -> m
  | Neq m -> m
  | Lt m -> m
  | Leq m -> m
  | Geq m -> m
  | Gt m -> m
  | Add m -> m
  | Subtract m -> m
  | Multiply m -> m
  | Divide m -> m

type 'm unop =
  | Negate of 'm
  | Not of 'm
[@@deriving show]

let get_meta_unop = function
  | Negate m -> m
  | Not m -> m

type 'm value =
  | Num of int * 'm
  | Real of float * 'm
  | Bool of bool * 'm
[@@deriving show]

let get_meta_value = function
  | Num (_, m) -> m
  | Real (_, m) -> m
  | Bool (_, m) -> m

type 'm typ =
  | Array of 'm typ * int * 'm
  | Int of 'm
  | Float of 'm
  | Char of 'm
  | Bool of 'm
[@@deriving show]

let get_meta_typ = function
  | Array (_, _, m) -> m
  | Int m -> m
  | Float m -> m
  | Char m -> m
  | Bool m -> m

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

let get_meta_expr = function
  | BinOp (_, _, _, m) -> m
  | UnOp (_, _, m) -> m
  | Const (_, m) -> m
  | Var (_, m) -> m
  | Typed (_, _, m) -> m

let get_meta_loc = function
  | Id (_, m) -> m
  | Deref (_, _, m) -> m
  | LTyped (_, _, m) -> m

type 'm decl =
  | Decl of 'm typ * string * 'm
[@@deriving show]

let get_meta_decl = function
  | Decl (_, _, m) -> m

let pp_scope (fmt: Format.formatter) (scope: ('m typ) Data.StringMap.t) =
  let pp_opaque = fun ofmt _ -> Format.pp_print_string ofmt "<opaque>" in
  Data.StringMap.iter (fun k v -> Format.fprintf fmt "%s %s" k (show_typ pp_opaque v)) scope

type 'm stmt =
  | Assign of 'm loc * 'm expr * 'm
  | ProbAssign of 'm loc * 'm expr list * 'm
  | If of 'm expr * 'm stmt * ('m stmt option) * 'm
  | While of 'm expr * 'm stmt * 'm
  | Do of 'm expr * 'm stmt * 'm
  | Break of 'm
  | Choose of 'm stmt list * int list * 'm
  | BlockStmt of 'm block * 'm
and 'm block =
  | Block of (('m typ) Data.StringMap.t [@printer pp_scope]) * ('m decl list) * ('m stmt list) * 'm
[@@deriving show]

let get_meta_stmt = function
  | Assign (_, _, m) -> m
  | ProbAssign (_, _, m) -> m
  | If (_, _, _, m) -> m
  | While (_, _, m) -> m
  | Do (_, _, m) -> m
  | Break m -> m
  | Choose (_, _, m) -> m
  | BlockStmt (_, m) -> m

let get_meta_block = function
  | Block (_, _, _, m) -> m

type 'm program = 'm block
[@@deriving show]

type 'm ast = [
  | `Binop of 'm binop
  | `Unop of 'm unop
  | `Value of 'm value
  | `Typ of 'm typ
  | `Expr of 'm expr
  | `Loc of 'm loc
  | `Decl of 'm decl
  | `Stmt of 'm stmt
  | `Block of 'm block
  | `Program of 'm program
  | `Root
]