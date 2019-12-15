open Common.AST

type context = { typeCheck: int }
type type_error =
  | IncompatibleBinOp of typ * binop * typ
  | IncompatibleUnOp of unop * typ
type pass_error = 
  | TypeError of type_error
  | Message of string