open Common.AST
open Common.Data

type type_error =
  | IncompatibleBinOp of typ * binop * typ
  | IncompatibleUnOp of unop * typ
type structural_error =
  | BadIdentifier of string
type pass_error = 
  | TypeError of type_error
  | StructuralError of structural_error
  | Message of string

type context = { scopes: typ StringMap.t list; other: int }
