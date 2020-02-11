open Common.AST
open Common.Data

type meta = Position of int * int [@@deriving show]
val dummy_meta: meta

type type_error =
  | IncompatibleBinOp of meta typ * meta binop * meta typ
  | IncompatibleUnOp of meta unop * meta typ
  | IncompatibleAssignment of meta typ * meta typ
  | IfRequiresBoolean 
  | WhileRequiresBoolean
  | DoRequiresBoolean
  | UntypedSubExpressions of meta expr
  | UntypedSubLocations of meta loc
  | UntypedStatementFragment of meta stmt
type structural_error =
  | BadIdentifier of string
  | DuplicateIdentifier of string
type pass_error = 
  | TypeError of type_error
  | StructuralError of structural_error
  | Message of string

type context = { scopes: meta typ StringMap.t list; other: int }
