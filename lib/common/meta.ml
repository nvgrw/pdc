open AST

let pp_position fmt (pos: Lexing.position) = Format.fprintf fmt "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

type meta =
  | Position of ((Lexing.position [@printer pp_position]) * (Lexing.position [@printer pp_position]))
[@@deriving show]

let dummy_meta = Position ((Lexing.dummy_pos, Lexing.dummy_pos))

type type_error =
  | IncompatibleBinOp of meta expr * meta typ * meta binop * meta typ
  | IncompatibleUnOp of meta expr * meta unop * meta typ
  | IncompatibleAssignment of meta stmt * meta typ * meta typ
  | IfRequiresBoolean of meta stmt
  | WhileRequiresBoolean of meta stmt
  | DoRequiresBoolean of meta stmt
  | UntypedSubExpressions of meta expr
  | UntypedSubLocations of meta loc
  | UntypedStatementFragment of meta stmt
  | BadIntPrecision of meta typ * int
type structural_error =
  | BadIdentifier of meta * string
  | DuplicateIdentifier of meta decl * string
  | ChooseInvalidWeight of meta stmt * int
type codegen_error =
  | ValueStackEmpty
  | BlockStackEmpty
  | CannotGenerateExpression of meta expr
  | NegateOnlyIntOrFloat of meta expr
  | CannotGenerateLocation of meta loc
  | ModuleVerification of string
  | ProbAssignNotLowered of meta stmt
  | UnimplementedStatement of meta stmt
type pass_error =
  | TypeError of type_error
  | StructuralError of structural_error
  | CodegenError of codegen_error
  | Message of string