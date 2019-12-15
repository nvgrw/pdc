open Common.AST
open Common.VisitorMonad
open PassContext

val process: program -> (context, program, pass_error) state