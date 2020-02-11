open Common.AST
open Common.VisitorMonad
open PassContext

val process: meta program -> (context, meta program, pass_error) state