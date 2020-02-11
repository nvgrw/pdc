open Common.AST
open Common.VisitorMonad
open PassContext

val check: meta program -> (context, meta program, pass_error) res