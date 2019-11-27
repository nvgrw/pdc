open Common.AST
open Common.VisitorMonad
open PassContext

val check: program -> (context, program) res