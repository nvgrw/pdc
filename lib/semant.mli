open Common.AST
open Common.VisitorMonad
open Common.Meta
open Common.Context

val check: meta program -> (context, meta program, pass_error) res