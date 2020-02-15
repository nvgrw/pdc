open Common.AST
open Common.VisitorMonad
open Common.Meta

open Semantic.Context

val check: meta program -> (context, meta program, pass_error) res