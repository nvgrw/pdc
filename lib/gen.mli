open Common.AST
open Common.VisitorMonad
open Common.Meta

open Codegen.Context

val generate: meta program -> (context, meta program, pass_error) res