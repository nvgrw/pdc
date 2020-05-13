open Common.AST
open Common.VisitorMonad
open Common.Meta
open Common.Context

val generate: string option -> bool -> Llvm.llmodule -> meta program -> (context, meta program, pass_error) res