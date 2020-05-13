open Common.AST
open Common.VisitorMonad
open Common.Meta
open Common.Context

val initialize: Llvm.llmodule -> Native.Extra.llmetadata -> Native.Extra.llmetadata -> unit
val process: meta program -> (context, meta program, pass_error) state