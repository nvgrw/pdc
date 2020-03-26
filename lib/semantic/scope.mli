open Common.AST
open Common.VisitorMonad
open Common.Meta

open Context

val scope_block_pre: meta block -> (context, meta block, pass_error) state
val scope_block_pos: meta block -> (context, meta block, pass_error) state
val get: meta -> string -> (context, meta typ, pass_error) state