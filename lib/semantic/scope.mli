open Common.AST
open Common.VisitorMonad
open Common.Meta

val scope_block_pre: meta block -> (Semantic.Context.context, meta block, pass_error) state
val scope_block_pos: meta block -> (Semantic.Context.context, meta block, pass_error) state
val get: meta -> string -> ('a, 'b, pass_error) state