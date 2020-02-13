open Common.AST
open Common.VisitorMonad
open Common.Meta
open PassContext

val scope_block_pre: meta block -> (context, meta block, pass_error) state
val scope_block_pos: meta block -> (context, meta block, pass_error) state
val get_type: string -> (context, meta typ, pass_error) state