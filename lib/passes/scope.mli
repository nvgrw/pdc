open Common.AST
open Common.VisitorMonad
open PassContext

val scope_block_pre: block -> (context, block, pass_error) state
val scope_block_pos: block -> (context, block, pass_error) state
val get_type: string -> (context, typ, pass_error) state