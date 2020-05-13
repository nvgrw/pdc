open AST
open VisitorMonad
open Meta
open Context

val scope_block_pre: meta ast -> meta block -> (context, meta block, pass_error) state
val scope_block_pos: meta ast -> meta block -> (context, meta block, pass_error) state
val get_typ: meta -> string -> (context, meta typ, pass_error) state
val get_llvalue: meta -> string -> (context, Llvm.llvalue * Native.Extra.llmetadata, pass_error) state