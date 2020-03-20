open Common.AST
open Common.Meta
open Common.Data

type context_t = { scopes: meta typ StringMap.t list }
type context = [ `Semantic of context_t ]
let empty_context: context = `Semantic { scopes = [] }