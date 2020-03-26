open Common.Data

type context_t = { values: Llvm.llvalue list; scopes: Llvm.llvalue StringMap.t list }
type context = [ `Codegen of context_t ]

let empty_context: context = `Codegen { values = []; scopes = [] }