open Data
open AST
open Meta

module S = struct
  type context = { scopes: meta typ StringMap.t list }
  let empty = `Semantic { scopes = [] }
end

module C = struct
  type context = { values: Llvm.llvalue list; scopes: Llvm.llvalue StringMap.t list }
  let empty = `Codegen { values = []; scopes = [] }
end

type context = [ 
  | `Semantic of S.context
  | `Codegen of C.context
]