open Data
open AST
open Meta

module S = struct
  type context = { scopes: meta typ StringMap.t list }
end

module C = struct
  type context = { values: Llvm.llvalue list; scopes: Llvm.llvalue StringMap.t list }
end

type context = [ 
  | `Semantic of S.context
  | `Codegen of C.context
]

let empty_context = `Semantic { S.scopes = [] }
let empty_context = `Codegen { C.values = []; C.scopes = [] }