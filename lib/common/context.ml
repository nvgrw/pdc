open Data
open AST
open Meta

module S = struct
  type context = { scopes: meta typ StringMap.t list }
  let empty = `Semantic { scopes = [] }
end

module C = struct
  type context = {
    values: Llvm.llvalue list;
    blocks: Llvm.llbasicblock list;
    scopes: Llvm.llvalue StringMap.t list;
    breakBlocks: Llvm.llbasicblock list;
    debugScopes: Native.Extra.llmetadata list;
    debugSubprogram: Native.Extra.llmetadata
  }
  let empty = `Codegen {
      values = [];
      blocks = [];
      scopes = [];
      debugScopes = [];
      breakBlocks = [];
      debugSubprogram = Native.Extra.mdnull ()
    }
end

type context = [
  | `Semantic of S.context
  | `Codegen of C.context
]