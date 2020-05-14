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
    scopes: (Llvm.llvalue * Extra.llmetadata) StringMap.t list;
    breakBlocks: Llvm.llbasicblock list;
    debugScopes: Extra.llmetadata list;
    debugSubprogram: Extra.llmetadata;
    debugLocalVariable: Extra.llmetadata;
  }
  let empty = `Codegen {
      values = [];
      blocks = [];
      scopes = [];
      debugScopes = [];
      breakBlocks = [];
      debugSubprogram = Extra.mdnull ();
      debugLocalVariable = Extra.mdnull ();
    }
end

type context = [
  | `Semantic of S.context
  | `Codegen of C.context
]