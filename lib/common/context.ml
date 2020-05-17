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
    debug: bool;
    debugScopes: Extra.llmetadata list;
    debugSubprogram: Extra.llmetadata;
    debugLocalVariable: Extra.llmetadata;
    debugCompileUnit: Extra.llmetadata;
    debugFile: Extra.llmetadata;
  }
  let empty = {
    values = [];
    blocks = [];
    scopes = [];
    debug = false;
    debugScopes = [];
    breakBlocks = [];
    debugSubprogram = Extra.mdnull ();
    debugLocalVariable = Extra.mdnull ();
    debugCompileUnit = Extra.mdnull ();
    debugFile = Extra.mdnull ();
  }
  let empty_wrapped = `Codegen empty
end

type context = [
  | `Semantic of S.context
  | `Codegen of C.context
]