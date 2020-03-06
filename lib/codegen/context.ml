type context = { values: Llvm.llvalue list }

let empty_context = { values = [] }

type pass_error = 
  | Message of string