open Common.Context

let generate mdl p =
  Codegen.LlvmPass.initialize mdl;
  Codegen.LlvmPass.process p C.empty