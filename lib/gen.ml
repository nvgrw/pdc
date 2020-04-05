open Common.Context

let generate mdl p =
  (* Extern stdlib *)
  let con = Llvm.global_context () in
  let random_type = Llvm.function_type (Llvm.double_type con) [||] in
  ignore @@ Llvm.declare_function "random" random_type mdl;
  (* ------------- *)
  Codegen.LlvmPass.initialize mdl;
  Codegen.LlvmPass.process p C.empty