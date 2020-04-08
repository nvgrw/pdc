open Common.Context

let generate mdl p =
  let con = Llvm.global_context () in
  (* Extern stdlib *)
  let noalias = Llvm.create_enum_attr con "noalias" 0L in
  let nocapture = Llvm.create_enum_attr con "nocapture" 0L in

  (* ** printf *)
  let printf_type = Llvm.var_arg_function_type (Llvm.i32_type con) [| Llvm.pointer_type (Llvm.i8_type con) |] in
  let printf = Llvm.declare_function "printf" printf_type mdl in
  Llvm.add_function_attr printf noalias (Llvm.AttrIndex.Param 0);
  Llvm.add_function_attr printf nocapture (Llvm.AttrIndex.Param 0);

  (* ** random *)
  let random_type = Llvm.function_type (Llvm.double_type con) [||] in
  ignore @@ Llvm.declare_function "random" random_type mdl;
  (* ------------- *)
  Codegen.LlvmPass.initialize mdl;
  Codegen.LlvmPass.process p C.empty