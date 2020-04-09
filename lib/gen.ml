open Common.Context
open Interop

let generate mdl p =
  let con = Llvm.global_context () in
  let stdlib_mdl = Stdlib.parse_stdlib_mdl () in
  Llvm.set_data_layout (Llvm.data_layout stdlib_mdl) mdl;
  Llvm_linker.link_modules' mdl stdlib_mdl;
  Stdlib.remove_exercise mdl;

  (* ** random *)
  let random_type = Llvm.function_type (Llvm.double_type con) [||] in
  ignore @@ Llvm.declare_function "random" random_type mdl;
  (* ------------- *)
  Codegen.LlvmPass.initialize mdl;
  Codegen.LlvmPass.process p C.empty