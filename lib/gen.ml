open Common.Context
open Interop

let generate mdl p =
  let stdlib_mdl = Stdlib.parse_stdlib_mdl () in
  Llvm.set_data_layout (Llvm.data_layout stdlib_mdl) mdl;
  Llvm_linker.link_modules' mdl stdlib_mdl;
  Stdlib.remove_exercise mdl;
  (* ------------- *)

  Codegen.LlvmPass.initialize mdl;
  Codegen.LlvmPass.process p C.empty