open Common.Context
open Interop

let generate filename_opt mdl p =
  let stdlib_mdl = Stdlib.parse_stdlib_mdl () in
  Llvm.set_data_layout (Llvm.data_layout stdlib_mdl) mdl;
  Llvm_linker.link_modules' mdl stdlib_mdl;
  Stdlib.remove_exercise mdl;
  (* ------------- *)

  let con = Llvm.module_context mdl in
  let difile = match filename_opt with
    | None -> Llvm.mdnull con
    | Some fn ->
      let (dir, base) = Core.Filename.split fn in
      Native.Extra.difile mdl { basename = base; directory = dir } in

  Codegen.LlvmPass.initialize mdl difile;
  Codegen.LlvmPass.process p C.empty