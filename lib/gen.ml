open Common.Context
open Interop

let generate filename_opt mdl p =
  let stdlib_mdl = Stdlib.parse_stdlib_mdl () in
  Llvm.set_data_layout (Llvm.data_layout stdlib_mdl) mdl;
  Llvm_linker.link_modules' mdl stdlib_mdl;
  Stdlib.remove_exercise mdl;
  (* ------------- *)

  let difile = match filename_opt with
    | None -> Native.Extra.mdnull ()
    | Some fn ->
      let (dir, base) = Core.Filename.split fn in
      Native.Extra.difile mdl { basename = base; directory = dir } in
  let dicompileunit = Native.Extra.dicompileunit mdl {
      Native.Extra.DICompileUnit.file = difile;
      producer =  "pdc";
      isOptimized = true; (* todo: figure out what to do with this *)
    } in

  Codegen.LlvmPass.initialize mdl difile dicompileunit;
  Codegen.LlvmPass.process p C.empty