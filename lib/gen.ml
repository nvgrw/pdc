open Common.Context
open Interop

let generate filename_opt is_optimized mdl p =
  (* Add Stdlib *)
  let stdlib_mdl = Stdlib.parse_stdlib_mdl () in
  Llvm.set_data_layout (Llvm.data_layout stdlib_mdl) mdl;
  Llvm_linker.link_modules' mdl stdlib_mdl;
  Stdlib.remove_exercise mdl;
  (* ---------- *)

  (* Debug Setup *)
  let difile = match filename_opt with
    | None -> Native.Extra.mdnull ()
    | Some fn ->
      let (dir, base) = Core.Filename.split fn in
      Native.Extra.difile mdl { basename = base; directory = dir } in
  let dicompileunit = Native.Extra.dicompileunit mdl {
      Native.Extra.DICompileUnit.file = difile;
      producer =  "pdc";
      isOptimized = is_optimized;
    } in
  Native.Extra.extra_add_int_module_flag mdl "Debug Info Version" 3;
  Native.Extra.extra_add_int_module_flag mdl "Dwarf Version" 4;
  (* ----------- *)

  Codegen.LlvmPass.initialize mdl difile dicompileunit;
  Codegen.LlvmPass.process p C.empty