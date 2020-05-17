open Common.Context
open Interop

let generate filename_opt is_optimized is_debug mdl p =
  (* Add Stdlib *)
  let stdlib_mdl = Stdlib.parse_stdlib_mdl () in
  Llvm.set_data_layout (Llvm.data_layout stdlib_mdl) mdl;
  Llvm_linker.link_modules' mdl stdlib_mdl;
  Stdlib.remove_exercise mdl;
  (* ---------- *)


  Codegen.LlvmPass.initialize mdl;
  if is_debug then begin
    (* Debug Setup *)
    Extra.extra_add_int_module_flag mdl "Debug Info Version" 3;
    Extra.extra_add_int_module_flag mdl "Dwarf Version" 4;
    (* ----------- *)
    let difile = match filename_opt with
      | None -> Extra.mdnull ()
      | Some fn ->
        let (dir, base) = Core.Filename.split fn in
        Extra.difile mdl { basename = base; directory = dir } in
    Codegen.LlvmPass.process p @@ `Codegen {
      C.empty with
      C.debug = is_debug;
      debugCompileUnit = Extra.dicompileunit mdl {
          Extra.DICompileUnit.file = difile;
          producer =  "pdc";
          isOptimized = is_optimized;
        };
      debugFile = difile;
    }
  end else
    Codegen.LlvmPass.process p @@ `Codegen { C.empty with C.debug = is_debug }