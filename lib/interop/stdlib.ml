
let parse_stdlib_mdl () =
  let buf = Llvm.MemoryBuffer.of_string [%blob "../../stdlib/build/llvm.bc"] in
  let con = Llvm.global_context () in
  Llvm_bitreader.parse_bitcode con buf

let remove_exercise mdl =
  begin match Llvm.lookup_function "_exercise" mdl with
    | None -> assert false
    | Some func ->
      Llvm.delete_function func
  end