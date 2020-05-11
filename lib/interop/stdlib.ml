
let parse_stdlib_mdl () =
  let buf = Llvm.MemoryBuffer.of_string [%blob "../../stdlib/build/llvm.bc"] in
  let con = Llvm.global_context () in
  Llvm_bitreader.parse_bitcode con buf

let remove_exercise mdl =
  Llvm.delete_function (Core.Option.value_exn (Llvm.lookup_function "_exercise" mdl))