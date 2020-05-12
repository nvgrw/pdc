
#include <vector>

//#include "caml/callback.h"
//#include "caml/custom.h"
//#include "caml/fail.h"
//#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/misc.h"

#include "llvm-c/Types.h"

#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"

#define Builder_val(v) (*(LLVMBuilderRef *)(Data_custom_val(v)))

using namespace llvm;

extern "C" {

/* llcontext -> string -> string -> llvalue */
CAMLprim LLVMValueRef extra_difile(LLVMContextRef C, value Basename,
                                   value Directory) {
  LLVMContext &Context = *unwrap(C);
  DIFile *DIF =
      DIFile::get(Context, String_val(Basename), String_val(Directory));
  return wrap(MetadataAsValue::get(Context, DIF));
}

/* llmodule -> string -> llvalue -> int -> llvalue */
CAMLprim LLVMValueRef extra_dilocalvariable(LLVMModuleRef M, value Name,
                                            LLVMValueRef File, value Line) {
  Module &Module = *unwrap(M);
  LLVMContext &Context = Module.getContext();

  DIBuilder DIB(Module);
  DILocalVariable *DIL = DIB.createAutoVariable(
      DIB.createUnspecifiedParameter(), String_val(Name),
      (DIFile *)((MetadataAsValue *)unwrap(File))->getMetadata(),
      Unsigned_int_val(Line), nullptr);

  return wrap(MetadataAsValue::get(Context, DIL));
}

/* llvalue -> llvalue -> llbuilder -> llmodule -> llvalue */
CAMLprim LLVMValueRef extra_build_declare(LLVMValueRef Value,
                                          LLVMValueRef Location, value B,
                                          LLVMModuleRef M) {
  IRBuilder<> *Builder = unwrap(Builder_val(B));
  Module *Module = unwrap(M);
  LLVMContext &Context = Builder->getContext();

  Function *F = Intrinsic::getDeclaration(Module, Intrinsic::dbg_declare);

  llvm::Value *V =
      MetadataAsValue::get(Context, LocalAsMetadata::get(unwrap(Value)));
  llvm::Value *DIL = unwrap(Location);
  SmallVector<uint64_t, 8> Elements;
  llvm::Value *DIE =
      MetadataAsValue::get(Context, DIExpression::get(Context, Elements));

  std::vector<llvm::Value *> Args = {V, DIL, DIE};
  return wrap(Builder->CreateCall(F, Args));
}
}
