#include "llvm/Transforms/IPO/PassManagerBuilder.h"

#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/LegacyPassManager.h"

#include "llvm/IR/IRBuilder.h"

using namespace llvm;

namespace {

struct RandomProvider : public FunctionPass {
  static char ID;
  bool IsSeeded = false;
  FunctionCallee Function_srand;
  FunctionCallee Function_rand;
  FunctionCallee Function_time;

  RandomProvider() : FunctionPass(ID) {}
  StringRef getPassName() const override;

  bool doInitialization(Module &M) override;
  bool runOnFunction(Function &F) override;
};
} // anonymous namespace

char RandomProvider::ID = 0;

StringRef RandomProvider::getPassName() const {
  return "arc4random Random Provider";
}

bool RandomProvider::doInitialization(Module &M) {
  LLVMContext &Context = M.getContext();
  Function_srand = M.getOrInsertFunction("srand", Type::getVoidTy(Context),
                                         Type::getInt32Ty(Context));
  Function_rand = M.getOrInsertFunction("rand", Type::getInt32Ty(Context));
  Function_time = M.getOrInsertFunction("time", Type::getInt32Ty(Context),
                                        Type::getInt64PtrTy(Context));
  return true;
}

bool RandomProvider::runOnFunction(Function &F) {
  bool MadeChange = false;

  for (BasicBlock &BB : F)
    for (BasicBlock::iterator II = BB.begin(), E = BB.end(); II != E;) {
      IntrinsicInst *CI = dyn_cast<IntrinsicInst>(II++);
      if (!CI || CI->getIntrinsicID() != Intrinsic::rand_uniform)
        continue;

      IRBuilder<> Builder(CI);
      auto *UpperBound = cast<ConstantInt>(CI->getArgOperand(0));
      assert(UpperBound->getBitWidth() -
                     UpperBound->getValue().countLeadingZeros() <=
                 32 &&
             "srand Random Provider only supports 32-bit values!");

      if (!IsSeeded) {
        Builder.CreateCall(
            Function_srand,
            {Builder.CreateCall(Function_time,
                                {Constant::getIntegerValue(
                                    Type::getInt32PtrTy(CI->getContext()),
                                    APInt(32, 0, false))})});
        IsSeeded = true;
      }

      Value *RandomValue = Builder.CreateIntCast(
          Builder.CreateURem(Builder.CreateCall(Function_rand), UpperBound),
          Builder.getInt64Ty(), false);

      CI->replaceAllUsesWith(RandomValue);
      CI->eraseFromParent();

      MadeChange = true;
    }

  return MadeChange;
}

static llvm::RegisterStandardPasses
    Y(llvm::PassManagerBuilder::EP_RandomProvider,
      [](const llvm::PassManagerBuilder &Builder,
         llvm::legacy::PassManagerBase &PM) { PM.add(new RandomProvider()); });
