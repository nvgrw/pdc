#include "llvm/Transforms/IPO/PassManagerBuilder.h"

#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/LegacyPassManager.h"

#include "llvm/IR/IRBuilder.h"

using namespace llvm;

namespace {

struct RandomProvider : public FunctionPass {
  static char ID;
  FunctionCallee Function_arc4random_uniform;

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
  Function_arc4random_uniform =
      M.getOrInsertFunction("arc4random_uniform", Type::getInt32Ty(Context),
                            Type::getInt32Ty(Context));
  return true;
}

bool RandomProvider::runOnFunction(Function &F) {
  bool MadeChange = false;

  for (BasicBlock &BB : F)
    for (BasicBlock::iterator II = BB.begin(), E = BB.end(); II != E;) {
      IntrinsicInst *CI = dyn_cast<IntrinsicInst>(II++);
      if (!CI)
        continue;

      IRBuilder<> Builder(CI);
      auto *UpperBound = cast<ConstantInt>(CI->getArgOperand(0));
      assert(UpperBound->getBitWidth() -
                     UpperBound->getValue().countLeadingZeros() <=
                 32 &&
             "arc4random Random Provider only supports 32-bit values!");
      Value *RandomValue = Builder.CreateIntCast(
          Builder.CreateCall(
              Function_arc4random_uniform,
              {Builder.CreateIntCast(UpperBound, Builder.getInt32Ty(), false)}),
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