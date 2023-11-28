#include "caml/memory.h"
#include "llvm-c/Transforms/PassBuilder.h"

#define TargetMachine_val(v) (*(LLVMTargetMachineRef *)(Data_custom_val(v)))

void run_passes(value module, value passes, value target_machine) {
  LLVMRunPasses((LLVMModuleRef)module, String_val(passes),
                TargetMachine_val(target_machine),
                LLVMCreatePassBuilderOptions());
}
