/**
 * Stub implementations for LLVM target initializers not present in the
 * pre-built Windows LLVM SDK.  Inkwell unconditionally compiles references
 * to every target's init functions, so we provide no-op stubs for the
 * targets we don't need.  Only the native target (X86) is actually used.
 *
 * We also implement the LLVM_Initialize{All,Native}* wrapper functions that
 * are normally built as inline/static by llvm-sys's targetwrappers.c.
 * When dynamically linking against LLVM-C.dll, those wrappers are not
 * provided by the DLL, so we supply them here.
 */

/* ================================================================== */
/* Forward declarations for targets present in LLVM-C.dll             */
/* ================================================================== */

/* X86 */
void LLVMInitializeX86Target(void);
void LLVMInitializeX86TargetInfo(void);
void LLVMInitializeX86AsmPrinter(void);
void LLVMInitializeX86AsmParser(void);
void LLVMInitializeX86Disassembler(void);
void LLVMInitializeX86TargetMC(void);

/* AArch64 */
void LLVMInitializeAArch64Target(void);
void LLVMInitializeAArch64TargetInfo(void);
void LLVMInitializeAArch64AsmPrinter(void);
void LLVMInitializeAArch64AsmParser(void);
void LLVMInitializeAArch64Disassembler(void);
void LLVMInitializeAArch64TargetMC(void);

/* ARM */
void LLVMInitializeARMTarget(void);
void LLVMInitializeARMTargetInfo(void);
void LLVMInitializeARMAsmPrinter(void);
void LLVMInitializeARMAsmParser(void);
void LLVMInitializeARMDisassembler(void);
void LLVMInitializeARMTargetMC(void);

/* RISCV */
void LLVMInitializeRISCVTarget(void);
void LLVMInitializeRISCVTargetInfo(void);
void LLVMInitializeRISCVAsmPrinter(void);
void LLVMInitializeRISCVAsmParser(void);
void LLVMInitializeRISCVDisassembler(void);
void LLVMInitializeRISCVTargetMC(void);

/* WebAssembly */
void LLVMInitializeWebAssemblyTarget(void);
void LLVMInitializeWebAssemblyTargetInfo(void);
void LLVMInitializeWebAssemblyAsmPrinter(void);
void LLVMInitializeWebAssemblyAsmParser(void);
void LLVMInitializeWebAssemblyDisassembler(void);
void LLVMInitializeWebAssemblyTargetMC(void);

/* NVPTX */
void LLVMInitializeNVPTXTarget(void);
void LLVMInitializeNVPTXTargetInfo(void);
void LLVMInitializeNVPTXAsmPrinter(void);
void LLVMInitializeNVPTXTargetMC(void);

/* BPF */
void LLVMInitializeBPFTarget(void);
void LLVMInitializeBPFTargetInfo(void);
void LLVMInitializeBPFAsmPrinter(void);
void LLVMInitializeBPFAsmParser(void);
void LLVMInitializeBPFDisassembler(void);
void LLVMInitializeBPFTargetMC(void);

/* ================================================================== */
/* LLVM_Initialize{All,Native}* wrappers                              */
/* These correspond to the static inline functions in llvm-c/Target.h */
/* that llvm-sys's targetwrappers.c normally provides.                */
/* ================================================================== */

void LLVM_InitializeAllTargets(void) {
    LLVMInitializeX86Target();
    LLVMInitializeAArch64Target();
    LLVMInitializeARMTarget();
    LLVMInitializeRISCVTarget();
    LLVMInitializeWebAssemblyTarget();
    LLVMInitializeNVPTXTarget();
    LLVMInitializeBPFTarget();
}

void LLVM_InitializeAllTargetInfos(void) {
    LLVMInitializeX86TargetInfo();
    LLVMInitializeAArch64TargetInfo();
    LLVMInitializeARMTargetInfo();
    LLVMInitializeRISCVTargetInfo();
    LLVMInitializeWebAssemblyTargetInfo();
    LLVMInitializeNVPTXTargetInfo();
    LLVMInitializeBPFTargetInfo();
}

void LLVM_InitializeAllAsmPrinters(void) {
    LLVMInitializeX86AsmPrinter();
    LLVMInitializeAArch64AsmPrinter();
    LLVMInitializeARMAsmPrinter();
    LLVMInitializeRISCVAsmPrinter();
    LLVMInitializeWebAssemblyAsmPrinter();
    LLVMInitializeNVPTXAsmPrinter();
    LLVMInitializeBPFAsmPrinter();
}

void LLVM_InitializeAllAsmParsers(void) {
    LLVMInitializeX86AsmParser();
    LLVMInitializeAArch64AsmParser();
    LLVMInitializeARMAsmParser();
    LLVMInitializeRISCVAsmParser();
    LLVMInitializeWebAssemblyAsmParser();
    /* NVPTX has no AsmParser */
    LLVMInitializeBPFAsmParser();
}

void LLVM_InitializeAllDisassemblers(void) {
    LLVMInitializeX86Disassembler();
    LLVMInitializeAArch64Disassembler();
    LLVMInitializeARMDisassembler();
    LLVMInitializeRISCVDisassembler();
    LLVMInitializeWebAssemblyDisassembler();
    /* NVPTX has no Disassembler */
    LLVMInitializeBPFDisassembler();
}

void LLVM_InitializeAllTargetMCs(void) {
    LLVMInitializeX86TargetMC();
    LLVMInitializeAArch64TargetMC();
    LLVMInitializeARMTargetMC();
    LLVMInitializeRISCVTargetMC();
    LLVMInitializeWebAssemblyTargetMC();
    LLVMInitializeNVPTXTargetMC();
    LLVMInitializeBPFTargetMC();
}

/* Native = X86 on this platform */
int LLVM_InitializeNativeTarget(void) {
    LLVMInitializeX86Target();
    LLVMInitializeX86TargetInfo();
    LLVMInitializeX86TargetMC();
    return 0;
}

void LLVM_InitializeNativeAsmPrinter(void) {
    LLVMInitializeX86AsmPrinter();
}

void LLVM_InitializeNativeAsmParser(void) {
    LLVMInitializeX86AsmParser();
}

void LLVM_InitializeNativeDisassembler(void) {
    LLVMInitializeX86Disassembler();
}

/* ---------- Mips ---------- */
void LLVMInitializeMipsTarget(void) {}
void LLVMInitializeMipsTargetInfo(void) {}
void LLVMInitializeMipsAsmPrinter(void) {}
void LLVMInitializeMipsAsmParser(void) {}
void LLVMInitializeMipsDisassembler(void) {}
void LLVMInitializeMipsTargetMC(void) {}

/* ---------- Lanai ---------- */
void LLVMInitializeLanaiTarget(void) {}
void LLVMInitializeLanaiTargetInfo(void) {}
void LLVMInitializeLanaiAsmPrinter(void) {}
void LLVMInitializeLanaiAsmParser(void) {}
void LLVMInitializeLanaiDisassembler(void) {}
void LLVMInitializeLanaiTargetMC(void) {}

/* ---------- Sparc ---------- */
void LLVMInitializeSparcTarget(void) {}
void LLVMInitializeSparcTargetInfo(void) {}
void LLVMInitializeSparcAsmPrinter(void) {}
void LLVMInitializeSparcAsmParser(void) {}
void LLVMInitializeSparcDisassembler(void) {}
void LLVMInitializeSparcTargetMC(void) {}

/* ---------- MSP430 ---------- */
void LLVMInitializeMSP430Target(void) {}
void LLVMInitializeMSP430TargetInfo(void) {}
void LLVMInitializeMSP430AsmPrinter(void) {}
void LLVMInitializeMSP430AsmParser(void) {}
void LLVMInitializeMSP430Disassembler(void) {}
void LLVMInitializeMSP430TargetMC(void) {}

/* ---------- XCore ---------- */
void LLVMInitializeXCoreTarget(void) {}
void LLVMInitializeXCoreTargetInfo(void) {}
void LLVMInitializeXCoreAsmPrinter(void) {}
void LLVMInitializeXCoreAsmParser(void) {}
void LLVMInitializeXCoreDisassembler(void) {}
void LLVMInitializeXCoreTargetMC(void) {}

/* ---------- AMDGPU ---------- */
void LLVMInitializeAMDGPUTarget(void) {}
void LLVMInitializeAMDGPUTargetInfo(void) {}
void LLVMInitializeAMDGPUAsmPrinter(void) {}
void LLVMInitializeAMDGPUAsmParser(void) {}
void LLVMInitializeAMDGPUDisassembler(void) {}
void LLVMInitializeAMDGPUTargetMC(void) {}

/* ---------- Hexagon ---------- */
void LLVMInitializeHexagonTarget(void) {}
void LLVMInitializeHexagonTargetInfo(void) {}
void LLVMInitializeHexagonAsmPrinter(void) {}
void LLVMInitializeHexagonAsmParser(void) {}
void LLVMInitializeHexagonDisassembler(void) {}
void LLVMInitializeHexagonTargetMC(void) {}

/* ---------- PowerPC ---------- */
void LLVMInitializePowerPCTarget(void) {}
void LLVMInitializePowerPCTargetInfo(void) {}
void LLVMInitializePowerPCAsmPrinter(void) {}
void LLVMInitializePowerPCAsmParser(void) {}
void LLVMInitializePowerPCDisassembler(void) {}
void LLVMInitializePowerPCTargetMC(void) {}

/* ---------- SystemZ ---------- */
void LLVMInitializeSystemZTarget(void) {}
void LLVMInitializeSystemZTargetInfo(void) {}
void LLVMInitializeSystemZAsmPrinter(void) {}
void LLVMInitializeSystemZAsmParser(void) {}
void LLVMInitializeSystemZDisassembler(void) {}
void LLVMInitializeSystemZTargetMC(void) {}

/* ---------- LoongArch ---------- */
void LLVMInitializeLoongArchTarget(void) {}
void LLVMInitializeLoongArchTargetInfo(void) {}
void LLVMInitializeLoongArchAsmPrinter(void) {}
void LLVMInitializeLoongArchAsmParser(void) {}
void LLVMInitializeLoongArchDisassembler(void) {}
void LLVMInitializeLoongArchTargetMC(void) {}
