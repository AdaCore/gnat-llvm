#include <string.h>

#include "llvm-c/Types.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/AArch64TargetParser.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/Instrumentation/AddressSanitizer.h"
#include "llvm/Transforms/Instrumentation/SanitizerCoverage.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/Scalar/LoopPassManager.h"
#include "llvm/Transforms/Scalar/LoopRotation.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm-c/Core.h"

#if LLVM_VERSION_MAJOR < 15
#include "llvm/Support/TargetRegistry.h"
#else
#include "llvm/MC/TargetRegistry.h"
#endif

#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticIDs.h"
#include "clang/Basic/DiagnosticOptions.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TargetOptions.h"

using namespace llvm;
using namespace llvm::sys;

using namespace clang;

extern "C"
Instruction *
Get_Latest_Instruction (IRBuilder<> *bld)
{
  return dyn_cast<Instruction>(&*--bld->GetInsertPoint ());
}

extern "C"
void
Add_Debug_Flags (Module *TheModule)
{
  TheModule->addModuleFlag (Module::Warning, "Debug Info Version",
			    DEBUG_METADATA_VERSION);
  TheModule->addModuleFlag (Module::Warning, "Dwarf Version", 4);
}

extern "C"
MDBuilder *
Create_MDBuilder_In_Context (LLVMContext &Ctx)
{
  return new MDBuilder (Ctx);
}

extern "C"
MDNode *
Create_TBAA_Root (MDBuilder *MDHelper)
{
  return MDHelper->createTBAARoot ("Ada Root");
}

extern "C"
void
Add_Cold_Attribute (Function *fn)
{
  fn->addFnAttr (Attribute::Cold);
}

extern "C"
void
Add_Dereferenceable_Attribute (Function *fn, unsigned idx,
			       unsigned long long Bytes)
{
  fn->addDereferenceableParamAttr (idx, Bytes);
}

extern "C"
void
Add_Ret_Dereferenceable_Attribute (Function *fn, unsigned long long Bytes)
{
  /* There doesn't appear to be a way to do this in LLVM 14, so skip for now.
     fn->addDereferenceableRetAttr (Bytes); */
}

extern "C"
void
Add_Dereferenceable_Or_Null_Attribute (Function *fn, unsigned idx,
				       unsigned long long Bytes)
{
   fn->addDereferenceableOrNullParamAttr (idx, Bytes);
}

extern "C"
void
Add_Ret_Dereferenceable_Or_Null_Attribute (Function *fn, 
					   unsigned long long Bytes)
{
  /* There doesn't appear to be a way to do this in LLVM 14, so skip for now.
     fn->addDereferenceableOrNullRetAttr (Bytes); */
}

extern "C"
void
Add_Inline_Always_Attribute (Function *fn)
{
  fn->addFnAttr (Attribute::AlwaysInline);
}

extern "C"
void
Add_Inline_Hint_Attribute (Function *fn)
{
  fn->addFnAttr (Attribute::InlineHint);
}

extern "C"
void
Add_Inline_No_Attribute (Function *fn)
{
  fn->addFnAttr (Attribute::NoInline);
}

extern "C"
void
Add_Fn_Readonly_Attribute (Function *fn)
{
  fn->setOnlyReadsMemory ();
}

extern "C"
void
Add_Named_Attribute (Function *fn, const char *name, const char *val,
		     LLVMContext &Ctx)
{
    fn->addFnAttr (Attribute::get (Ctx, StringRef (name, strlen (name)),
				   StringRef (val, strlen (val))));
}

extern "C"
void
Add_Nest_Attribute (Value *v, unsigned idx)
{
  if (Function *fn = dyn_cast<Function>(v))
    fn->addParamAttr (idx, Attribute::Nest);
  else if (CallInst *ci = dyn_cast<CallInst>(v))
    ci->addParamAttr (idx, Attribute::Nest);
  else if (InvokeInst *ii = dyn_cast<InvokeInst>(v))
    ii->addParamAttr (idx, Attribute::Nest);
}

extern "C"
void
Add_Noalias_Attribute (Function *fn, unsigned idx)
{
  fn->addParamAttr (idx, Attribute::NoAlias);
}

extern "C"
void
Add_Ret_Noalias_Attribute (Function *fn)
{
  fn->addRetAttr (Attribute::NoAlias);
}

extern "C"
void
Add_Nocapture_Attribute (Function *fn, unsigned idx)
{
  fn->addParamAttr (idx, Attribute::NoCapture);
}

extern "C"
void
Add_Non_Null_Attribute (Function *fn, unsigned idx)
{
  fn->addParamAttr (idx, Attribute::NonNull);
}

extern "C"
void
Add_Ret_Non_Null_Attribute (Function *fn, unsigned idx)
{
  fn->addRetAttr (Attribute::NonNull);
}

extern "C"
void
Add_Readonly_Attribute (Function *fn, unsigned idx)
{
  fn->addParamAttr (idx, Attribute::ReadOnly);
}

extern "C"
void
Add_Writeonly_Attribute (Function *fn, unsigned idx)
{
  fn->addParamAttr (idx, Attribute::WriteOnly);
}

extern "C"
void
Add_Opt_For_Fuzzing_Attribute (Function *fn)
{
  fn->addFnAttr(Attribute::OptForFuzzing);
}

extern "C"
void
Add_Sanitize_Address_Attribute (Function *fn)
{
  fn->addFnAttr(Attribute::SanitizeAddress);
}

extern "C"
void
Add_No_Implicit_Float_Attribute (Function *fn)
{
  fn->addFnAttr(Attribute::NoImplicitFloat);
}

extern "C"
bool
Has_Inline_Attribute (Function *fn)
{
  return fn->hasFnAttribute (Attribute::InlineHint);
}

extern "C"
bool
Has_Inline_Always_Attribute (Function *fn)
{
  return fn->hasFnAttribute (Attribute::AlwaysInline);
}

extern "C"
bool
Has_Nest_Attribute (Function *fn, unsigned idx)
{
  return fn->hasParamAttribute (idx, Attribute::Nest);
}

extern "C"
bool
Call_Param_Has_Nest (CallBase *CI, unsigned idx)
{
  return CI->getAttributes ().hasParamAttr (idx, Attribute::Nest);
}

extern "C"
MDNode *
Create_TBAA_Scalar_Type_Node (LLVMContext &ctx, MDBuilder *MDHelper,
			      const char *name, uint64_t size, MDNode *parent)
{
  Type *Int64 = Type::getInt64Ty (ctx);
  auto MDname = MDHelper->createString (name);
  auto MDsize = MDHelper->createConstant (ConstantInt::get (Int64, size));
  return MDNode::get (ctx, {parent, MDsize, MDname});
}

extern "C"
MDNode *
Create_TBAA_Struct_Type_Node (LLVMContext &ctx, MDBuilder *MDHelper,
			      const char *name, uint64_t size, int num_fields,
			      MDNode *parent, MDNode *fields[],
			      uint64_t offsets[], uint64_t sizes[])
{
  Type *Int64 = Type::getInt64Ty (ctx);
  SmallVector<Metadata *, 8> Ops (num_fields * 3 + 3);
  Ops [0] = parent;
  Ops [1] = MDHelper->createConstant (ConstantInt::get (Int64, size));
  Ops [2] = MDHelper->createString (name);
  for (unsigned i = 0; i < num_fields; i++)
    {
      Ops[3 + i * 3] = fields[i];
      Ops[3 + i * 3 + 1]
	  = MDHelper->createConstant (ConstantInt::get (Int64, offsets[i]));
      Ops[3 + i * 3 + 2]
	  = MDHelper->createConstant (ConstantInt::get (Int64, sizes[i]));
    }

  return MDNode:: get (ctx, Ops);
}

extern "C"
MDNode *
Create_TBAA_Struct_Node (LLVMContext &ctx, MDBuilder *MDHelper,
			 int num_fields, MDNode *types[], uint64_t offsets[],
			 uint64_t sizes[])
{
  Type *Int64 = Type::getInt64Ty (ctx);
  SmallVector<Metadata *, 8> Ops (num_fields * 3);
  for (unsigned i = 0; i < num_fields; i++)
    {
      Ops[i * 3]
	  = MDHelper->createConstant (ConstantInt::get (Int64, offsets[i]));
      Ops[i * 3 + 1]
	  = MDHelper->createConstant (ConstantInt::get (Int64, sizes[i]));
      Ops[i * 3 + 2] = types[i];
    }

  return MDNode:: get (ctx, Ops);
}

extern "C"
MDNode *
Create_TBAA_Access_Tag (MDBuilder *MDHelper, MDNode *BaseType,
			MDNode *AccessType, uint64_t offset, uint64_t size)
{
  return MDHelper->createTBAAAccessTag (BaseType, AccessType, offset, size,
					false);
}

extern "C"
void
Set_NUW (Instruction *inst)
{
  inst->setHasNoUnsignedWrap ();
}

extern "C"
void
Set_NSW (Instruction *inst)
{
  inst->setHasNoSignedWrap ();
}

extern "C"
bool
Has_NSW (Instruction *inst)
{
  return inst->hasNoSignedWrap ();
}

extern "C"
void
Add_TBAA_Access (Instruction *inst, MDNode *md)
{
  inst->setMetadata (LLVMContext::MD_tbaa, md);
}

extern "C"
void
Set_DSO_Local (GlobalVariable *GV)
{
  GV->setDSOLocal (true);
}

/* Return nonnull if this value is a constant data.  */

extern "C"
Value *
Is_Constant_Data (Value *v)
{
  return dyn_cast<ConstantData>(v);
}

/* Say whether this struct type has a name.  */

extern "C"
bool
Struct_Has_Name (StructType *t)
{
  return t->hasName ();
}

/*  Say whether this value has a name */

extern "C"
bool
Value_Has_Name (Value *v)
{
    return v->getName ().size () != 0;
}

/* The LLVM C interface only provide single-index forms of extractvalue
   and insertvalue, so provide the multi-index forms here.  */

extern "C"
Value *
Build_Extract_Value_C (IRBuilder<> *bld, Value *aggr,
		       unsigned *IdxList, unsigned NumIdx, char *name)
{
  return bld->CreateExtractValue (aggr, {IdxList, NumIdx}, name);
}

extern "C"
Value *
Build_Insert_Value_C (IRBuilder<> *bld, Value *aggr, Value *elt,
		     unsigned *IdxList, unsigned NumIdx, char *name)
{
  return bld->CreateInsertValue (aggr, elt, {IdxList, NumIdx}, name);
}

/* The LLVM C interface only provides a subset of the arguments for building
   memory copy/move/set, so we provide the full interface here.  */

extern "C"
Value *
Build_MemCpy (IRBuilder<> *bld, Value *Dst, unsigned DstAlign, Value *Src,
	      unsigned SrcAlign, Value *Size, bool isVolatile, MDNode *TBAATag,
	      MDNode *TBAAStructTag, MDNode *ScopeTag, MDNode *NoAliasTag)
{
  return bld->CreateMemCpy (Dst, MaybeAlign (DstAlign), Src,
			    MaybeAlign(SrcAlign), Size, isVolatile, TBAATag,
			    TBAAStructTag, ScopeTag, NoAliasTag);
}

extern "C"
Value *
Build_MemMove (IRBuilder<> *bld, Value *Dst, unsigned DstAlign, Value *Src,
	       unsigned SrcAlign, Value *Size, bool isVolatile,
	       MDNode *TBAATag, MDNode *ScopeTag, MDNode *NoAliasTag)
{
  return bld->CreateMemMove (Dst, MaybeAlign (DstAlign), Src,
			     MaybeAlign (SrcAlign), Size, isVolatile, TBAATag,
			     ScopeTag, NoAliasTag);
}

extern "C"
Value *
Build_MemSet (IRBuilder<> *bld, Value *Ptr, Value *Val, Value *Size,
	      unsigned align, bool isVolatile, MDNode *TBAATag,
	      MDNode *ScopeTag, MDNode *NoAliasTag)
{
  return bld->CreateMemSet (Ptr, Val, Size, MaybeAlign (align), isVolatile,
			    TBAATag, ScopeTag, NoAliasTag);
}

extern "C"
CallInst *
Create_Lifetime_Start (IRBuilder<> *bld, Value *Ptr, ConstantInt *Size)
{
  return bld->CreateLifetimeStart (Ptr, Size);
}

extern "C"
CallInst *
Create_Lifetime_End (IRBuilder<> *bld, Value *Ptr, ConstantInt *Size)
{
  return bld->CreateLifetimeEnd (Ptr, Size);
}

extern "C"
CallInst *
Create_Invariant_Start (IRBuilder<> *bld, Value *Ptr, ConstantInt *Size)
{
  return bld->CreateInvariantStart (Ptr, Size);
}

extern "C"
unsigned char
Does_Not_Throw (Function *fn)
{
  return fn->doesNotThrow () ? 1 : 0;
}

extern "C"
void
Set_Does_Not_Throw (Function *fn)
{
  return fn->setDoesNotThrow ();
}

extern "C"
unsigned char
Does_Not_Return (Function *fn)
{
  return fn->doesNotReturn () ? 1 : 0;
}

extern "C"
void
Set_Does_Not_Return (Function *fn)
{
  return fn->setDoesNotReturn ();
}

/* The LLVM C procedure Set_Volatile only works for loads and stores, not
   Atomic instructions.  */

extern "C"
void
Set_Volatile_For_Atomic (Instruction *inst)
{
  if (AtomicRMWInst *ARW = dyn_cast<AtomicRMWInst>(inst))
    ARW->setVolatile (true);
  else
    cast<AtomicCmpXchgInst>(inst)->setVolatile (true);
}

extern "C"
void
Set_Weak_For_Atomic_Xchg (AtomicCmpXchgInst *inst)
{
  inst->setWeak (true);
}

extern "C"
void
Add_Function_To_Module (Function *f, Module *m, bool allowDeduplication)
{
  // Check if the function already exists in the module. We only add imported
  // functions to the module after we've processed the entire GNAT tree, and we
  // deduplicate based on GNAT node IDs. However, builtin functions such as
  // __gnat_last_chance_handler are processed separately (because there is no
  // associated node in the GNAT tree) and added eagerly to the module.
  //
  // If a module uses a builtin function implicitly and also calls it
  // explicitly, we don't want two LLVM values representing the same function.
  // In this case, we replace all uses of one with the other (unless they're
  // identical), which is safe because the LLVM value is just a declaration for
  // a function to be imported.

  auto existingFunction = m->getFunction(f->getName());
  if (existingFunction == f)
    return;

  if (existingFunction && !allowDeduplication) {
    assert(f->isDeclaration() && existingFunction->isDeclaration() &&
           f->getType() == existingFunction->getType());
    f->replaceAllUsesWith(existingFunction);
    delete f;
  } else {
    m->getFunctionList().push_back(f);
  }
}

extern "C"
void
Dump_Metadata (MDNode *MD)
{
  MD->print (errs ());
}

extern "C"
unsigned
Get_Metadata_Num_Operands (MDNode *MD)
{
  return MD->getNumOperands ();
}

extern "C"
uint64_t
Get_Metadata_Operand_Constant_Value (MDNode *MD, unsigned i)
{
  return mdconst::extract<ConstantInt> (MD->getOperand (i))->getZExtValue ();
}

extern "C"
MDNode *
Get_Metadata_Operand (MDNode *MD, unsigned i)
{
  return dyn_cast<MDNode>(MD->getOperand (i));
}

extern "C"
void
Initialize_LLVM (void)
{
  // Initialize the target registry etc.  These functions appear to be
  // in LLVM.Target, but they reference static inline function, so they
  // can only be used from C, not Ada.

  InitializeAllTargetInfos ();
  InitializeAllTargets ();
  InitializeAllTargetMCs ();
  InitializeAllAsmParsers ();
  InitializeAllAsmPrinters ();
}

// Description of C type properties to pass to Ada. Unless otherwise noted,
// sizes are in bits.
struct Target_C_Type_Info {
  unsigned PointerSize;
  unsigned CharSize;
  unsigned WCharTSize;
  unsigned ShortSize;
  unsigned IntSize;
  unsigned LongSize;
  unsigned LongLongSize;
  unsigned LongLongLongSize;
  unsigned LongDoubleSemanticSize; // 0 if the target doesn't have long double
  unsigned LongDoubleStorageSize;  // 0 if the target doesn't have long double
  unsigned LongDoubleAlignment;    // 0 if the target doesn't have long double
  unsigned MaximumAlignmentBytes;
  unsigned RegisterSize;
};

extern "C"
void
Get_Target_C_Types (const char *Triple, const char *CPU, const char *ABI,
                    const char *Features, Target_C_Type_Info *Result,
                    unsigned char *success)
{
  *Result = {};
  *success = 0;

  auto Options = std::make_shared<clang::TargetOptions>();
  Options->Triple = Triple;

  std::string CPUString = CPU;
  if (CPUString != "generic") // GNAT-LLVM's default CPU, unknown to LLVM
    Options->CPU = CPU;

  std::string FeatureString = Features;
  if (!FeatureString.empty()) {
    SmallVector<StringRef, 16> FeatureVector;
    SplitString(FeatureString, FeatureVector, ",");
    for (const auto F : FeatureVector)
      Options->Features.push_back(F.str());
  }

  // The Clang API requires us to provide a handler for diagnostic messages
  // emitted during the operation.
  //
  // ??? Use a real diagnostics consumer if we ever get an error from Clang.
  IntrusiveRefCntPtr<DiagnosticIDs> DiagID(new DiagnosticIDs());
  IntrusiveRefCntPtr<DiagnosticOptions> DiagOpts = new DiagnosticOptions();
  DiagnosticsEngine Diags(DiagID, &*DiagOpts, new IgnoringDiagConsumer());

  // Finally, we can create the TargetInfo structure.
  std::unique_ptr<TargetInfo> Info(
    TargetInfo::CreateTargetInfo(Diags, Options));

  std::string ABIString = ABI;
  if (!ABIString.empty())
    Info->setABI(ABIString);

  if (Info == nullptr)
    return;

  Result->PointerSize = Info->getPointerWidth(LangAS::Default);
  Result->CharSize = Info->getCharWidth();
  Result->WCharTSize = Info->getWCharWidth();
  Result->ShortSize = Info->getShortWidth();
  Result->IntSize = Info->getIntWidth();
  Result->LongSize = Info->getLongWidth();
  Result->LongLongSize = Info->getLongLongWidth();
  Result->LongLongLongSize = Info->hasInt128Type() ? 128 : 64;
  if (Info->hasLongDoubleType()) {
    Result->LongDoubleSemanticSize =
      APFloat::semanticsSizeInBits(Info->getLongDoubleFormat());
    Result->LongDoubleStorageSize = Info->getLongDoubleWidth();
    Result->LongDoubleAlignment = Info->getLongDoubleAlign();
  }
  Result->MaximumAlignmentBytes = Info->getSuitableAlign() / 8;
  Result->RegisterSize = Info->getRegisterWidth();
  *success = 1;
}

/* This is a dummy optimization "pass" that serves just to obtain loop
   information when generating C.

   For now, we don't actually do anything except collect information to look
   at in the debugger.  */

namespace llvm
{
  class Loop;

  struct OurLoopPass : PassInfoMixin<OurLoopPass>
  {
  public:
    PreservedAnalyses run (Loop &L, LoopAnalysisManager &AM,
			   LoopStandardAnalysisResults &AR, LPMUpdater &U);
    static bool isRequired ()
    {
      return true;
    }
  };
}

PreservedAnalyses
OurLoopPass::run (Loop &L, LoopAnalysisManager &LAM,
		  LoopStandardAnalysisResults &AR, LPMUpdater &U)
{

  auto pre = L.getLoopPreheader ();
  auto header = L.getHeader ();
  auto latch = L.getLoopLatch ();
  auto cmp = L.getLatchCmpInst ();
  auto issimple = L.isLoopSimplifyForm ();
  return PreservedAnalyses::all ();
}

extern "C"
LLVMBool
LLVM_Optimize_Module (Module *M, TargetMachine *TM, int CodeOptLevel,
                      int SizeOptLevel, bool NeedLoopInfo, bool NoUnrollLoops,
                      bool NoLoopVectorization, bool NoSLPVectorization,
                      bool MergeFunctions, bool PrepareForThinLTO,
                      bool PrepareForLTO, bool RerollLoops, bool EnableFuzzer,
                      bool EnableAddressSanitizer, const char *SanCovAllowList,
                      const char *SanCovIgnoreList, const char *PassPluginName,
                      char **ErrorMessage) {
  // This code is derived from EmitAssemblyWithNewPassManager in clang

  std::optional<PGOOptions> PGOOpt;
  PipelineTuningOptions PTO;
  PassInstrumentationCallbacks PIC;
  Triple TargetTriple (M->getTargetTriple ());
  OptimizationLevel Level
    = (CodeOptLevel == 1 ? OptimizationLevel::O1
       : CodeOptLevel == 2 ? OptimizationLevel::O2
       : CodeOptLevel == 3 ? OptimizationLevel::O3
       : OptimizationLevel::O0);
  PTO.LoopUnrolling = !NoUnrollLoops;
  PTO.LoopInterleaving = !NoUnrollLoops;
  PTO.LoopVectorization = !NoLoopVectorization;
  PTO.SLPVectorization = !NoSLPVectorization;
  PTO.MergeFunctions = MergeFunctions;

  LoopAnalysisManager LAM;
  FunctionAnalysisManager FAM;
  CGSCCAnalysisManager CGAM;
  ModuleAnalysisManager MAM;

  PassBuilder PB (TM, PTO, PGOOpt, &PIC);

  if (PassPluginName != nullptr)
    {
      auto Plugin = PassPlugin::Load (PassPluginName);

      if (auto Err = Plugin.takeError())
        {
          handleAllErrors(std::move(Err), [&](const StringError &Err) {
            if (ErrorMessage != nullptr)
              *ErrorMessage = strdup (Err.getMessage().c_str());
          });

          return 1;
        }

      Plugin->registerPassBuilderCallbacks(PB);
    }

  FAM.registerPass ([&] { return PB.buildDefaultAAPipeline (); });

  // Register the target library analysis directly and give it a customized
  // preset TLI.
  TargetLibraryInfoImpl *TLII = new TargetLibraryInfoImpl(TargetTriple);
  FAM.registerPass ([&] { return TargetLibraryAnalysis (*TLII); });

  // Register all the basic analyses with the managers.

  PB.registerModuleAnalyses (MAM);
  PB.registerCGSCCAnalyses (CGAM);
  PB.registerFunctionAnalyses (FAM);
  PB.registerLoopAnalyses (LAM);
  PB.crossRegisterProxies (LAM, FAM, CGAM, MAM);

  // Register additional passes for the sanitizers if applicable. This code is
  // inspired by addSanitizers in LLVM's clang/lib/CodeGen/BackendUtil.cpp.
  PB.registerOptimizerLastEPCallback(
      [&](ModulePassManager &MPM, OptimizationLevel Level) {
        if (EnableFuzzer) {
          // Configure sanitizer coverage according to what Clang does in
          // clang/lib/Driver/SanitizerArgs.cpp when the fuzzer is enabled.
          SanitizerCoverageOptions CoverageOpts;
          CoverageOpts.CoverageType = SanitizerCoverageOptions::SCK_Edge;
          CoverageOpts.Inline8bitCounters = true;
          CoverageOpts.IndirectCalls = true;
          CoverageOpts.TraceCmp = true;
          CoverageOpts.PCTable = true;
          if (TargetTriple.isOSLinux())
            CoverageOpts.StackDepth = true;

          std::vector<std::string> AllowListFiles;
          if (SanCovAllowList != nullptr)
            AllowListFiles.push_back(SanCovAllowList);

          std::vector<std::string> IgnoreListFiles;
          if (SanCovIgnoreList != nullptr)
            IgnoreListFiles.push_back(SanCovIgnoreList);

          MPM.addPass(SanitizerCoveragePass(CoverageOpts, AllowListFiles,
                                            IgnoreListFiles));
        }

        if (EnableAddressSanitizer)
          MPM.addPass(AddressSanitizerPass(AddressSanitizerOptions()));
      });

  ModulePassManager MPM;
  if (CodeOptLevel == 0)
    {
      MPM = PB.buildO0DefaultPipeline (Level,
				       PrepareForLTO || PrepareForThinLTO);
      if (NeedLoopInfo)
	MPM.addPass (createModuleToFunctionPassAdaptor
		     (createFunctionToLoopPassAdaptor (LoopRotatePass ())));
    }
  else if (PrepareForThinLTO)
    MPM = PB.buildThinLTOPreLinkDefaultPipeline (Level);
  else if (PrepareForLTO)
    MPM = PB.buildLTOPreLinkDefaultPipeline (Level);
  else
    MPM = PB.buildPerModuleDefaultPipeline (Level);

  if (NeedLoopInfo)
    MPM.addPass (createModuleToFunctionPassAdaptor
		 (createFunctionToLoopPassAdaptor (OurLoopPass ())));
  MPM.run (*M, MAM);
  return 0;
}

extern "C"
Value *
Get_Float_From_Words_And_Exp (LLVMContext *Context, Type *T, int Exp,
			      unsigned NumWords, const uint64_t Words[])
{
  auto LongInt = APInt (NumWords * 64, {Words, NumWords});
  auto Initial = APFloat (T->getFltSemantics (),
			  APInt::getNullValue (T->getPrimitiveSizeInBits ()));
  Initial.convertFromAPInt (LongInt, false, APFloat::rmTowardZero);
  auto Result = scalbn (Initial, Exp, APFloat::rmTowardZero);
  return ConstantFP::get (*Context, Result);
}

extern "C"
Value *
Pred_FP (LLVMContext *Context, Type *T, Value *Val)
{
  // We want to compute the predecessor of Val, but the "next" function
  // can return unnormalized, so we have to multiply by one (adding zero
  // doesn't do it).
  auto apf = dyn_cast<ConstantFP>(Val)->getValueAPF ();
  auto one = APFloat (T->getFltSemantics (), 1);
  apf.next (true);
  apf.multiply (one, APFloat::rmTowardZero);
  return ConstantFP:: get (*Context, apf);
}

extern "C"
int
Convert_FP_To_String (Value *V, char *Buf)
{
  const APFloat &APF = dyn_cast<ConstantFP>(V)->getValueAPF ();

  if (&APF.getSemantics () == &APFloat::IEEEsingle ()
      || &APF.getSemantics () == &APFloat::IEEEdouble ())
    {
      if (!APF.isInfinity () && !APF.isNaN ())
	{
	  SmallString<128> StrVal;
	  APF.toString (StrVal, 0, 0, false);

	  if (&APF.getSemantics () == &APFloat::IEEEsingle ())
	    StrVal += "f";

	  strcpy (Buf, StrVal.c_str ());
	  return strlen (Buf);
	}

      // Output special values in hexadecimal format
      std::string S =
	("0x" + utohexstr (APF.bitcastToAPInt ().getZExtValue (),
			   /*Lower=*/true)
	 + "p0");

      std::strcpy (Buf, S.c_str ());
      return S.length ();
    }

  return strlen (strcpy (Buf, "<unsupported floating point type>"));
}

extern "C"
Value *
Get_Infinity (Type *T)
{
  return ConstantFP::getInfinity (T, false);
}

extern "C"
bool
Equals_Int (ConstantInt *v, uint64_t val)
{
  return v->equalsInt (val);
}

/* Return whether V1 and V2 have the same constant values, interpreted as
   signed integers. If the sizes differ, we have to convert to the
   widest size.  */

extern "C"
bool
Equal_Constants (ConstantInt *v1, ConstantInt *v2)
{
  APInt i1 = v1->getValue (), i2 = v2->getValue ();

  if (i1.getBitWidth () > i2.getBitWidth ())
    i2 = i2.sext (i1.getBitWidth ());
  else if (i2.getBitWidth () > i1.getBitWidth ())
    i1 = i1.sext (i2.getBitWidth ());
  return i1 == i2;
}

extern "C"
bool
Get_GEP_Constant_Offset (Value *GEP, DataLayout &dl, uint64_t *result)
{
  auto Offset =
      APInt(dl.getIndexSizeInBits(GEP->getType()->getPointerAddressSpace()), 0);
  auto GEPO = dyn_cast<GEPOperator>(GEP);

  if (!GEPO || !GEPO->accumulateConstantOffset (dl, Offset)
      || !Offset.isIntN (64))
    return false;

  *result = Offset.getZExtValue ();
  return true;
}

extern "C"
int64_t
Get_Element_Offset (DataLayout &DL, StructType *ST, unsigned idx)
{
  const StructLayout *SL = DL.getStructLayout (ST);
  return SL->getElementOffset (idx);
}

extern "C"
unsigned
Get_Num_CDA_Elements (ConstantDataArray *CA)
{
  return CA->getNumElements ();
}

extern "C"
bool
Is_C_String (ConstantDataSequential *CDS)
{
  return CDS->isCString ();
}

/* There are two LLVM "opcodes": the real LLVM opcode, which is used
   throughout the LLVM C++ interface, and a "stable" version of the
   opcodes, that's used in the C interface.  We need to map between them,
   but the only function to do so in LLVM is static (in Core.cpp), so we
   duplicate that small function here.  */

static int map_from_llvmopcode (LLVMOpcode code)
{
  switch (code)
    {
#define HANDLE_INST(num, opc, clas) case LLVM##opc: return num;
#include "llvm/IR/Instruction.def"
#undef HANDLE_INST
    }

  llvm_unreachable ("Unhandled Opcode.");
}

extern "C"
const char *
Get_Opcode_Name (LLVMOpcode opc)
{
  return Instruction::getOpcodeName (map_from_llvmopcode (opc));
}

extern "C"
Type *
Get_Load_Store_Type (Value *I)
{
  return getLoadStoreType (I);
}

extern "C"
Type *
Get_Source_Element_Type (Value *I)
{
  auto *GEPO = cast<GEPOperator>(I);
  return GEPO->getSourceElementType ();
}
    
extern "C"
Type *
Get_Function_Type (Function *F)
{
  return F->getFunctionType ();
}

extern "C"
BasicBlock *Get_Unique_Predecessor (BasicBlock *bb)
{
  return bb->getUniquePredecessor ();
}

extern "C"
void
Invert_Predicate (CmpInst *c)
{
  c->setPredicate (c->getInversePredicate ());
}

extern "C"
void
Swap_Successors (BranchInst *c)
{
  c->swapSuccessors ();
}

extern "C"
void
Replace_Inst_With_Inst (Instruction *from, Instruction *to)
{
  ReplaceInstWithInst (from, to);
}

extern "C"
BinaryOperator *
Create_And (Value *Op1, Value *Op2)
{
  return BinaryOperator::CreateAnd (Op1, Op2);
}

extern "C"
BinaryOperator *
Create_Or (Value *Op1, Value *Op2)
{
  return BinaryOperator::CreateOr (Op1, Op2);
}

extern "C"
CallInst *
Create_Call_2 (Function *Fn, Value *op1, Value *op2)
{
  return CallInst::Create (Fn, {op1, op2});
}

extern "C"
ReturnInst *
Create_Return (LLVMContext &C, Value *retVal)
{
  return ReturnInst::Create (C, retVal);
}

extern "C"
BranchInst *
Create_Br (BasicBlock *dest)
{
  return BranchInst::Create (dest);
}

extern "C"
void
Insert_At_Block_End (Instruction *I, BasicBlock *BB, Instruction *From)
{
  I->insertInto (BB, BB->end());
  I->setDebugLoc (From->getDebugLoc ());
}

extern "C"
AllocaInst *
Insert_Alloca_Before (Type *Ty, Instruction *Before)
{
 auto Inst = new AllocaInst (Ty, 0, "", Before);
 Inst->setDebugLoc (Before->getDebugLoc ());
 return Inst;
}

extern "C"
LoadInst*
Insert_Load_Before (Type *Ty, Value *Ptr, Instruction *Before)
{
 auto Inst = new LoadInst (Ty, Ptr, "", Before);
 Inst->setDebugLoc (Before->getDebugLoc ());
 return Inst;
}

extern "C"
void
Insert_Store_Before (Value *Val, Value *Ptr, Instruction *Before)
{
 auto Inst = new StoreInst (Val, Ptr, Before);
 Inst->setDebugLoc (Before->getDebugLoc ());
}

extern "C"
bool
All_Preds_Are_Unc_Branches (BasicBlock *BB)
{
  for (auto *PBB : predecessors (BB))
    if (PBB->getTerminator ()->getNumSuccessors () != 1)
      return false;

  return true;
}

extern "C"
bool
Is_Dead_Basic_Block (BasicBlock *BB)
{
  return BB->hasNPredecessors (0);
}

extern "C"
Value *
Get_First_Non_Phi_Or_Dbg (BasicBlock *BB)
{
  return BB->getFirstNonPHIOrDbg ();
}

extern "C"
bool
Is_Lifetime_Intrinsic (Instruction *v)
{
  return v->isLifetimeStartOrEnd ();
}

/* If we call into CCG from GNAT LLVM during the compilation process to
   record some information about a Value (for example, its signedness),
   there's a chance that that value will be deleted during the optimization
   process and that same address used for some other value. So we need to
   set a callback on that value to notify us that this happened so we can
   delete the value from our table. The below class and function is used
   for that purpose.  */

class GNATCallbackVH final : public CallbackVH
{
  void (*fn) (Value *);
  void deleted () override;

 public:
  GNATCallbackVH (Value *V, void (*Fn) (Value *));
};

void
GNATCallbackVH::deleted()
{
  (this->fn) (getValPtr());

  // This callback object is no longer needed, and we don't keep references to
  // it anywhere, so self-delete.
  delete this;
}

GNATCallbackVH::GNATCallbackVH (Value *V, void (*Fn) (Value *))
  : CallbackVH (V), fn (Fn)
{
}

extern "C"
void
Notify_On_Value_Delete (Value *V, void (*Fn) (Value *))
{
  GNATCallbackVH *CB= new GNATCallbackVH (V, Fn);
}

extern "C"
void
Set_Module_PIC_PIE (Module *M, int PIC, int PIE)
{
  M->setPICLevel(static_cast<PICLevel::Level>(PIC));
  M->setPIELevel(static_cast<PIELevel::Level>(PIE));
}

extern "C"
bool
Has_Default_PIE (const char *Target)
{
  Triple TargetTriple(Target);

  // Like Clang, we default to PIE on Linux and x86_64 Windows (out of the
  // supported targets). See the comment at the call site in GNATLLVM.Codegen
  // for details.
  return TargetTriple.isOSLinux() ||
    (TargetTriple.isOSWindows() && TargetTriple.getArch() == Triple::x86_64);
}

extern "C"
bool
Has_SEH (const char *Target)
{
  Triple TargetTriple(Target);

  return TargetTriple.isOSWindows ()
         && (TargetTriple.isX86 ()
             || TargetTriple.getArch () == Triple::aarch64);
}

extern "C"
const char *
Get_Personality_Function_Name (const char *Target)
{
  Triple TargetTriple(Target);

  // For now, we don't support SJLJ exceptions, so we just need to decide
  // whether the target uses SEH.
  if (TargetTriple.isOSWindows())
    return "__gnat_personality_seh0";
  else
    return "__gnat_personality_v0";
}

extern "C"
char *
Get_Features (const char *TargetTriple, const char *Arch, const char *CPU)
{
  // This is a simplified version of Clang's tools::getTargetFeatures (see
  // clang/lib/Driver/ToolChains/CommonArgs.cpp), adapted to the arguments that
  // we have available and the defaults that we set during option parsing.

  Triple T(TargetTriple);

  // ??? We may want to add code for more target architectures here; Clang
  // often puts it in clang/lib/Driver/ToolChains/Arch/.

  switch (T.getArch()) {
  default:
    return nullptr;

  case Triple::aarch64: {
    // Here we replicate relevant parts of Clang's aarch64::getAArch64Features
    // (see clang/lib/Driver/ToolChains/Arch/AArch64.cpp).

    std::vector<StringRef> Features;

    // Clang enables NEON by default, so we do the same.
    Features.push_back("+neon");

    auto ArchLowerCase = StringRef(Arch).lower();
    if (ArchLowerCase.empty()) {
      // Clang defaults to ARMv8-A if the user hasn't specified a CPU either,
      // so let's do the same.
      if (StringRef(CPU) == "generic")
        ArchLowerCase = "armv8-a";
      else
        // ??? Clang can also derive the list of features from -mcpu (which only
        // happens if -march isn't specified); we may want to do the same here.
        return nullptr;
    }

    // The -march option value has the format
    // "architecture+feature1+feature2", so first split the architecture from
    // the list of features.
    auto const ArchSplit = StringRef(ArchLowerCase).split("+");
    auto const ArchInfo = AArch64::parseArch(ArchSplit.first);

    if (ArchInfo == AArch64::INVALID) {
      errs() << "warning: ignoring unsupported -march value " << Arch << "\n";
      return nullptr;
    }

    Features.push_back(ArchInfo.ArchFeature);

    // Now process the user-specified additional features, if any.
    if (!ArchSplit.second.empty()) {
      SmallVector<StringRef, 8> FeatureSplit;
      ArchSplit.second.split(FeatureSplit, "+", /*MaxSplit=*/-1,
                             /*KeepEmpty=*/false);

      for (auto const Feature : FeatureSplit) {
        auto const FeatureName = AArch64::getArchExtFeature(Feature);
        if (!FeatureName.empty())
          Features.push_back(FeatureName);
        else
          errs() << "warning: ignoring unsupported feature " << Feature << "\n";
      }
    }

    // ??? There is a lot more in Clang's AArch64 feature lookup code that we
    // may want to copy.

    return strdup(join(Features, ",").c_str());
  }
  }
}

extern "C"
unsigned
Get_Default_Address_Space (const DataLayout &DL)
{
  return DL.getDefaultGlobalsAddressSpace();
}

extern "C"
void
Set_Absolute_Address (LLVMContext &Ctx, Value *V, Value *Addr)
{
  cast<GlobalObject>(V)->setMetadata(
      LLVMContext::MD_absolute_symbol,
      MDNode::get(Ctx, {ValueAsMetadata::get(Addr)}));
}

extern "C"
bool
Need_Enable_Execute_Stack (const char *Target)
{
  // Decide whether we need to call __enable_execute_stack in order to make the
  // stack executable. The GNU linker does this automatically in ELF binaries,
  // but on Windows the helper function is required.

  Triple TargetTriple(Target);
  return TargetTriple.isOSWindows();
}

extern "C"
void
Print_Targets (void)
{
  TargetRegistry::printRegisteredTargetsForVersion(outs());
}
