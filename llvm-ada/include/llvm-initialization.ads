pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;

package LLVM.Initialization is

   procedure Initialize_Core (R : LLVM.Types.Pass_Registry_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Initialization.h:34
   pragma Import (C, Initialize_Core, "LLVMInitializeCore");

   procedure Initialize_Transform_Utils (R : LLVM.Types.Pass_Registry_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Initialization.h:35
   pragma Import (C, Initialize_Transform_Utils, "LLVMInitializeTransformUtils");

   procedure Initialize_Scalar_Opts (R : LLVM.Types.Pass_Registry_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Initialization.h:36
   pragma Import (C, Initialize_Scalar_Opts, "LLVMInitializeScalarOpts");

   procedure Initialize_Obj_CARC_Opts (R : LLVM.Types.Pass_Registry_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Initialization.h:37
   pragma Import (C, Initialize_Obj_CARC_Opts, "LLVMInitializeObjCARCOpts");

   procedure Initialize_Vectorization (R : LLVM.Types.Pass_Registry_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Initialization.h:38
   pragma Import (C, Initialize_Vectorization, "LLVMInitializeVectorization");

   procedure Initialize_Inst_Combine (R : LLVM.Types.Pass_Registry_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Initialization.h:39
   pragma Import (C, Initialize_Inst_Combine, "LLVMInitializeInstCombine");

   procedure Initialize_IPO (R : LLVM.Types.Pass_Registry_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Initialization.h:40
   pragma Import (C, Initialize_IPO, "LLVMInitializeIPO");

   procedure Initialize_Instrumentation (R : LLVM.Types.Pass_Registry_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Initialization.h:41
   pragma Import (C, Initialize_Instrumentation, "LLVMInitializeInstrumentation");

   procedure Initialize_Analysis (R : LLVM.Types.Pass_Registry_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Initialization.h:42
   pragma Import (C, Initialize_Analysis, "LLVMInitializeAnalysis");

   procedure Initialize_IPA (R : LLVM.Types.Pass_Registry_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Initialization.h:43
   pragma Import (C, Initialize_IPA, "LLVMInitializeIPA");

   procedure Initialize_Code_Gen (R : LLVM.Types.Pass_Registry_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Initialization.h:44
   pragma Import (C, Initialize_Code_Gen, "LLVMInitializeCodeGen");

   procedure Initialize_Target (R : LLVM.Types.Pass_Registry_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Initialization.h:45
   pragma Import (C, Initialize_Target, "LLVMInitializeTarget");

end LLVM.Initialization;

