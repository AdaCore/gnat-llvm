pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Transforms_Pass_Builder is

   function Run_Passes
     (M       : LLVM.Types.Module_T;
      Passes  : Interfaces.C.Strings.chars_ptr;
      TM      : LLVM.Target_Machine.Target_Machine_T;
      Options : Pass_Builder_Options_T)
      return LLVM.Error.Error_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMRunPasses";
   function Run_Passes
     (M       : LLVM.Types.Module_T;
      Passes  : String;
      TM      : LLVM.Target_Machine.Target_Machine_T;
      Options : Pass_Builder_Options_T)
      return LLVM.Error.Error_T
   is
      Return_Value  : LLVM.Error.Error_T;
      Passes_Array  : aliased char_array := To_C (Passes);
      Passes_String : constant chars_ptr := To_Chars_Ptr (Passes_Array'Unchecked_Access);
   begin
      Return_Value := Run_Passes (M, Passes_String, TM, Options);
      return Return_Value;
   end Run_Passes;

   procedure Pass_Builder_Options_Set_Verify_Each
     (Options     : Pass_Builder_Options_T;
      Verify_Each : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMPassBuilderOptionsSetVerifyEach";
   procedure Pass_Options_Set_Verify_Each
     (Options     : Pass_Builder_Options_T;
      Verify_Each : Boolean)
   is
      Verify_Each_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Verify_Each);
   begin
      Pass_Builder_Options_Set_Verify_Each (Options, Verify_Each_Bool);
   end Pass_Options_Set_Verify_Each;

   procedure Pass_Builder_Options_Set_Debug_Logging
     (Options       : Pass_Builder_Options_T;
      Debug_Logging : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMPassBuilderOptionsSetDebugLogging";
   procedure Pass_Options_Set_Debug_Logging
     (Options       : Pass_Builder_Options_T;
      Debug_Logging : Boolean)
   is
      Debug_Logging_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Debug_Logging);
   begin
      Pass_Builder_Options_Set_Debug_Logging (Options, Debug_Logging_Bool);
   end Pass_Options_Set_Debug_Logging;

   procedure Pass_Builder_Options_Set_Loop_Interleaving
     (Options           : Pass_Builder_Options_T;
      Loop_Interleaving : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMPassBuilderOptionsSetLoopInterleaving";
   procedure Pass_Options_Set_Loop_Interleaving
     (Options           : Pass_Builder_Options_T;
      Loop_Interleaving : Boolean)
   is
      Loop_Interleaving_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Loop_Interleaving);
   begin
      Pass_Builder_Options_Set_Loop_Interleaving (Options, Loop_Interleaving_Bool);
   end Pass_Options_Set_Loop_Interleaving;

   procedure Pass_Builder_Options_Set_Loop_Vectorization
     (Options            : Pass_Builder_Options_T;
      Loop_Vectorization : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMPassBuilderOptionsSetLoopVectorization";
   procedure Pass_Options_Set_Loop_Vectorization
     (Options            : Pass_Builder_Options_T;
      Loop_Vectorization : Boolean)
   is
      Loop_Vectorization_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Loop_Vectorization);
   begin
      Pass_Builder_Options_Set_Loop_Vectorization (Options, Loop_Vectorization_Bool);
   end Pass_Options_Set_Loop_Vectorization;

   procedure Pass_Builder_Options_Set_SLP_Vectorization
     (Options           : Pass_Builder_Options_T;
      SLP_Vectorization : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMPassBuilderOptionsSetSLPVectorization";
   procedure Pass_Options_Set_SLP_Vectorization
     (Options           : Pass_Builder_Options_T;
      SLP_Vectorization : Boolean)
   is
      SLP_Vectorization_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (SLP_Vectorization);
   begin
      Pass_Builder_Options_Set_SLP_Vectorization (Options, SLP_Vectorization_Bool);
   end Pass_Options_Set_SLP_Vectorization;

   procedure Pass_Builder_Options_Set_Loop_Unrolling
     (Options        : Pass_Builder_Options_T;
      Loop_Unrolling : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMPassBuilderOptionsSetLoopUnrolling";
   procedure Pass_Options_Set_Loop_Unrolling
     (Options        : Pass_Builder_Options_T;
      Loop_Unrolling : Boolean)
   is
      Loop_Unrolling_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Loop_Unrolling);
   begin
      Pass_Builder_Options_Set_Loop_Unrolling (Options, Loop_Unrolling_Bool);
   end Pass_Options_Set_Loop_Unrolling;

   procedure Pass_Builder_Options_Set_Forget_All_SCEV_In_Loop_Unroll
     (Options                        : Pass_Builder_Options_T;
      Forget_All_SCEV_In_Loop_Unroll : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMPassBuilderOptionsSetForgetAllSCEVInLoopUnroll";
   procedure Pass_Options_Set_Forget_All_SCEV_In_Loop_Unroll
     (Options                        : Pass_Builder_Options_T;
      Forget_All_SCEV_In_Loop_Unroll : Boolean)
   is
      Forget_All_SCEV_In_Loop_Unroll_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Forget_All_SCEV_In_Loop_Unroll);
   begin
      Pass_Builder_Options_Set_Forget_All_SCEV_In_Loop_Unroll (Options, Forget_All_SCEV_In_Loop_Unroll_Bool);
   end Pass_Options_Set_Forget_All_SCEV_In_Loop_Unroll;

   procedure Pass_Builder_Options_Set_Call_Graph_Profile
     (Options            : Pass_Builder_Options_T;
      Call_Graph_Profile : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMPassBuilderOptionsSetCallGraphProfile";
   procedure Pass_Options_Set_Call_Graph_Profile
     (Options            : Pass_Builder_Options_T;
      Call_Graph_Profile : Boolean)
   is
      Call_Graph_Profile_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Call_Graph_Profile);
   begin
      Pass_Builder_Options_Set_Call_Graph_Profile (Options, Call_Graph_Profile_Bool);
   end Pass_Options_Set_Call_Graph_Profile;

   procedure Pass_Builder_Options_Set_Merge_Functions
     (Options         : Pass_Builder_Options_T;
      Merge_Functions : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMPassBuilderOptionsSetMergeFunctions";
   procedure Pass_Options_Set_Merge_Functions
     (Options         : Pass_Builder_Options_T;
      Merge_Functions : Boolean)
   is
      Merge_Functions_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Merge_Functions);
   begin
      Pass_Builder_Options_Set_Merge_Functions (Options, Merge_Functions_Bool);
   end Pass_Options_Set_Merge_Functions;

end LLVM.Transforms_Pass_Builder;
