pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Transforms_Pass_Builder is

   function Run_Passes
     (M       : LLVM.Types.Module_T;
      Passes  : String;
      TM      : LLVM.Target_Machine.Target_Machine_T;
      Options : Pass_Builder_Options_T)
      return LLVM.Error.Error_T
   is
      Passes_Array  : aliased char_array := To_C (Passes);
      Passes_String : constant chars_ptr := To_Chars_Ptr (Passes_Array'Unchecked_Access);
   begin
      return Run_Passes_C (M, Passes_String, TM, Options);
   end Run_Passes;

   procedure Pass_Options_Set_Verify_Each
     (Options     : Pass_Builder_Options_T;
      Verify_Each : Boolean)
   is
      Verify_Each_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Verify_Each);
   begin
      Pass_Builder_Options_Set_Verify_Each_C (Options, Verify_Each_Bool);
   end Pass_Options_Set_Verify_Each;

   procedure Pass_Options_Set_Debug_Logging
     (Options       : Pass_Builder_Options_T;
      Debug_Logging : Boolean)
   is
      Debug_Logging_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Debug_Logging);
   begin
      Pass_Builder_Options_Set_Debug_Logging_C (Options, Debug_Logging_Bool);
   end Pass_Options_Set_Debug_Logging;

   procedure Pass_Options_Set_Loop_Interleaving
     (Options           : Pass_Builder_Options_T;
      Loop_Interleaving : Boolean)
   is
      Loop_Interleaving_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Loop_Interleaving);
   begin
      Pass_Builder_Options_Set_Loop_Interleaving_C (Options, Loop_Interleaving_Bool);
   end Pass_Options_Set_Loop_Interleaving;

   procedure Pass_Options_Set_Loop_Vectorization
     (Options            : Pass_Builder_Options_T;
      Loop_Vectorization : Boolean)
   is
      Loop_Vectorization_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Loop_Vectorization);
   begin
      Pass_Builder_Options_Set_Loop_Vectorization_C (Options, Loop_Vectorization_Bool);
   end Pass_Options_Set_Loop_Vectorization;

   procedure Pass_Options_Set_SLP_Vectorization
     (Options           : Pass_Builder_Options_T;
      SLP_Vectorization : Boolean)
   is
      SLP_Vectorization_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (SLP_Vectorization);
   begin
      Pass_Builder_Options_Set_SLP_Vectorization_C (Options, SLP_Vectorization_Bool);
   end Pass_Options_Set_SLP_Vectorization;

   procedure Pass_Options_Set_Loop_Unrolling
     (Options        : Pass_Builder_Options_T;
      Loop_Unrolling : Boolean)
   is
      Loop_Unrolling_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Loop_Unrolling);
   begin
      Pass_Builder_Options_Set_Loop_Unrolling_C (Options, Loop_Unrolling_Bool);
   end Pass_Options_Set_Loop_Unrolling;

   procedure Pass_Options_Set_Forget_All_SCEV_In_Loop_Unroll
     (Options                        : Pass_Builder_Options_T;
      Forget_All_SCEV_In_Loop_Unroll : Boolean)
   is
      Forget_All_SCEV_In_Loop_Unroll_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Forget_All_SCEV_In_Loop_Unroll);
   begin
      Pass_Builder_Options_Set_Forget_All_SCEV_In_Loop_Unroll_C (Options, Forget_All_SCEV_In_Loop_Unroll_Bool);
   end Pass_Options_Set_Forget_All_SCEV_In_Loop_Unroll;

   procedure Pass_Options_Set_Call_Graph_Profile
     (Options            : Pass_Builder_Options_T;
      Call_Graph_Profile : Boolean)
   is
      Call_Graph_Profile_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Call_Graph_Profile);
   begin
      Pass_Builder_Options_Set_Call_Graph_Profile_C (Options, Call_Graph_Profile_Bool);
   end Pass_Options_Set_Call_Graph_Profile;

   procedure Pass_Options_Set_Merge_Functions
     (Options         : Pass_Builder_Options_T;
      Merge_Functions : Boolean)
   is
      Merge_Functions_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Merge_Functions);
   begin
      Pass_Builder_Options_Set_Merge_Functions_C (Options, Merge_Functions_Bool);
   end Pass_Options_Set_Merge_Functions;

end LLVM.Transforms_Pass_Builder;
