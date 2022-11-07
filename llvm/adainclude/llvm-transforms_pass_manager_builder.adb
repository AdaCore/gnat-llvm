pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Transforms_Pass_Manager_Builder is

   procedure Pass_Manager_Builder_Set_Disable_Unit_At_A_Time
     (PMB   : Pass_Manager_Builder_T;
      Value : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMPassManagerBuilderSetDisableUnitAtATime";
   procedure Pass_Manager_Set_Disable_Unit_At_A_Time
     (PMB   : Pass_Manager_Builder_T;
      Value : Boolean)
   is
      Value_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Value);
   begin
      Pass_Manager_Builder_Set_Disable_Unit_At_A_Time (PMB, Value_Bool);
   end Pass_Manager_Set_Disable_Unit_At_A_Time;

   procedure Pass_Manager_Builder_Set_Disable_Unroll_Loops
     (PMB   : Pass_Manager_Builder_T;
      Value : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMPassManagerBuilderSetDisableUnrollLoops";
   procedure Pass_Manager_Set_Disable_Unroll_Loops
     (PMB   : Pass_Manager_Builder_T;
      Value : Boolean)
   is
      Value_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Value);
   begin
      Pass_Manager_Builder_Set_Disable_Unroll_Loops (PMB, Value_Bool);
   end Pass_Manager_Set_Disable_Unroll_Loops;

   procedure Pass_Manager_Builder_Set_Disable_Simplify_Lib_Calls
     (PMB   : Pass_Manager_Builder_T;
      Value : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMPassManagerBuilderSetDisableSimplifyLibCalls";
   procedure Pass_Manager_Set_Disable_Simplify_Lib_Calls
     (PMB   : Pass_Manager_Builder_T;
      Value : Boolean)
   is
      Value_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Value);
   begin
      Pass_Manager_Builder_Set_Disable_Simplify_Lib_Calls (PMB, Value_Bool);
   end Pass_Manager_Set_Disable_Simplify_Lib_Calls;

end LLVM.Transforms_Pass_Manager_Builder;
