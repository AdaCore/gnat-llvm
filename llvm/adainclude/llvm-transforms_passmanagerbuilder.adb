pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Transforms_Passmanagerbuilder is

   procedure Pass_Manager_Set_Disable_Unit_At_A_Time
     (PMB   : Pass_Manager_Builder_T;
      Value : Boolean)
   is
      Value_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Value);
   begin
      Pass_Manager_Builder_Set_Disable_Unit_At_A_Time_C (PMB, Value_Bool);
   end Pass_Manager_Set_Disable_Unit_At_A_Time;

   procedure Pass_Manager_Set_Disable_Unroll_Loops
     (PMB   : Pass_Manager_Builder_T;
      Value : Boolean)
   is
      Value_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Value);
   begin
      Pass_Manager_Builder_Set_Disable_Unroll_Loops_C (PMB, Value_Bool);
   end Pass_Manager_Set_Disable_Unroll_Loops;

   procedure Pass_Manager_Set_Disable_Simplify_Lib_Calls
     (PMB   : Pass_Manager_Builder_T;
      Value : Boolean)
   is
      Value_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Value);
   begin
      Pass_Manager_Builder_Set_Disable_Simplify_Lib_Calls_C (PMB, Value_Bool);
   end Pass_Manager_Set_Disable_Simplify_Lib_Calls;

   procedure Pass_Manager_Populate_LTO_Pass_Manager
     (PMB         : Pass_Manager_Builder_T;
      PM          : LLVM.Types.Pass_Manager_T;
      Internalize : Boolean;
      Run_Inliner : Boolean)
   is
      Internalize_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Internalize);
      Run_Inliner_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Run_Inliner);
   begin
      Pass_Manager_Builder_Populate_LTO_Pass_Manager_C (PMB, PM, Internalize_Bool, Run_Inliner_Bool);
   end Pass_Manager_Populate_LTO_Pass_Manager;

end LLVM.Transforms_Passmanagerbuilder;
