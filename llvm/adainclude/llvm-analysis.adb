pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Analysis is

   function Verify_Module
     (M           : LLVM.Types.Module_T;
      Action      : Verifier_Failure_Action_T;
      Out_Message : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMVerifyModule";
   function Verify_Module
     (M           : LLVM.Types.Module_T;
      Action      : Verifier_Failure_Action_T;
      Out_Message : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Verify_Module (M, Action, Out_Message);
      return Return_Value /= 0;
   end Verify_Module;

   function Verify_Function
     (Fn     : LLVM.Types.Value_T;
      Action : Verifier_Failure_Action_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMVerifyFunction";
   function Verify_Function
     (Fn     : LLVM.Types.Value_T;
      Action : Verifier_Failure_Action_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Verify_Function (Fn, Action);
      return Return_Value /= 0;
   end Verify_Function;

end LLVM.Analysis;
