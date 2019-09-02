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
      return Boolean
   is
   begin
      return Verify_Module_C (M, Action, Out_Message) /= 0;
   end Verify_Module;

   function Verify_Function
     (Fn     : LLVM.Types.Value_T;
      Action : Verifier_Failure_Action_T)
      return Boolean
   is
   begin
      return Verify_Function_C (Fn, Action) /= 0;
   end Verify_Function;

end LLVM.Analysis;
