pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Opt_Remarks is

   function Opt_Remark_Parser_Has_Error
     (Parser : Opt_Remark_Parser_T)
      return Boolean
   is
   begin
      return Opt_Remark_Parser_Has_Error_C (Parser) /= 0;
   end Opt_Remark_Parser_Has_Error;

   function Opt_Remark_Parser_Get_Error_Message
     (Parser : Opt_Remark_Parser_T)
      return String
   is
   begin
      return Value (Opt_Remark_Parser_Get_Error_Message_C (Parser));
   end Opt_Remark_Parser_Get_Error_Message;

end LLVM.Opt_Remarks;
