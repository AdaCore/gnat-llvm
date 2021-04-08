pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Remarks is

   function Remark_String_Get_Data
     (Str : Remark_String_T)
      return String
   is
   begin
      return Value (Remark_String_Get_Data_C (Str));
   end Remark_String_Get_Data;

   function Remark_Parser_Has_Error
     (Parser : Remark_Parser_T)
      return Boolean
   is
   begin
      return Remark_Parser_Has_Error_C (Parser) /= 0;
   end Remark_Parser_Has_Error;

   function Remark_Parser_Get_Error_Message
     (Parser : Remark_Parser_T)
      return String
   is
   begin
      return Value (Remark_Parser_Get_Error_Message_C (Parser));
   end Remark_Parser_Get_Error_Message;

end LLVM.Remarks;
