pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Remarks is

   function Remark_String_Get_Data
     (Str : Remark_String_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMRemarkStringGetData";
   function Remark_String_Get_Data
     (Str : Remark_String_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Remark_String_Get_Data (Str);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Remark_String_Get_Data;

   function Remark_Parser_Has_Error
     (Parser : Remark_Parser_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMRemarkParserHasError";
   function Remark_Parser_Has_Error
     (Parser : Remark_Parser_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Remark_Parser_Has_Error (Parser);
      return Return_Value /= 0;
   end Remark_Parser_Has_Error;

   function Remark_Parser_Get_Error_Message
     (Parser : Remark_Parser_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMRemarkParserGetErrorMessage";
   function Remark_Parser_Get_Error_Message
     (Parser : Remark_Parser_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Remark_Parser_Get_Error_Message (Parser);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Remark_Parser_Get_Error_Message;

end LLVM.Remarks;
