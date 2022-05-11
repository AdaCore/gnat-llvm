pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Error is

   function Get_Error_Message
     (Err : Error_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetErrorMessage";
   function Get_Error_Message
     (Err : Error_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Error_Message (Err);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Error_Message;

   procedure Dispose_Error_Message
     (Err_Msg : Interfaces.C.Strings.chars_ptr)
   with Import => True,
        Convention => C,
        External_Name => "LLVMDisposeErrorMessage";
   procedure Dispose_Error_Message
     (Err_Msg : String)
   is
      Err_Msg_Array  : aliased char_array := To_C (Err_Msg);
      Err_Msg_String : constant chars_ptr := To_Chars_Ptr (Err_Msg_Array'Unchecked_Access);
   begin
      Dispose_Error_Message (Err_Msg_String);
   end Dispose_Error_Message;

   function Create_String_Error
     (Err_Msg : Interfaces.C.Strings.chars_ptr)
      return Error_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMCreateStringError";
   function Create_String_Error
     (Err_Msg : String)
      return Error_T
   is
      Return_Value   : Error_T;
      Err_Msg_Array  : aliased char_array := To_C (Err_Msg);
      Err_Msg_String : constant chars_ptr := To_Chars_Ptr (Err_Msg_Array'Unchecked_Access);
   begin
      Return_Value := Create_String_Error (Err_Msg_String);
      return Return_Value;
   end Create_String_Error;

end LLVM.Error;
