pragma Ada_2005;
pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Error is

   function Get_Error_Message
     (Err : Error_T)
      return String
   is
   begin
      return Value (Get_Error_Message_C (Err));
   end Get_Error_Message;

   procedure Dispose_Error_Message
     (Err_Msg : String)
   is
      Err_Msg_Array  : aliased char_array := To_C (Err_Msg);
      Err_Msg_String : constant chars_ptr := To_Chars_Ptr (Err_Msg_Array'Unchecked_Access);
   begin
      Dispose_Error_Message_C (Err_Msg_String);
   end Dispose_Error_Message;

end LLVM.Error;
