pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Target is

   function Create_Target_Data
     (String_Rep : Interfaces.C.Strings.chars_ptr)
      return Target_Data_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMCreateTargetData";
   function Create_Target_Data
     (String_Rep : String)
      return Target_Data_T
   is
      Return_Value      : Target_Data_T;
      String_Rep_Array  : aliased char_array := To_C (String_Rep);
      String_Rep_String : constant chars_ptr := To_Chars_Ptr (String_Rep_Array'Unchecked_Access);
   begin
      Return_Value := Create_Target_Data (String_Rep_String);
      return Return_Value;
   end Create_Target_Data;

   function Copy_String_Rep_Of_Target_Data
     (TD : Target_Data_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMCopyStringRepOfTargetData";
   function Copy_String_Rep_Of_Target_Data
     (TD : Target_Data_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Copy_String_Rep_Of_Target_Data (TD);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Copy_String_Rep_Of_Target_Data;

end LLVM.Target;
