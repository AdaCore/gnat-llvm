pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Link_Time_Optimizer is

   function Read_Object_File
     (Lto            : Lto_T_T;
      Input_Filename : String)
      return Lto_Status_T_T
   is
      Input_Filename_Array  : aliased char_array := To_C (Input_Filename);
      Input_Filename_String : constant chars_ptr := To_Chars_Ptr (Input_Filename_Array'Unchecked_Access);
   begin
      return Read_Object_File_C (Lto, Input_Filename_String);
   end Read_Object_File;

   function Optimize_Modules
     (Lto             : Lto_T_T;
      Output_Filename : String)
      return Lto_Status_T_T
   is
      Output_Filename_Array  : aliased char_array := To_C (Output_Filename);
      Output_Filename_String : constant chars_ptr := To_Chars_Ptr (Output_Filename_Array'Unchecked_Access);
   begin
      return Optimize_Modules_C (Lto, Output_Filename_String);
   end Optimize_Modules;

end LLVM.Link_Time_Optimizer;
