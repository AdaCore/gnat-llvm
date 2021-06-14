pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body Clang.CX_Compilation_Database is

   function Compilation_Database_From_Directory
     (Build_Dir  : String;
      Error_Code : access Compilation_Database_Error_T)
      return Compilation_Database_T
   is
      Build_Dir_Array  : aliased char_array := To_C (Build_Dir);
      Build_Dir_String : constant chars_ptr := To_Chars_Ptr (Build_Dir_Array'Unchecked_Access);
   begin
      return Compilation_Database_From_Directory_C (Build_Dir_String, Error_Code);
   end Compilation_Database_From_Directory;

   function Compilation_Database_Get_Compile_Commands
     (Arg_1              : Compilation_Database_T;
      Complete_File_Name : String)
      return Compile_Commands_T
   is
      Complete_File_Name_Array  : aliased char_array := To_C (Complete_File_Name);
      Complete_File_Name_String : constant chars_ptr := To_Chars_Ptr (Complete_File_Name_Array'Unchecked_Access);
   begin
      return Compilation_Database_Get_Compile_Commands_C (Arg_1, Complete_File_Name_String);
   end Compilation_Database_Get_Compile_Commands;

end Clang.CX_Compilation_Database;
