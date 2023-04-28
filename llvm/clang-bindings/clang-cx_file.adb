pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body Clang.CX_File is

   function Get_File_Name
     (S_File : File_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getFileName";
   function Get_File_Name
     (S_File : File_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_File_Name (S_File);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_File_Name;

   function File_Is_Equal
     (File_1 : File_T;
      File_2 : File_T)
      return int
   with Import => True,
        Convention => C,
        External_Name => "clang_File_isEqual";
   function File_Is_Equal
     (File_1 : File_T;
      File_2 : File_T)
      return Boolean
   is
      Return_Value : int;
   begin
      Return_Value := File_Is_Equal (File_1, File_2);
      return Return_Value /= 0;
   end File_Is_Equal;

   function File_Try_Get_Real_Path_Name
     (File : File_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_File_tryGetRealPathName";
   function File_Try_Get_Real_Path_Name
     (File : File_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := File_Try_Get_Real_Path_Name (File);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end File_Try_Get_Real_Path_Name;

end Clang.CX_File;
