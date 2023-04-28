pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body Clang.CX_Source_Location is

   function Location_Is_In_System_Header
     (Location : Source_Location_T)
      return int
   with Import => True,
        Convention => C,
        External_Name => "clang_Location_isInSystemHeader";
   function Location_Is_In_System_Header
     (Location : Source_Location_T)
      return Boolean
   is
      Return_Value : int;
   begin
      Return_Value := Location_Is_In_System_Header (Location);
      return Return_Value /= 0;
   end Location_Is_In_System_Header;

   function Location_Is_From_Main_File
     (Location : Source_Location_T)
      return int
   with Import => True,
        Convention => C,
        External_Name => "clang_Location_isFromMainFile";
   function Location_Is_From_Main_File
     (Location : Source_Location_T)
      return Boolean
   is
      Return_Value : int;
   begin
      Return_Value := Location_Is_From_Main_File (Location);
      return Return_Value /= 0;
   end Location_Is_From_Main_File;

   function Range_Is_Null
     (C_Range : Source_Range_T)
      return int
   with Import => True,
        Convention => C,
        External_Name => "clang_Range_isNull";
   function Range_Is_Null
     (C_Range : Source_Range_T)
      return Boolean
   is
      Return_Value : int;
   begin
      Return_Value := Range_Is_Null (C_Range);
      return Return_Value /= 0;
   end Range_Is_Null;

end Clang.CX_Source_Location;
