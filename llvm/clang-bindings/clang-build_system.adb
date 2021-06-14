pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body Clang.Build_System is

   function Virtual_File_Overlay_Add_File_Mapping
     (Arg_1        : Virtual_File_Overlay_T;
      Virtual_Path : String;
      Real_Path    : String)
      return Clang.CX_Error_Code.Error_Code_T
   is
      Virtual_Path_Array  : aliased char_array := To_C (Virtual_Path);
      Virtual_Path_String : constant chars_ptr := To_Chars_Ptr (Virtual_Path_Array'Unchecked_Access);
      Real_Path_Array     : aliased char_array := To_C (Real_Path);
      Real_Path_String    : constant chars_ptr := To_Chars_Ptr (Real_Path_Array'Unchecked_Access);
   begin
      return Virtual_File_Overlay_Add_File_Mapping_C (Arg_1, Virtual_Path_String, Real_Path_String);
   end Virtual_File_Overlay_Add_File_Mapping;

   function Module_Map_Descriptor_Set_Framework_Module_Name
     (Arg_1 : Module_Map_Descriptor_T;
      Name  : String)
      return Clang.CX_Error_Code.Error_Code_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Module_Map_Descriptor_Set_Framework_Module_Name_C (Arg_1, Name_String);
   end Module_Map_Descriptor_Set_Framework_Module_Name;

   function Module_Map_Descriptor_Set_Umbrella_Header
     (Arg_1 : Module_Map_Descriptor_T;
      Name  : String)
      return Clang.CX_Error_Code.Error_Code_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Module_Map_Descriptor_Set_Umbrella_Header_C (Arg_1, Name_String);
   end Module_Map_Descriptor_Set_Umbrella_Header;

end Clang.Build_System;
