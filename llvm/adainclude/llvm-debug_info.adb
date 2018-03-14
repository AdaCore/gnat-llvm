pragma Ada_2005;
pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Debug_Info is

   function Strip_Module_Debug_Info
     (Module : LLVM.Types.Module_T)
      return Boolean
   is
   begin
      return Strip_Module_Debug_Info_C (Module) /= 0;
   end Strip_Module_Debug_Info;

   function DI_Create_Compile_Unit
     (Builder                  : LLVM.Types.DI_Builder_T;
      Lang                     : DWARF_Source_Language_T;
      File_Ref                 : LLVM.Types.Metadata_T;
      Producer                 : String;
      Producer_Len             : stddef_h.size_t;
      is_Optimized             : Boolean;
      Flags                    : String;
      Flags_Len                : stddef_h.size_t;
      Runtime_Ver              : unsigned;
      Split_Name               : String;
      Split_Name_Len           : stddef_h.size_t;
      Kind                     : DWARF_Emission_Kind_T;
      DWO_Id                   : unsigned;
      Split_Debug_Inlining     : Boolean;
      Debug_Info_For_Profiling : Boolean)
      return LLVM.Types.Metadata_T
   is
      Producer_Array                : aliased char_array := To_C (Producer);
      Producer_String               : constant chars_ptr := To_Chars_Ptr (Producer_Array'Unchecked_Access);
      is_Optimized_Bool             : constant LLVM.Types.Bool_T := Boolean'Pos (is_Optimized);
      Flags_Array                   : aliased char_array := To_C (Flags);
      Flags_String                  : constant chars_ptr := To_Chars_Ptr (Flags_Array'Unchecked_Access);
      Split_Name_Array              : aliased char_array := To_C (Split_Name);
      Split_Name_String             : constant chars_ptr := To_Chars_Ptr (Split_Name_Array'Unchecked_Access);
      Split_Debug_Inlining_Bool     : constant LLVM.Types.Bool_T := Boolean'Pos (Split_Debug_Inlining);
      Debug_Info_For_Profiling_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Debug_Info_For_Profiling);
   begin
      return DI_Builder_Create_Compile_Unit_C (Builder, Lang, File_Ref, Producer_String, Producer_Len, is_Optimized_Bool, Flags_String, Flags_Len, Runtime_Ver, Split_Name_String, Split_Name_Len, Kind, DWO_Id, Split_Debug_Inlining_Bool, Debug_Info_For_Profiling_Bool);
   end DI_Create_Compile_Unit;

   function DI_Create_File
     (Builder       : LLVM.Types.DI_Builder_T;
      Filename      : String;
      Filename_Len  : stddef_h.size_t;
      Directory     : String;
      Directory_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   is
      Filename_Array   : aliased char_array := To_C (Filename);
      Filename_String  : constant chars_ptr := To_Chars_Ptr (Filename_Array'Unchecked_Access);
      Directory_Array  : aliased char_array := To_C (Directory);
      Directory_String : constant chars_ptr := To_Chars_Ptr (Directory_Array'Unchecked_Access);
   begin
      return DI_Builder_Create_File_C (Builder, Filename_String, Filename_Len, Directory_String, Directory_Len);
   end DI_Create_File;

end LLVM.Debug_Info;
