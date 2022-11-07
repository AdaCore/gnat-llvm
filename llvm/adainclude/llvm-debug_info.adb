pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Debug_Info is

   function Strip_Module_Debug_Info
     (Module : LLVM.Types.Module_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMStripModuleDebugInfo";
   function Strip_Module_Debug_Info
     (Module : LLVM.Types.Module_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Strip_Module_Debug_Info (Module);
      return Return_Value /= 0;
   end Strip_Module_Debug_Info;

   function DI_Builder_Create_Compile_Unit
     (Builder                  : LLVM.Types.DI_Builder_T;
      Lang                     : DWARF_Source_Language_T;
      File_Ref                 : LLVM.Types.Metadata_T;
      Producer                 : Interfaces.C.Strings.chars_ptr;
      Producer_Len             : stddef_h.size_t;
      Is_Optimized             : LLVM.Types.Bool_T;
      Flags                    : Interfaces.C.Strings.chars_ptr;
      Flags_Len                : stddef_h.size_t;
      Runtime_Ver              : unsigned;
      Split_Name               : Interfaces.C.Strings.chars_ptr;
      Split_Name_Len           : stddef_h.size_t;
      Kind                     : DWARF_Emission_Kind_T;
      DWO_Id                   : unsigned;
      Split_Debug_Inlining     : LLVM.Types.Bool_T;
      Debug_Info_For_Profiling : LLVM.Types.Bool_T;
      Sys_Root                 : Interfaces.C.Strings.chars_ptr;
      Sys_Root_Len             : stddef_h.size_t;
      SDK                      : Interfaces.C.Strings.chars_ptr;
      SDK_Len                  : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateCompileUnit";
   function DI_Create_Compile_Unit
     (Builder                  : LLVM.Types.DI_Builder_T;
      Lang                     : DWARF_Source_Language_T;
      File_Ref                 : LLVM.Types.Metadata_T;
      Producer                 : String;
      Producer_Len             : stddef_h.size_t;
      Is_Optimized             : LLVM.Types.Bool_T;
      Flags                    : String;
      Flags_Len                : stddef_h.size_t;
      Runtime_Ver              : unsigned;
      Split_Name               : String;
      Split_Name_Len           : stddef_h.size_t;
      Kind                     : DWARF_Emission_Kind_T;
      DWO_Id                   : unsigned;
      Split_Debug_Inlining     : LLVM.Types.Bool_T;
      Debug_Info_For_Profiling : LLVM.Types.Bool_T;
      Sys_Root                 : String;
      Sys_Root_Len             : stddef_h.size_t;
      SDK                      : String;
      SDK_Len                  : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value      : LLVM.Types.Metadata_T;
      Producer_Array    : aliased char_array := To_C (Producer);
      Producer_String   : constant chars_ptr := To_Chars_Ptr (Producer_Array'Unchecked_Access);
      Flags_Array       : aliased char_array := To_C (Flags);
      Flags_String      : constant chars_ptr := To_Chars_Ptr (Flags_Array'Unchecked_Access);
      Split_Name_Array  : aliased char_array := To_C (Split_Name);
      Split_Name_String : constant chars_ptr := To_Chars_Ptr (Split_Name_Array'Unchecked_Access);
      Sys_Root_Array    : aliased char_array := To_C (Sys_Root);
      Sys_Root_String   : constant chars_ptr := To_Chars_Ptr (Sys_Root_Array'Unchecked_Access);
      SDK_Array         : aliased char_array := To_C (SDK);
      SDK_String        : constant chars_ptr := To_Chars_Ptr (SDK_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Compile_Unit (Builder, Lang, File_Ref, Producer_String, Producer_Len, Is_Optimized, Flags_String, Flags_Len, Runtime_Ver, Split_Name_String, Split_Name_Len, Kind, DWO_Id, Split_Debug_Inlining, Debug_Info_For_Profiling, Sys_Root_String, Sys_Root_Len, SDK_String, SDK_Len);
      return Return_Value;
   end DI_Create_Compile_Unit;

   function DI_Create_Compile_Unit
     (Builder                  : LLVM.Types.DI_Builder_T;
      Lang                     : DWARF_Source_Language_T;
      File_Ref                 : LLVM.Types.Metadata_T;
      Producer                 : String;
      Producer_Len             : stddef_h.size_t;
      Is_Optimized             : Boolean;
      Flags                    : String;
      Flags_Len                : stddef_h.size_t;
      Runtime_Ver              : unsigned;
      Split_Name               : String;
      Split_Name_Len           : stddef_h.size_t;
      Kind                     : DWARF_Emission_Kind_T;
      DWO_Id                   : unsigned;
      Split_Debug_Inlining     : Boolean;
      Debug_Info_For_Profiling : Boolean;
      Sys_Root                 : String;
      Sys_Root_Len             : stddef_h.size_t;
      SDK                      : String;
      SDK_Len                  : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value                  : LLVM.Types.Metadata_T;
      Is_Optimized_Bool             : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Optimized);
      Split_Debug_Inlining_Bool     : constant LLVM.Types.Bool_T := Boolean'Pos (Split_Debug_Inlining);
      Debug_Info_For_Profiling_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Debug_Info_For_Profiling);
   begin
      Return_Value := DI_Create_Compile_Unit (Builder, Lang, File_Ref, Producer, Producer_Len, Is_Optimized_Bool, Flags, Flags_Len, Runtime_Ver, Split_Name, Split_Name_Len, Kind, DWO_Id, Split_Debug_Inlining_Bool, Debug_Info_For_Profiling_Bool, Sys_Root, Sys_Root_Len, SDK, SDK_Len);
      return Return_Value;
   end DI_Create_Compile_Unit;

   function DI_Builder_Create_File
     (Builder       : LLVM.Types.DI_Builder_T;
      Filename      : Interfaces.C.Strings.chars_ptr;
      Filename_Len  : stddef_h.size_t;
      Directory     : Interfaces.C.Strings.chars_ptr;
      Directory_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateFile";
   function DI_Create_File
     (Builder       : LLVM.Types.DI_Builder_T;
      Filename      : String;
      Filename_Len  : stddef_h.size_t;
      Directory     : String;
      Directory_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value     : LLVM.Types.Metadata_T;
      Filename_Array   : aliased char_array := To_C (Filename);
      Filename_String  : constant chars_ptr := To_Chars_Ptr (Filename_Array'Unchecked_Access);
      Directory_Array  : aliased char_array := To_C (Directory);
      Directory_String : constant chars_ptr := To_Chars_Ptr (Directory_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_File (Builder, Filename_String, Filename_Len, Directory_String, Directory_Len);
      return Return_Value;
   end DI_Create_File;

   function DI_Builder_Create_Module
     (Builder            : LLVM.Types.DI_Builder_T;
      Parent_Scope       : LLVM.Types.Metadata_T;
      Name               : Interfaces.C.Strings.chars_ptr;
      Name_Len           : stddef_h.size_t;
      Config_Macros      : Interfaces.C.Strings.chars_ptr;
      Config_Macros_Len  : stddef_h.size_t;
      Include_Path       : Interfaces.C.Strings.chars_ptr;
      Include_Path_Len   : stddef_h.size_t;
      API_Notes_File     : Interfaces.C.Strings.chars_ptr;
      API_Notes_File_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateModule";
   function DI_Create_Module
     (Builder            : LLVM.Types.DI_Builder_T;
      Parent_Scope       : LLVM.Types.Metadata_T;
      Name               : String;
      Name_Len           : stddef_h.size_t;
      Config_Macros      : String;
      Config_Macros_Len  : stddef_h.size_t;
      Include_Path       : String;
      Include_Path_Len   : stddef_h.size_t;
      API_Notes_File     : String;
      API_Notes_File_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value          : LLVM.Types.Metadata_T;
      Name_Array            : aliased char_array := To_C (Name);
      Name_String           : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Config_Macros_Array   : aliased char_array := To_C (Config_Macros);
      Config_Macros_String  : constant chars_ptr := To_Chars_Ptr (Config_Macros_Array'Unchecked_Access);
      Include_Path_Array    : aliased char_array := To_C (Include_Path);
      Include_Path_String   : constant chars_ptr := To_Chars_Ptr (Include_Path_Array'Unchecked_Access);
      API_Notes_File_Array  : aliased char_array := To_C (API_Notes_File);
      API_Notes_File_String : constant chars_ptr := To_Chars_Ptr (API_Notes_File_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Module (Builder, Parent_Scope, Name_String, Name_Len, Config_Macros_String, Config_Macros_Len, Include_Path_String, Include_Path_Len, API_Notes_File_String, API_Notes_File_Len);
      return Return_Value;
   end DI_Create_Module;

   function DI_Builder_Create_Name_Space
     (Builder        : LLVM.Types.DI_Builder_T;
      Parent_Scope   : LLVM.Types.Metadata_T;
      Name           : Interfaces.C.Strings.chars_ptr;
      Name_Len       : stddef_h.size_t;
      Export_Symbols : LLVM.Types.Bool_T)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateNameSpace";
   function DI_Create_Name_Space
     (Builder        : LLVM.Types.DI_Builder_T;
      Parent_Scope   : LLVM.Types.Metadata_T;
      Name           : String;
      Name_Len       : stddef_h.size_t;
      Export_Symbols : LLVM.Types.Bool_T)
      return LLVM.Types.Metadata_T
   is
      Return_Value : LLVM.Types.Metadata_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Name_Space (Builder, Parent_Scope, Name_String, Name_Len, Export_Symbols);
      return Return_Value;
   end DI_Create_Name_Space;

   function DI_Create_Name_Space
     (Builder        : LLVM.Types.DI_Builder_T;
      Parent_Scope   : LLVM.Types.Metadata_T;
      Name           : String;
      Name_Len       : stddef_h.size_t;
      Export_Symbols : Boolean)
      return LLVM.Types.Metadata_T
   is
      Return_Value        : LLVM.Types.Metadata_T;
      Export_Symbols_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Export_Symbols);
   begin
      Return_Value := DI_Create_Name_Space (Builder, Parent_Scope, Name, Name_Len, Export_Symbols_Bool);
      return Return_Value;
   end DI_Create_Name_Space;

   function DI_Builder_Create_Function
     (Builder          : LLVM.Types.DI_Builder_T;
      Scope            : LLVM.Types.Metadata_T;
      Name             : Interfaces.C.Strings.chars_ptr;
      Name_Len         : stddef_h.size_t;
      Linkage_Name     : Interfaces.C.Strings.chars_ptr;
      Linkage_Name_Len : stddef_h.size_t;
      File             : LLVM.Types.Metadata_T;
      Line_No          : unsigned;
      Ty               : LLVM.Types.Metadata_T;
      Is_Local_To_Unit : LLVM.Types.Bool_T;
      Is_Definition    : LLVM.Types.Bool_T;
      Scope_Line       : unsigned;
      Flags            : DI_Flags_T;
      Is_Optimized     : LLVM.Types.Bool_T)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateFunction";
   function DI_Create_Function
     (Builder          : LLVM.Types.DI_Builder_T;
      Scope            : LLVM.Types.Metadata_T;
      Name             : String;
      Name_Len         : stddef_h.size_t;
      Linkage_Name     : String;
      Linkage_Name_Len : stddef_h.size_t;
      File             : LLVM.Types.Metadata_T;
      Line_No          : unsigned;
      Ty               : LLVM.Types.Metadata_T;
      Is_Local_To_Unit : LLVM.Types.Bool_T;
      Is_Definition    : LLVM.Types.Bool_T;
      Scope_Line       : unsigned;
      Flags            : DI_Flags_T;
      Is_Optimized     : LLVM.Types.Bool_T)
      return LLVM.Types.Metadata_T
   is
      Return_Value        : LLVM.Types.Metadata_T;
      Name_Array          : aliased char_array := To_C (Name);
      Name_String         : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Linkage_Name_Array  : aliased char_array := To_C (Linkage_Name);
      Linkage_Name_String : constant chars_ptr := To_Chars_Ptr (Linkage_Name_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Function (Builder, Scope, Name_String, Name_Len, Linkage_Name_String, Linkage_Name_Len, File, Line_No, Ty, Is_Local_To_Unit, Is_Definition, Scope_Line, Flags, Is_Optimized);
      return Return_Value;
   end DI_Create_Function;

   function DI_Create_Function
     (Builder          : LLVM.Types.DI_Builder_T;
      Scope            : LLVM.Types.Metadata_T;
      Name             : String;
      Name_Len         : stddef_h.size_t;
      Linkage_Name     : String;
      Linkage_Name_Len : stddef_h.size_t;
      File             : LLVM.Types.Metadata_T;
      Line_No          : unsigned;
      Ty               : LLVM.Types.Metadata_T;
      Is_Local_To_Unit : Boolean;
      Is_Definition    : Boolean;
      Scope_Line       : unsigned;
      Flags            : DI_Flags_T;
      Is_Optimized     : Boolean)
      return LLVM.Types.Metadata_T
   is
      Return_Value          : LLVM.Types.Metadata_T;
      Is_Local_To_Unit_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Local_To_Unit);
      Is_Definition_Bool    : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Definition);
      Is_Optimized_Bool     : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Optimized);
   begin
      Return_Value := DI_Create_Function (Builder, Scope, Name, Name_Len, Linkage_Name, Linkage_Name_Len, File, Line_No, Ty, Is_Local_To_Unit_Bool, Is_Definition_Bool, Scope_Line, Flags, Is_Optimized_Bool);
      return Return_Value;
   end DI_Create_Function;

   function DI_Builder_Create_Imported_Declaration
     (Builder      : LLVM.Types.DI_Builder_T;
      Scope        : LLVM.Types.Metadata_T;
      Decl         : LLVM.Types.Metadata_T;
      File         : LLVM.Types.Metadata_T;
      Line         : unsigned;
      Name         : Interfaces.C.Strings.chars_ptr;
      Name_Len     : stddef_h.size_t;
      Elements     : System.Address;
      Num_Elements : unsigned)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateImportedDeclaration";
   function DI_Create_Imported_Declaration
     (Builder      : LLVM.Types.DI_Builder_T;
      Scope        : LLVM.Types.Metadata_T;
      Decl         : LLVM.Types.Metadata_T;
      File         : LLVM.Types.Metadata_T;
      Line         : unsigned;
      Name         : String;
      Name_Len     : stddef_h.size_t;
      Elements     : System.Address;
      Num_Elements : unsigned)
      return LLVM.Types.Metadata_T
   is
      Return_Value : LLVM.Types.Metadata_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Imported_Declaration (Builder, Scope, Decl, File, Line, Name_String, Name_Len, Elements, Num_Elements);
      return Return_Value;
   end DI_Create_Imported_Declaration;

   function DI_File_Get_Directory
     (File : LLVM.Types.Metadata_T;
      Len  : access unsigned)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIFileGetDirectory";
   function DI_File_Get_Directory
     (File : LLVM.Types.Metadata_T;
      Len  : access unsigned)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := DI_File_Get_Directory (File, Len);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end DI_File_Get_Directory;

   function DI_File_Get_Filename
     (File : LLVM.Types.Metadata_T;
      Len  : access unsigned)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIFileGetFilename";
   function DI_File_Get_Filename
     (File : LLVM.Types.Metadata_T;
      Len  : access unsigned)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := DI_File_Get_Filename (File, Len);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end DI_File_Get_Filename;

   function DI_File_Get_Source
     (File : LLVM.Types.Metadata_T;
      Len  : access unsigned)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIFileGetSource";
   function DI_File_Get_Source
     (File : LLVM.Types.Metadata_T;
      Len  : access unsigned)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := DI_File_Get_Source (File, Len);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end DI_File_Get_Source;

   function DI_Builder_Create_Macro
     (Builder           : LLVM.Types.DI_Builder_T;
      Parent_Macro_File : LLVM.Types.Metadata_T;
      Line              : unsigned;
      Record_Type       : DWARF_Macinfo_Record_Type_T;
      Name              : Interfaces.C.Strings.chars_ptr;
      Name_Len          : stddef_h.size_t;
      Value             : Interfaces.C.Strings.chars_ptr;
      Value_Len         : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateMacro";
   function DI_Create_Macro
     (Builder           : LLVM.Types.DI_Builder_T;
      Parent_Macro_File : LLVM.Types.Metadata_T;
      Line              : unsigned;
      Record_Type       : DWARF_Macinfo_Record_Type_T;
      Name              : String;
      Name_Len          : stddef_h.size_t;
      Value             : String;
      Value_Len         : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value : LLVM.Types.Metadata_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Value_Array  : aliased char_array := To_C (Value);
      Value_String : constant chars_ptr := To_Chars_Ptr (Value_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Macro (Builder, Parent_Macro_File, Line, Record_Type, Name_String, Name_Len, Value_String, Value_Len);
      return Return_Value;
   end DI_Create_Macro;

   function DI_Builder_Create_Enumerator
     (Builder     : LLVM.Types.DI_Builder_T;
      Name        : Interfaces.C.Strings.chars_ptr;
      Name_Len    : stddef_h.size_t;
      Value       : stdint_h.int64_t;
      Is_Unsigned : LLVM.Types.Bool_T)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateEnumerator";
   function DI_Create_Enumerator
     (Builder     : LLVM.Types.DI_Builder_T;
      Name        : String;
      Name_Len    : stddef_h.size_t;
      Value       : stdint_h.int64_t;
      Is_Unsigned : LLVM.Types.Bool_T)
      return LLVM.Types.Metadata_T
   is
      Return_Value : LLVM.Types.Metadata_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Enumerator (Builder, Name_String, Name_Len, Value, Is_Unsigned);
      return Return_Value;
   end DI_Create_Enumerator;

   function DI_Create_Enumerator
     (Builder     : LLVM.Types.DI_Builder_T;
      Name        : String;
      Name_Len    : stddef_h.size_t;
      Value       : stdint_h.int64_t;
      Is_Unsigned : Boolean)
      return LLVM.Types.Metadata_T
   is
      Return_Value     : LLVM.Types.Metadata_T;
      Is_Unsigned_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Unsigned);
   begin
      Return_Value := DI_Create_Enumerator (Builder, Name, Name_Len, Value, Is_Unsigned_Bool);
      return Return_Value;
   end DI_Create_Enumerator;

   function DI_Builder_Create_Enumeration_Type
     (Builder       : LLVM.Types.DI_Builder_T;
      Scope         : LLVM.Types.Metadata_T;
      Name          : Interfaces.C.Strings.chars_ptr;
      Name_Len      : stddef_h.size_t;
      File          : LLVM.Types.Metadata_T;
      Line_Number   : unsigned;
      Size_In_Bits  : stdint_h.uint64_t;
      Align_In_Bits : stdint_h.uint32_t;
      Elements      : System.Address;
      Num_Elements  : unsigned;
      Class_Ty      : LLVM.Types.Metadata_T)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateEnumerationType";
   function DI_Create_Enumeration_Type
     (Builder       : LLVM.Types.DI_Builder_T;
      Scope         : LLVM.Types.Metadata_T;
      Name          : String;
      Name_Len      : stddef_h.size_t;
      File          : LLVM.Types.Metadata_T;
      Line_Number   : unsigned;
      Size_In_Bits  : stdint_h.uint64_t;
      Align_In_Bits : stdint_h.uint32_t;
      Elements      : System.Address;
      Num_Elements  : unsigned;
      Class_Ty      : LLVM.Types.Metadata_T)
      return LLVM.Types.Metadata_T
   is
      Return_Value : LLVM.Types.Metadata_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Enumeration_Type (Builder, Scope, Name_String, Name_Len, File, Line_Number, Size_In_Bits, Align_In_Bits, Elements, Num_Elements, Class_Ty);
      return Return_Value;
   end DI_Create_Enumeration_Type;

   function DI_Builder_Create_Union_Type
     (Builder       : LLVM.Types.DI_Builder_T;
      Scope         : LLVM.Types.Metadata_T;
      Name          : Interfaces.C.Strings.chars_ptr;
      Name_Len      : stddef_h.size_t;
      File          : LLVM.Types.Metadata_T;
      Line_Number   : unsigned;
      Size_In_Bits  : stdint_h.uint64_t;
      Align_In_Bits : stdint_h.uint32_t;
      Flags         : DI_Flags_T;
      Elements      : System.Address;
      Num_Elements  : unsigned;
      Run_Time_Lang : unsigned;
      Unique_Id     : Interfaces.C.Strings.chars_ptr;
      Unique_Id_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateUnionType";
   function DI_Create_Union_Type
     (Builder       : LLVM.Types.DI_Builder_T;
      Scope         : LLVM.Types.Metadata_T;
      Name          : String;
      Name_Len      : stddef_h.size_t;
      File          : LLVM.Types.Metadata_T;
      Line_Number   : unsigned;
      Size_In_Bits  : stdint_h.uint64_t;
      Align_In_Bits : stdint_h.uint32_t;
      Flags         : DI_Flags_T;
      Elements      : System.Address;
      Num_Elements  : unsigned;
      Run_Time_Lang : unsigned;
      Unique_Id     : String;
      Unique_Id_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value     : LLVM.Types.Metadata_T;
      Name_Array       : aliased char_array := To_C (Name);
      Name_String      : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Unique_Id_Array  : aliased char_array := To_C (Unique_Id);
      Unique_Id_String : constant chars_ptr := To_Chars_Ptr (Unique_Id_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Union_Type (Builder, Scope, Name_String, Name_Len, File, Line_Number, Size_In_Bits, Align_In_Bits, Flags, Elements, Num_Elements, Run_Time_Lang, Unique_Id_String, Unique_Id_Len);
      return Return_Value;
   end DI_Create_Union_Type;

   function DI_Builder_Create_Unspecified_Type
     (Builder  : LLVM.Types.DI_Builder_T;
      Name     : Interfaces.C.Strings.chars_ptr;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateUnspecifiedType";
   function DI_Create_Unspecified_Type
     (Builder  : LLVM.Types.DI_Builder_T;
      Name     : String;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value : LLVM.Types.Metadata_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Unspecified_Type (Builder, Name_String, Name_Len);
      return Return_Value;
   end DI_Create_Unspecified_Type;

   function DI_Builder_Create_Basic_Type
     (Builder      : LLVM.Types.DI_Builder_T;
      Name         : Interfaces.C.Strings.chars_ptr;
      Name_Len     : stddef_h.size_t;
      Size_In_Bits : stdint_h.uint64_t;
      Encoding     : DWARF_Type_Encoding_T;
      Flags        : DI_Flags_T)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateBasicType";
   function DI_Create_Basic_Type
     (Builder      : LLVM.Types.DI_Builder_T;
      Name         : String;
      Name_Len     : stddef_h.size_t;
      Size_In_Bits : stdint_h.uint64_t;
      Encoding     : DWARF_Type_Encoding_T;
      Flags        : DI_Flags_T)
      return LLVM.Types.Metadata_T
   is
      Return_Value : LLVM.Types.Metadata_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Basic_Type (Builder, Name_String, Name_Len, Size_In_Bits, Encoding, Flags);
      return Return_Value;
   end DI_Create_Basic_Type;

   function DI_Builder_Create_Pointer_Type
     (Builder       : LLVM.Types.DI_Builder_T;
      Pointee_Ty    : LLVM.Types.Metadata_T;
      Size_In_Bits  : stdint_h.uint64_t;
      Align_In_Bits : stdint_h.uint32_t;
      Address_Space : unsigned;
      Name          : Interfaces.C.Strings.chars_ptr;
      Name_Len      : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreatePointerType";
   function DI_Create_Pointer_Type
     (Builder       : LLVM.Types.DI_Builder_T;
      Pointee_Ty    : LLVM.Types.Metadata_T;
      Size_In_Bits  : stdint_h.uint64_t;
      Align_In_Bits : stdint_h.uint32_t;
      Address_Space : unsigned;
      Name          : String;
      Name_Len      : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value : LLVM.Types.Metadata_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Pointer_Type (Builder, Pointee_Ty, Size_In_Bits, Align_In_Bits, Address_Space, Name_String, Name_Len);
      return Return_Value;
   end DI_Create_Pointer_Type;

   function DI_Builder_Create_Struct_Type
     (Builder        : LLVM.Types.DI_Builder_T;
      Scope          : LLVM.Types.Metadata_T;
      Name           : Interfaces.C.Strings.chars_ptr;
      Name_Len       : stddef_h.size_t;
      File           : LLVM.Types.Metadata_T;
      Line_Number    : unsigned;
      Size_In_Bits   : stdint_h.uint64_t;
      Align_In_Bits  : stdint_h.uint32_t;
      Flags          : DI_Flags_T;
      Derived_From   : LLVM.Types.Metadata_T;
      Elements       : System.Address;
      Num_Elements   : unsigned;
      Run_Time_Lang  : unsigned;
      V_Table_Holder : LLVM.Types.Metadata_T;
      Unique_Id      : Interfaces.C.Strings.chars_ptr;
      Unique_Id_Len  : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateStructType";
   function DI_Create_Struct_Type
     (Builder        : LLVM.Types.DI_Builder_T;
      Scope          : LLVM.Types.Metadata_T;
      Name           : String;
      Name_Len       : stddef_h.size_t;
      File           : LLVM.Types.Metadata_T;
      Line_Number    : unsigned;
      Size_In_Bits   : stdint_h.uint64_t;
      Align_In_Bits  : stdint_h.uint32_t;
      Flags          : DI_Flags_T;
      Derived_From   : LLVM.Types.Metadata_T;
      Elements       : System.Address;
      Num_Elements   : unsigned;
      Run_Time_Lang  : unsigned;
      V_Table_Holder : LLVM.Types.Metadata_T;
      Unique_Id      : String;
      Unique_Id_Len  : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value     : LLVM.Types.Metadata_T;
      Name_Array       : aliased char_array := To_C (Name);
      Name_String      : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Unique_Id_Array  : aliased char_array := To_C (Unique_Id);
      Unique_Id_String : constant chars_ptr := To_Chars_Ptr (Unique_Id_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Struct_Type (Builder, Scope, Name_String, Name_Len, File, Line_Number, Size_In_Bits, Align_In_Bits, Flags, Derived_From, Elements, Num_Elements, Run_Time_Lang, V_Table_Holder, Unique_Id_String, Unique_Id_Len);
      return Return_Value;
   end DI_Create_Struct_Type;

   function DI_Builder_Create_Member_Type
     (Builder        : LLVM.Types.DI_Builder_T;
      Scope          : LLVM.Types.Metadata_T;
      Name           : Interfaces.C.Strings.chars_ptr;
      Name_Len       : stddef_h.size_t;
      File           : LLVM.Types.Metadata_T;
      Line_No        : unsigned;
      Size_In_Bits   : stdint_h.uint64_t;
      Align_In_Bits  : stdint_h.uint32_t;
      Offset_In_Bits : stdint_h.uint64_t;
      Flags          : DI_Flags_T;
      Ty             : LLVM.Types.Metadata_T)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateMemberType";
   function DI_Create_Member_Type
     (Builder        : LLVM.Types.DI_Builder_T;
      Scope          : LLVM.Types.Metadata_T;
      Name           : String;
      Name_Len       : stddef_h.size_t;
      File           : LLVM.Types.Metadata_T;
      Line_No        : unsigned;
      Size_In_Bits   : stdint_h.uint64_t;
      Align_In_Bits  : stdint_h.uint32_t;
      Offset_In_Bits : stdint_h.uint64_t;
      Flags          : DI_Flags_T;
      Ty             : LLVM.Types.Metadata_T)
      return LLVM.Types.Metadata_T
   is
      Return_Value : LLVM.Types.Metadata_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Member_Type (Builder, Scope, Name_String, Name_Len, File, Line_No, Size_In_Bits, Align_In_Bits, Offset_In_Bits, Flags, Ty);
      return Return_Value;
   end DI_Create_Member_Type;

   function DI_Builder_Create_Static_Member_Type
     (Builder       : LLVM.Types.DI_Builder_T;
      Scope         : LLVM.Types.Metadata_T;
      Name          : Interfaces.C.Strings.chars_ptr;
      Name_Len      : stddef_h.size_t;
      File          : LLVM.Types.Metadata_T;
      Line_Number   : unsigned;
      C_Type        : LLVM.Types.Metadata_T;
      Flags         : DI_Flags_T;
      Constant_Val  : LLVM.Types.Value_T;
      Align_In_Bits : stdint_h.uint32_t)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateStaticMemberType";
   function DI_Create_Static_Member_Type
     (Builder       : LLVM.Types.DI_Builder_T;
      Scope         : LLVM.Types.Metadata_T;
      Name          : String;
      Name_Len      : stddef_h.size_t;
      File          : LLVM.Types.Metadata_T;
      Line_Number   : unsigned;
      C_Type        : LLVM.Types.Metadata_T;
      Flags         : DI_Flags_T;
      Constant_Val  : LLVM.Types.Value_T;
      Align_In_Bits : stdint_h.uint32_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value : LLVM.Types.Metadata_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Static_Member_Type (Builder, Scope, Name_String, Name_Len, File, Line_Number, C_Type, Flags, Constant_Val, Align_In_Bits);
      return Return_Value;
   end DI_Create_Static_Member_Type;

   function DI_Builder_Create_Obj_CI_Var
     (Builder        : LLVM.Types.DI_Builder_T;
      Name           : Interfaces.C.Strings.chars_ptr;
      Name_Len       : stddef_h.size_t;
      File           : LLVM.Types.Metadata_T;
      Line_No        : unsigned;
      Size_In_Bits   : stdint_h.uint64_t;
      Align_In_Bits  : stdint_h.uint32_t;
      Offset_In_Bits : stdint_h.uint64_t;
      Flags          : DI_Flags_T;
      Ty             : LLVM.Types.Metadata_T;
      Property_Node  : LLVM.Types.Metadata_T)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateObjCIVar";
   function DI_Create_Obj_CI_Var
     (Builder        : LLVM.Types.DI_Builder_T;
      Name           : String;
      Name_Len       : stddef_h.size_t;
      File           : LLVM.Types.Metadata_T;
      Line_No        : unsigned;
      Size_In_Bits   : stdint_h.uint64_t;
      Align_In_Bits  : stdint_h.uint32_t;
      Offset_In_Bits : stdint_h.uint64_t;
      Flags          : DI_Flags_T;
      Ty             : LLVM.Types.Metadata_T;
      Property_Node  : LLVM.Types.Metadata_T)
      return LLVM.Types.Metadata_T
   is
      Return_Value : LLVM.Types.Metadata_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Obj_CI_Var (Builder, Name_String, Name_Len, File, Line_No, Size_In_Bits, Align_In_Bits, Offset_In_Bits, Flags, Ty, Property_Node);
      return Return_Value;
   end DI_Create_Obj_CI_Var;

   function DI_Builder_Create_Obj_C_Property
     (Builder             : LLVM.Types.DI_Builder_T;
      Name                : Interfaces.C.Strings.chars_ptr;
      Name_Len            : stddef_h.size_t;
      File                : LLVM.Types.Metadata_T;
      Line_No             : unsigned;
      Getter_Name         : Interfaces.C.Strings.chars_ptr;
      Getter_Name_Len     : stddef_h.size_t;
      Setter_Name         : Interfaces.C.Strings.chars_ptr;
      Setter_Name_Len     : stddef_h.size_t;
      Property_Attributes : unsigned;
      Ty                  : LLVM.Types.Metadata_T)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateObjCProperty";
   function DI_Create_Obj_C_Property
     (Builder             : LLVM.Types.DI_Builder_T;
      Name                : String;
      Name_Len            : stddef_h.size_t;
      File                : LLVM.Types.Metadata_T;
      Line_No             : unsigned;
      Getter_Name         : String;
      Getter_Name_Len     : stddef_h.size_t;
      Setter_Name         : String;
      Setter_Name_Len     : stddef_h.size_t;
      Property_Attributes : unsigned;
      Ty                  : LLVM.Types.Metadata_T)
      return LLVM.Types.Metadata_T
   is
      Return_Value       : LLVM.Types.Metadata_T;
      Name_Array         : aliased char_array := To_C (Name);
      Name_String        : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Getter_Name_Array  : aliased char_array := To_C (Getter_Name);
      Getter_Name_String : constant chars_ptr := To_Chars_Ptr (Getter_Name_Array'Unchecked_Access);
      Setter_Name_Array  : aliased char_array := To_C (Setter_Name);
      Setter_Name_String : constant chars_ptr := To_Chars_Ptr (Setter_Name_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Obj_C_Property (Builder, Name_String, Name_Len, File, Line_No, Getter_Name_String, Getter_Name_Len, Setter_Name_String, Setter_Name_Len, Property_Attributes, Ty);
      return Return_Value;
   end DI_Create_Obj_C_Property;

   function DI_Builder_Create_Typedef
     (Builder       : LLVM.Types.DI_Builder_T;
      C_Type        : LLVM.Types.Metadata_T;
      Name          : Interfaces.C.Strings.chars_ptr;
      Name_Len      : stddef_h.size_t;
      File          : LLVM.Types.Metadata_T;
      Line_No       : unsigned;
      Scope         : LLVM.Types.Metadata_T;
      Align_In_Bits : stdint_h.uint32_t)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateTypedef";
   function DI_Create_Typedef
     (Builder       : LLVM.Types.DI_Builder_T;
      C_Type        : LLVM.Types.Metadata_T;
      Name          : String;
      Name_Len      : stddef_h.size_t;
      File          : LLVM.Types.Metadata_T;
      Line_No       : unsigned;
      Scope         : LLVM.Types.Metadata_T;
      Align_In_Bits : stdint_h.uint32_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value : LLVM.Types.Metadata_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Typedef (Builder, C_Type, Name_String, Name_Len, File, Line_No, Scope, Align_In_Bits);
      return Return_Value;
   end DI_Create_Typedef;

   function DI_Builder_Create_Forward_Decl
     (Builder               : LLVM.Types.DI_Builder_T;
      Tag                   : unsigned;
      Name                  : Interfaces.C.Strings.chars_ptr;
      Name_Len              : stddef_h.size_t;
      Scope                 : LLVM.Types.Metadata_T;
      File                  : LLVM.Types.Metadata_T;
      Line                  : unsigned;
      Runtime_Lang          : unsigned;
      Size_In_Bits          : stdint_h.uint64_t;
      Align_In_Bits         : stdint_h.uint32_t;
      Unique_Identifier     : Interfaces.C.Strings.chars_ptr;
      Unique_Identifier_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateForwardDecl";
   function DI_Create_Forward_Decl
     (Builder               : LLVM.Types.DI_Builder_T;
      Tag                   : unsigned;
      Name                  : String;
      Name_Len              : stddef_h.size_t;
      Scope                 : LLVM.Types.Metadata_T;
      File                  : LLVM.Types.Metadata_T;
      Line                  : unsigned;
      Runtime_Lang          : unsigned;
      Size_In_Bits          : stdint_h.uint64_t;
      Align_In_Bits         : stdint_h.uint32_t;
      Unique_Identifier     : String;
      Unique_Identifier_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value             : LLVM.Types.Metadata_T;
      Name_Array               : aliased char_array := To_C (Name);
      Name_String              : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Unique_Identifier_Array  : aliased char_array := To_C (Unique_Identifier);
      Unique_Identifier_String : constant chars_ptr := To_Chars_Ptr (Unique_Identifier_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Forward_Decl (Builder, Tag, Name_String, Name_Len, Scope, File, Line, Runtime_Lang, Size_In_Bits, Align_In_Bits, Unique_Identifier_String, Unique_Identifier_Len);
      return Return_Value;
   end DI_Create_Forward_Decl;

   function DI_Builder_Create_Replaceable_Composite_Type
     (Builder               : LLVM.Types.DI_Builder_T;
      Tag                   : unsigned;
      Name                  : Interfaces.C.Strings.chars_ptr;
      Name_Len              : stddef_h.size_t;
      Scope                 : LLVM.Types.Metadata_T;
      File                  : LLVM.Types.Metadata_T;
      Line                  : unsigned;
      Runtime_Lang          : unsigned;
      Size_In_Bits          : stdint_h.uint64_t;
      Align_In_Bits         : stdint_h.uint32_t;
      Flags                 : DI_Flags_T;
      Unique_Identifier     : Interfaces.C.Strings.chars_ptr;
      Unique_Identifier_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateReplaceableCompositeType";
   function DI_Create_Replaceable_Composite_Type
     (Builder               : LLVM.Types.DI_Builder_T;
      Tag                   : unsigned;
      Name                  : String;
      Name_Len              : stddef_h.size_t;
      Scope                 : LLVM.Types.Metadata_T;
      File                  : LLVM.Types.Metadata_T;
      Line                  : unsigned;
      Runtime_Lang          : unsigned;
      Size_In_Bits          : stdint_h.uint64_t;
      Align_In_Bits         : stdint_h.uint32_t;
      Flags                 : DI_Flags_T;
      Unique_Identifier     : String;
      Unique_Identifier_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value             : LLVM.Types.Metadata_T;
      Name_Array               : aliased char_array := To_C (Name);
      Name_String              : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Unique_Identifier_Array  : aliased char_array := To_C (Unique_Identifier);
      Unique_Identifier_String : constant chars_ptr := To_Chars_Ptr (Unique_Identifier_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Replaceable_Composite_Type (Builder, Tag, Name_String, Name_Len, Scope, File, Line, Runtime_Lang, Size_In_Bits, Align_In_Bits, Flags, Unique_Identifier_String, Unique_Identifier_Len);
      return Return_Value;
   end DI_Create_Replaceable_Composite_Type;

   function DI_Builder_Create_Bit_Field_Member_Type
     (Builder                : LLVM.Types.DI_Builder_T;
      Scope                  : LLVM.Types.Metadata_T;
      Name                   : Interfaces.C.Strings.chars_ptr;
      Name_Len               : stddef_h.size_t;
      File                   : LLVM.Types.Metadata_T;
      Line_Number            : unsigned;
      Size_In_Bits           : stdint_h.uint64_t;
      Offset_In_Bits         : stdint_h.uint64_t;
      Storage_Offset_In_Bits : stdint_h.uint64_t;
      Flags                  : DI_Flags_T;
      C_Type                 : LLVM.Types.Metadata_T)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateBitFieldMemberType";
   function DI_Create_Bit_Field_Member_Type
     (Builder                : LLVM.Types.DI_Builder_T;
      Scope                  : LLVM.Types.Metadata_T;
      Name                   : String;
      Name_Len               : stddef_h.size_t;
      File                   : LLVM.Types.Metadata_T;
      Line_Number            : unsigned;
      Size_In_Bits           : stdint_h.uint64_t;
      Offset_In_Bits         : stdint_h.uint64_t;
      Storage_Offset_In_Bits : stdint_h.uint64_t;
      Flags                  : DI_Flags_T;
      C_Type                 : LLVM.Types.Metadata_T)
      return LLVM.Types.Metadata_T
   is
      Return_Value : LLVM.Types.Metadata_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Bit_Field_Member_Type (Builder, Scope, Name_String, Name_Len, File, Line_Number, Size_In_Bits, Offset_In_Bits, Storage_Offset_In_Bits, Flags, C_Type);
      return Return_Value;
   end DI_Create_Bit_Field_Member_Type;

   function DI_Builder_Create_Class_Type
     (Builder               : LLVM.Types.DI_Builder_T;
      Scope                 : LLVM.Types.Metadata_T;
      Name                  : Interfaces.C.Strings.chars_ptr;
      Name_Len              : stddef_h.size_t;
      File                  : LLVM.Types.Metadata_T;
      Line_Number           : unsigned;
      Size_In_Bits          : stdint_h.uint64_t;
      Align_In_Bits         : stdint_h.uint32_t;
      Offset_In_Bits        : stdint_h.uint64_t;
      Flags                 : DI_Flags_T;
      Derived_From          : LLVM.Types.Metadata_T;
      Elements              : System.Address;
      Num_Elements          : unsigned;
      V_Table_Holder        : LLVM.Types.Metadata_T;
      Template_Params_Node  : LLVM.Types.Metadata_T;
      Unique_Identifier     : Interfaces.C.Strings.chars_ptr;
      Unique_Identifier_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateClassType";
   function DI_Create_Class_Type
     (Builder               : LLVM.Types.DI_Builder_T;
      Scope                 : LLVM.Types.Metadata_T;
      Name                  : String;
      Name_Len              : stddef_h.size_t;
      File                  : LLVM.Types.Metadata_T;
      Line_Number           : unsigned;
      Size_In_Bits          : stdint_h.uint64_t;
      Align_In_Bits         : stdint_h.uint32_t;
      Offset_In_Bits        : stdint_h.uint64_t;
      Flags                 : DI_Flags_T;
      Derived_From          : LLVM.Types.Metadata_T;
      Elements              : System.Address;
      Num_Elements          : unsigned;
      V_Table_Holder        : LLVM.Types.Metadata_T;
      Template_Params_Node  : LLVM.Types.Metadata_T;
      Unique_Identifier     : String;
      Unique_Identifier_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value             : LLVM.Types.Metadata_T;
      Name_Array               : aliased char_array := To_C (Name);
      Name_String              : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Unique_Identifier_Array  : aliased char_array := To_C (Unique_Identifier);
      Unique_Identifier_String : constant chars_ptr := To_Chars_Ptr (Unique_Identifier_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Class_Type (Builder, Scope, Name_String, Name_Len, File, Line_Number, Size_In_Bits, Align_In_Bits, Offset_In_Bits, Flags, Derived_From, Elements, Num_Elements, V_Table_Holder, Template_Params_Node, Unique_Identifier_String, Unique_Identifier_Len);
      return Return_Value;
   end DI_Create_Class_Type;

   function DI_Type_Get_Name
     (D_Type : LLVM.Types.Metadata_T;
      Length : access stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMDITypeGetName";
   function DI_Type_Get_Name
     (D_Type : LLVM.Types.Metadata_T;
      Length : access stddef_h.size_t)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := DI_Type_Get_Name (D_Type, Length);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end DI_Type_Get_Name;

   function DI_Builder_Create_Global_Variable_Expression
     (Builder       : LLVM.Types.DI_Builder_T;
      Scope         : LLVM.Types.Metadata_T;
      Name          : Interfaces.C.Strings.chars_ptr;
      Name_Len      : stddef_h.size_t;
      Linkage       : Interfaces.C.Strings.chars_ptr;
      Link_Len      : stddef_h.size_t;
      File          : LLVM.Types.Metadata_T;
      Line_No       : unsigned;
      Ty            : LLVM.Types.Metadata_T;
      Local_To_Unit : LLVM.Types.Bool_T;
      Expr          : LLVM.Types.Metadata_T;
      Decl          : LLVM.Types.Metadata_T;
      Align_In_Bits : stdint_h.uint32_t)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateGlobalVariableExpression";
   function DI_Create_Global_Variable_Expression
     (Builder       : LLVM.Types.DI_Builder_T;
      Scope         : LLVM.Types.Metadata_T;
      Name          : String;
      Name_Len      : stddef_h.size_t;
      Linkage       : String;
      Link_Len      : stddef_h.size_t;
      File          : LLVM.Types.Metadata_T;
      Line_No       : unsigned;
      Ty            : LLVM.Types.Metadata_T;
      Local_To_Unit : LLVM.Types.Bool_T;
      Expr          : LLVM.Types.Metadata_T;
      Decl          : LLVM.Types.Metadata_T;
      Align_In_Bits : stdint_h.uint32_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value   : LLVM.Types.Metadata_T;
      Name_Array     : aliased char_array := To_C (Name);
      Name_String    : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Linkage_Array  : aliased char_array := To_C (Linkage);
      Linkage_String : constant chars_ptr := To_Chars_Ptr (Linkage_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Global_Variable_Expression (Builder, Scope, Name_String, Name_Len, Linkage_String, Link_Len, File, Line_No, Ty, Local_To_Unit, Expr, Decl, Align_In_Bits);
      return Return_Value;
   end DI_Create_Global_Variable_Expression;

   function DI_Create_Global_Variable_Expression
     (Builder       : LLVM.Types.DI_Builder_T;
      Scope         : LLVM.Types.Metadata_T;
      Name          : String;
      Name_Len      : stddef_h.size_t;
      Linkage       : String;
      Link_Len      : stddef_h.size_t;
      File          : LLVM.Types.Metadata_T;
      Line_No       : unsigned;
      Ty            : LLVM.Types.Metadata_T;
      Local_To_Unit : Boolean;
      Expr          : LLVM.Types.Metadata_T;
      Decl          : LLVM.Types.Metadata_T;
      Align_In_Bits : stdint_h.uint32_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value       : LLVM.Types.Metadata_T;
      Local_To_Unit_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Local_To_Unit);
   begin
      Return_Value := DI_Create_Global_Variable_Expression (Builder, Scope, Name, Name_Len, Linkage, Link_Len, File, Line_No, Ty, Local_To_Unit_Bool, Expr, Decl, Align_In_Bits);
      return Return_Value;
   end DI_Create_Global_Variable_Expression;

   function DI_Builder_Create_Temp_Global_Variable_Fwd_Decl
     (Builder       : LLVM.Types.DI_Builder_T;
      Scope         : LLVM.Types.Metadata_T;
      Name          : Interfaces.C.Strings.chars_ptr;
      Name_Len      : stddef_h.size_t;
      Linkage       : Interfaces.C.Strings.chars_ptr;
      Lnk_Len       : stddef_h.size_t;
      File          : LLVM.Types.Metadata_T;
      Line_No       : unsigned;
      Ty            : LLVM.Types.Metadata_T;
      Local_To_Unit : LLVM.Types.Bool_T;
      Decl          : LLVM.Types.Metadata_T;
      Align_In_Bits : stdint_h.uint32_t)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateTempGlobalVariableFwdDecl";
   function DI_Create_Temp_Global_Variable_Fwd_Decl
     (Builder       : LLVM.Types.DI_Builder_T;
      Scope         : LLVM.Types.Metadata_T;
      Name          : String;
      Name_Len      : stddef_h.size_t;
      Linkage       : String;
      Lnk_Len       : stddef_h.size_t;
      File          : LLVM.Types.Metadata_T;
      Line_No       : unsigned;
      Ty            : LLVM.Types.Metadata_T;
      Local_To_Unit : LLVM.Types.Bool_T;
      Decl          : LLVM.Types.Metadata_T;
      Align_In_Bits : stdint_h.uint32_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value   : LLVM.Types.Metadata_T;
      Name_Array     : aliased char_array := To_C (Name);
      Name_String    : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Linkage_Array  : aliased char_array := To_C (Linkage);
      Linkage_String : constant chars_ptr := To_Chars_Ptr (Linkage_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Temp_Global_Variable_Fwd_Decl (Builder, Scope, Name_String, Name_Len, Linkage_String, Lnk_Len, File, Line_No, Ty, Local_To_Unit, Decl, Align_In_Bits);
      return Return_Value;
   end DI_Create_Temp_Global_Variable_Fwd_Decl;

   function DI_Create_Temp_Global_Variable_Fwd_Decl
     (Builder       : LLVM.Types.DI_Builder_T;
      Scope         : LLVM.Types.Metadata_T;
      Name          : String;
      Name_Len      : stddef_h.size_t;
      Linkage       : String;
      Lnk_Len       : stddef_h.size_t;
      File          : LLVM.Types.Metadata_T;
      Line_No       : unsigned;
      Ty            : LLVM.Types.Metadata_T;
      Local_To_Unit : Boolean;
      Decl          : LLVM.Types.Metadata_T;
      Align_In_Bits : stdint_h.uint32_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value       : LLVM.Types.Metadata_T;
      Local_To_Unit_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Local_To_Unit);
   begin
      Return_Value := DI_Create_Temp_Global_Variable_Fwd_Decl (Builder, Scope, Name, Name_Len, Linkage, Lnk_Len, File, Line_No, Ty, Local_To_Unit_Bool, Decl, Align_In_Bits);
      return Return_Value;
   end DI_Create_Temp_Global_Variable_Fwd_Decl;

   function DI_Builder_Create_Auto_Variable
     (Builder         : LLVM.Types.DI_Builder_T;
      Scope           : LLVM.Types.Metadata_T;
      Name            : Interfaces.C.Strings.chars_ptr;
      Name_Len        : stddef_h.size_t;
      File            : LLVM.Types.Metadata_T;
      Line_No         : unsigned;
      Ty              : LLVM.Types.Metadata_T;
      Always_Preserve : LLVM.Types.Bool_T;
      Flags           : DI_Flags_T;
      Align_In_Bits   : stdint_h.uint32_t)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateAutoVariable";
   function DI_Create_Auto_Variable
     (Builder         : LLVM.Types.DI_Builder_T;
      Scope           : LLVM.Types.Metadata_T;
      Name            : String;
      Name_Len        : stddef_h.size_t;
      File            : LLVM.Types.Metadata_T;
      Line_No         : unsigned;
      Ty              : LLVM.Types.Metadata_T;
      Always_Preserve : LLVM.Types.Bool_T;
      Flags           : DI_Flags_T;
      Align_In_Bits   : stdint_h.uint32_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value : LLVM.Types.Metadata_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Auto_Variable (Builder, Scope, Name_String, Name_Len, File, Line_No, Ty, Always_Preserve, Flags, Align_In_Bits);
      return Return_Value;
   end DI_Create_Auto_Variable;

   function DI_Create_Auto_Variable
     (Builder         : LLVM.Types.DI_Builder_T;
      Scope           : LLVM.Types.Metadata_T;
      Name            : String;
      Name_Len        : stddef_h.size_t;
      File            : LLVM.Types.Metadata_T;
      Line_No         : unsigned;
      Ty              : LLVM.Types.Metadata_T;
      Always_Preserve : Boolean;
      Flags           : DI_Flags_T;
      Align_In_Bits   : stdint_h.uint32_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value         : LLVM.Types.Metadata_T;
      Always_Preserve_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Always_Preserve);
   begin
      Return_Value := DI_Create_Auto_Variable (Builder, Scope, Name, Name_Len, File, Line_No, Ty, Always_Preserve_Bool, Flags, Align_In_Bits);
      return Return_Value;
   end DI_Create_Auto_Variable;

   function DI_Builder_Create_Parameter_Variable
     (Builder         : LLVM.Types.DI_Builder_T;
      Scope           : LLVM.Types.Metadata_T;
      Name            : Interfaces.C.Strings.chars_ptr;
      Name_Len        : stddef_h.size_t;
      Arg_No          : unsigned;
      File            : LLVM.Types.Metadata_T;
      Line_No         : unsigned;
      Ty              : LLVM.Types.Metadata_T;
      Always_Preserve : LLVM.Types.Bool_T;
      Flags           : DI_Flags_T)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMDIBuilderCreateParameterVariable";
   function DI_Create_Parameter_Variable
     (Builder         : LLVM.Types.DI_Builder_T;
      Scope           : LLVM.Types.Metadata_T;
      Name            : String;
      Name_Len        : stddef_h.size_t;
      Arg_No          : unsigned;
      File            : LLVM.Types.Metadata_T;
      Line_No         : unsigned;
      Ty              : LLVM.Types.Metadata_T;
      Always_Preserve : LLVM.Types.Bool_T;
      Flags           : DI_Flags_T)
      return LLVM.Types.Metadata_T
   is
      Return_Value : LLVM.Types.Metadata_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := DI_Builder_Create_Parameter_Variable (Builder, Scope, Name_String, Name_Len, Arg_No, File, Line_No, Ty, Always_Preserve, Flags);
      return Return_Value;
   end DI_Create_Parameter_Variable;

   function DI_Create_Parameter_Variable
     (Builder         : LLVM.Types.DI_Builder_T;
      Scope           : LLVM.Types.Metadata_T;
      Name            : String;
      Name_Len        : stddef_h.size_t;
      Arg_No          : unsigned;
      File            : LLVM.Types.Metadata_T;
      Line_No         : unsigned;
      Ty              : LLVM.Types.Metadata_T;
      Always_Preserve : Boolean;
      Flags           : DI_Flags_T)
      return LLVM.Types.Metadata_T
   is
      Return_Value         : LLVM.Types.Metadata_T;
      Always_Preserve_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Always_Preserve);
   begin
      Return_Value := DI_Create_Parameter_Variable (Builder, Scope, Name, Name_Len, Arg_No, File, Line_No, Ty, Always_Preserve_Bool, Flags);
      return Return_Value;
   end DI_Create_Parameter_Variable;

end LLVM.Debug_Info;
