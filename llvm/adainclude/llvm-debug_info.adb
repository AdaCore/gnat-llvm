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

   function DI_Create_Module
     (Builder           : LLVM.Types.DI_Builder_T;
      Parent_Scope      : LLVM.Types.Metadata_T;
      Name              : String;
      Name_Len          : stddef_h.size_t;
      Config_Macros     : String;
      Config_Macros_Len : stddef_h.size_t;
      Include_Path      : String;
      Include_Path_Len  : stddef_h.size_t;
      Sys_Root          : String;
      Sys_Root_Len      : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   is
      Name_Array           : aliased char_array := To_C (Name);
      Name_String          : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Config_Macros_Array  : aliased char_array := To_C (Config_Macros);
      Config_Macros_String : constant chars_ptr := To_Chars_Ptr (Config_Macros_Array'Unchecked_Access);
      Include_Path_Array   : aliased char_array := To_C (Include_Path);
      Include_Path_String  : constant chars_ptr := To_Chars_Ptr (Include_Path_Array'Unchecked_Access);
      Sys_Root_Array       : aliased char_array := To_C (Sys_Root);
      Sys_Root_String      : constant chars_ptr := To_Chars_Ptr (Sys_Root_Array'Unchecked_Access);
   begin
      return DI_Builder_Create_Module_C (Builder, Parent_Scope, Name_String, Name_Len, Config_Macros_String, Config_Macros_Len, Include_Path_String, Include_Path_Len, Sys_Root_String, Sys_Root_Len);
   end DI_Create_Module;

   function DI_Create_Name_Space
     (Builder        : LLVM.Types.DI_Builder_T;
      Parent_Scope   : LLVM.Types.Metadata_T;
      Name           : String;
      Name_Len       : stddef_h.size_t;
      Export_Symbols : Boolean)
      return LLVM.Types.Metadata_T
   is
      Name_Array          : aliased char_array := To_C (Name);
      Name_String         : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Export_Symbols_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Export_Symbols);
   begin
      return DI_Builder_Create_Name_Space_C (Builder, Parent_Scope, Name_String, Name_Len, Export_Symbols_Bool);
   end DI_Create_Name_Space;

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
      Name_Array            : aliased char_array := To_C (Name);
      Name_String           : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Linkage_Name_Array    : aliased char_array := To_C (Linkage_Name);
      Linkage_Name_String   : constant chars_ptr := To_Chars_Ptr (Linkage_Name_Array'Unchecked_Access);
      Is_Local_To_Unit_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Local_To_Unit);
      Is_Definition_Bool    : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Definition);
      Is_Optimized_Bool     : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Optimized);
   begin
      return DI_Builder_Create_Function_C (Builder, Scope, Name_String, Name_Len, Linkage_Name_String, Linkage_Name_Len, File, Line_No, Ty, Is_Local_To_Unit_Bool, Is_Definition_Bool, Scope_Line, Flags, Is_Optimized_Bool);
   end DI_Create_Function;

   function DI_Create_Imported_Declaration
     (Builder  : LLVM.Types.DI_Builder_T;
      Scope    : LLVM.Types.Metadata_T;
      Decl     : LLVM.Types.Metadata_T;
      File     : LLVM.Types.Metadata_T;
      Line     : unsigned;
      Name     : String;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return DI_Builder_Create_Imported_Declaration_C (Builder, Scope, Decl, File, Line, Name_String, Name_Len);
   end DI_Create_Imported_Declaration;

   function DI_File_Get_Directory
     (File : LLVM.Types.Metadata_T;
      Len  : unsigned)
      return String
   is
   begin
      return Value (DI_File_Get_Directory_C (File, Len));
   end DI_File_Get_Directory;

   function DI_File_Get_Filename
     (File : LLVM.Types.Metadata_T;
      Len  : unsigned)
      return String
   is
   begin
      return Value (DI_File_Get_Filename_C (File, Len));
   end DI_File_Get_Filename;

   function DI_File_Get_Source
     (File : LLVM.Types.Metadata_T;
      Len  : unsigned)
      return String
   is
   begin
      return Value (DI_File_Get_Source_C (File, Len));
   end DI_File_Get_Source;

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
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Value_Array  : aliased char_array := To_C (Value);
      Value_String : constant chars_ptr := To_Chars_Ptr (Value_Array'Unchecked_Access);
   begin
      return DI_Builder_Create_Macro_C (Builder, Parent_Macro_File, Line, Record_Type, Name_String, Name_Len, Value_String, Value_Len);
   end DI_Create_Macro;

   function DI_Create_Enumerator
     (Builder     : LLVM.Types.DI_Builder_T;
      Name        : String;
      Name_Len    : stddef_h.size_t;
      Value       : stdint_h.int64_t;
      Is_Unsigned : Boolean)
      return LLVM.Types.Metadata_T
   is
      Name_Array       : aliased char_array := To_C (Name);
      Name_String      : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Is_Unsigned_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Unsigned);
   begin
      return DI_Builder_Create_Enumerator_C (Builder, Name_String, Name_Len, Value, Is_Unsigned_Bool);
   end DI_Create_Enumerator;

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
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return DI_Builder_Create_Enumeration_Type_C (Builder, Scope, Name_String, Name_Len, File, Line_Number, Size_In_Bits, Align_In_Bits, Elements, Num_Elements, Class_Ty);
   end DI_Create_Enumeration_Type;

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
      Name_Array       : aliased char_array := To_C (Name);
      Name_String      : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Unique_Id_Array  : aliased char_array := To_C (Unique_Id);
      Unique_Id_String : constant chars_ptr := To_Chars_Ptr (Unique_Id_Array'Unchecked_Access);
   begin
      return DI_Builder_Create_Union_Type_C (Builder, Scope, Name_String, Name_Len, File, Line_Number, Size_In_Bits, Align_In_Bits, Flags, Elements, Num_Elements, Run_Time_Lang, Unique_Id_String, Unique_Id_Len);
   end DI_Create_Union_Type;

   function DI_Create_Unspecified_Type
     (Builder  : LLVM.Types.DI_Builder_T;
      Name     : String;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return DI_Builder_Create_Unspecified_Type_C (Builder, Name_String, Name_Len);
   end DI_Create_Unspecified_Type;

   function DI_Create_Basic_Type
     (Builder      : LLVM.Types.DI_Builder_T;
      Name         : String;
      Name_Len     : stddef_h.size_t;
      Size_In_Bits : stdint_h.uint64_t;
      Encoding     : DWARF_Type_Encoding_T;
      Flags        : DI_Flags_T)
      return LLVM.Types.Metadata_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return DI_Builder_Create_Basic_Type_C (Builder, Name_String, Name_Len, Size_In_Bits, Encoding, Flags);
   end DI_Create_Basic_Type;

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
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return DI_Builder_Create_Pointer_Type_C (Builder, Pointee_Ty, Size_In_Bits, Align_In_Bits, Address_Space, Name_String, Name_Len);
   end DI_Create_Pointer_Type;

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
      Name_Array       : aliased char_array := To_C (Name);
      Name_String      : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Unique_Id_Array  : aliased char_array := To_C (Unique_Id);
      Unique_Id_String : constant chars_ptr := To_Chars_Ptr (Unique_Id_Array'Unchecked_Access);
   begin
      return DI_Builder_Create_Struct_Type_C (Builder, Scope, Name_String, Name_Len, File, Line_Number, Size_In_Bits, Align_In_Bits, Flags, Derived_From, Elements, Num_Elements, Run_Time_Lang, V_Table_Holder, Unique_Id_String, Unique_Id_Len);
   end DI_Create_Struct_Type;

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
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return DI_Builder_Create_Member_Type_C (Builder, Scope, Name_String, Name_Len, File, Line_No, Size_In_Bits, Align_In_Bits, Offset_In_Bits, Flags, Ty);
   end DI_Create_Member_Type;

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
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return DI_Builder_Create_Static_Member_Type_C (Builder, Scope, Name_String, Name_Len, File, Line_Number, C_Type, Flags, Constant_Val, Align_In_Bits);
   end DI_Create_Static_Member_Type;

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
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return DI_Builder_Create_Obj_CI_Var_C (Builder, Name_String, Name_Len, File, Line_No, Size_In_Bits, Align_In_Bits, Offset_In_Bits, Flags, Ty, Property_Node);
   end DI_Create_Obj_CI_Var;

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
      Name_Array         : aliased char_array := To_C (Name);
      Name_String        : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Getter_Name_Array  : aliased char_array := To_C (Getter_Name);
      Getter_Name_String : constant chars_ptr := To_Chars_Ptr (Getter_Name_Array'Unchecked_Access);
      Setter_Name_Array  : aliased char_array := To_C (Setter_Name);
      Setter_Name_String : constant chars_ptr := To_Chars_Ptr (Setter_Name_Array'Unchecked_Access);
   begin
      return DI_Builder_Create_Obj_C_Property_C (Builder, Name_String, Name_Len, File, Line_No, Getter_Name_String, Getter_Name_Len, Setter_Name_String, Setter_Name_Len, Property_Attributes, Ty);
   end DI_Create_Obj_C_Property;

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
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return DI_Builder_Create_Typedef_C (Builder, C_Type, Name_String, Name_Len, File, Line_No, Scope, Align_In_Bits);
   end DI_Create_Typedef;

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
      Name_Array               : aliased char_array := To_C (Name);
      Name_String              : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Unique_Identifier_Array  : aliased char_array := To_C (Unique_Identifier);
      Unique_Identifier_String : constant chars_ptr := To_Chars_Ptr (Unique_Identifier_Array'Unchecked_Access);
   begin
      return DI_Builder_Create_Forward_Decl_C (Builder, Tag, Name_String, Name_Len, Scope, File, Line, Runtime_Lang, Size_In_Bits, Align_In_Bits, Unique_Identifier_String, Unique_Identifier_Len);
   end DI_Create_Forward_Decl;

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
      Name_Array               : aliased char_array := To_C (Name);
      Name_String              : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Unique_Identifier_Array  : aliased char_array := To_C (Unique_Identifier);
      Unique_Identifier_String : constant chars_ptr := To_Chars_Ptr (Unique_Identifier_Array'Unchecked_Access);
   begin
      return DI_Builder_Create_Replaceable_Composite_Type_C (Builder, Tag, Name_String, Name_Len, Scope, File, Line, Runtime_Lang, Size_In_Bits, Align_In_Bits, Flags, Unique_Identifier_String, Unique_Identifier_Len);
   end DI_Create_Replaceable_Composite_Type;

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
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return DI_Builder_Create_Bit_Field_Member_Type_C (Builder, Scope, Name_String, Name_Len, File, Line_Number, Size_In_Bits, Offset_In_Bits, Storage_Offset_In_Bits, Flags, C_Type);
   end DI_Create_Bit_Field_Member_Type;

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
      Name_Array               : aliased char_array := To_C (Name);
      Name_String              : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Unique_Identifier_Array  : aliased char_array := To_C (Unique_Identifier);
      Unique_Identifier_String : constant chars_ptr := To_Chars_Ptr (Unique_Identifier_Array'Unchecked_Access);
   begin
      return DI_Builder_Create_Class_Type_C (Builder, Scope, Name_String, Name_Len, File, Line_Number, Size_In_Bits, Align_In_Bits, Offset_In_Bits, Flags, Derived_From, Elements, Num_Elements, V_Table_Holder, Template_Params_Node, Unique_Identifier_String, Unique_Identifier_Len);
   end DI_Create_Class_Type;

   function DI_Type_Get_Name
     (D_Type : LLVM.Types.Metadata_T;
      Length : stddef_h.size_t)
      return String
   is
   begin
      return Value (DI_Type_Get_Name_C (D_Type, Length));
   end DI_Type_Get_Name;

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
      Name_Array         : aliased char_array := To_C (Name);
      Name_String        : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Linkage_Array      : aliased char_array := To_C (Linkage);
      Linkage_String     : constant chars_ptr := To_Chars_Ptr (Linkage_Array'Unchecked_Access);
      Local_To_Unit_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Local_To_Unit);
   begin
      return DI_Builder_Create_Global_Variable_Expression_C (Builder, Scope, Name_String, Name_Len, Linkage_String, Link_Len, File, Line_No, Ty, Local_To_Unit_Bool, Expr, Decl, Align_In_Bits);
   end DI_Create_Global_Variable_Expression;

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
      Name_Array         : aliased char_array := To_C (Name);
      Name_String        : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Linkage_Array      : aliased char_array := To_C (Linkage);
      Linkage_String     : constant chars_ptr := To_Chars_Ptr (Linkage_Array'Unchecked_Access);
      Local_To_Unit_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Local_To_Unit);
   begin
      return DI_Builder_Create_Temp_Global_Variable_Fwd_Decl_C (Builder, Scope, Name_String, Name_Len, Linkage_String, Lnk_Len, File, Line_No, Ty, Local_To_Unit_Bool, Decl, Align_In_Bits);
   end DI_Create_Temp_Global_Variable_Fwd_Decl;

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
      Name_Array           : aliased char_array := To_C (Name);
      Name_String          : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Always_Preserve_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Always_Preserve);
   begin
      return DI_Builder_Create_Auto_Variable_C (Builder, Scope, Name_String, Name_Len, File, Line_No, Ty, Always_Preserve_Bool, Flags, Align_In_Bits);
   end DI_Create_Auto_Variable;

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
      Name_Array           : aliased char_array := To_C (Name);
      Name_String          : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
      Always_Preserve_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Always_Preserve);
   begin
      return DI_Builder_Create_Parameter_Variable_C (Builder, Scope, Name_String, Name_Len, Arg_No, File, Line_No, Ty, Always_Preserve_Bool, Flags);
   end DI_Create_Parameter_Variable;

end LLVM.Debug_Info;
