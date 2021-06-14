pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body Clang.Index is

   procedure CX_Index_Set_Invocation_Emission_Path_Option
     (Arg_1 : Index_T;
      Path  : String)
   is
      Path_Array  : aliased char_array := To_C (Path);
      Path_String : constant chars_ptr := To_Chars_Ptr (Path_Array'Unchecked_Access);
   begin
      CX_Index_Set_Invocation_Emission_Path_Option_C (Arg_1, Path_String);
   end CX_Index_Set_Invocation_Emission_Path_Option;

   function Is_File_Multiple_Include_Guarded
     (Tu   : Translation_Unit_T;
      File : File_T)
      return Boolean
   is
   begin
      return (if Is_File_Multiple_Include_Guarded_C (Tu, File) = 0 then False else True);
   end Is_File_Multiple_Include_Guarded;

   function Get_File
     (Tu        : Translation_Unit_T;
      File_Name : String)
      return File_T
   is
      File_Name_Array  : aliased char_array := To_C (File_Name);
      File_Name_String : constant chars_ptr := To_Chars_Ptr (File_Name_Array'Unchecked_Access);
   begin
      return Get_File_C (Tu, File_Name_String);
   end Get_File;

   function Get_File_Contents
     (Tu   : Translation_Unit_T;
      File : File_T;
      Size : access stddef_h.size_t)
      return String
   is
   begin
      return Value (Get_File_Contents_C (Tu, File, Size));
   end Get_File_Contents;

   function File_Is_Equal
     (File_1 : File_T;
      File_2 : File_T)
      return Boolean
   is
   begin
      return (if File_Is_Equal_C (File_1, File_2) = 0 then False else True);
   end File_Is_Equal;

   function Location_Is_In_System_Header
     (Location : Source_Location_T)
      return Boolean
   is
   begin
      return (if Location_Is_In_System_Header_C (Location) = 0 then False else True);
   end Location_Is_In_System_Header;

   function Location_Is_From_Main_File
     (Location : Source_Location_T)
      return Boolean
   is
   begin
      return (if Location_Is_From_Main_File_C (Location) = 0 then False else True);
   end Location_Is_From_Main_File;

   function Range_Is_Null
     (C_Range : Source_Range_T)
      return Boolean
   is
   begin
      return (if Range_Is_Null_C (C_Range) = 0 then False else True);
   end Range_Is_Null;

   function Load_Diagnostics
     (File         : String;
      Error        : access Load_Diag_Error_T;
      Error_String : access Clang.CX_String.String_T)
      return Diagnostic_Set_T
   is
      File_Array  : aliased char_array := To_C (File);
      File_String : constant chars_ptr := To_Chars_Ptr (File_Array'Unchecked_Access);
   begin
      return Load_Diagnostics_C (File_String, Error, Error_String);
   end Load_Diagnostics;

   function Create_Translation_Unit_From_Source_File
     (C_Idx                       : Index_T;
      Source_Filename             : String;
      Num_Clang_Command_Line_Args : int;
      Command_Line_Args           : System.Address;
      Num_Unsaved_Files           : unsigned;
      Unsaved_Files               : access Unsaved_File_T)
      return Translation_Unit_T
   is
      Source_Filename_Array  : aliased char_array := To_C (Source_Filename);
      Source_Filename_String : constant chars_ptr := To_Chars_Ptr (Source_Filename_Array'Unchecked_Access);
   begin
      return Create_Translation_Unit_From_Source_File_C (C_Idx, Source_Filename_String, Num_Clang_Command_Line_Args, Command_Line_Args, Num_Unsaved_Files, Unsaved_Files);
   end Create_Translation_Unit_From_Source_File;

   function Create_Translation_Unit
     (C_Idx        : Index_T;
      Ast_Filename : String)
      return Translation_Unit_T
   is
      Ast_Filename_Array  : aliased char_array := To_C (Ast_Filename);
      Ast_Filename_String : constant chars_ptr := To_Chars_Ptr (Ast_Filename_Array'Unchecked_Access);
   begin
      return Create_Translation_Unit_C (C_Idx, Ast_Filename_String);
   end Create_Translation_Unit;

   function Create_Translation_Unit_2
     (C_Idx        : Index_T;
      Ast_Filename : String;
      Out_TU       : System.Address)
      return Clang.CX_Error_Code.Error_Code_T
   is
      Ast_Filename_Array  : aliased char_array := To_C (Ast_Filename);
      Ast_Filename_String : constant chars_ptr := To_Chars_Ptr (Ast_Filename_Array'Unchecked_Access);
   begin
      return Create_Translation_Unit_2_C (C_Idx, Ast_Filename_String, Out_TU);
   end Create_Translation_Unit_2;

   function Parse_Translation_Unit
     (C_Idx                 : Index_T;
      Source_Filename       : String;
      Command_Line_Args     : System.Address;
      Num_Command_Line_Args : int;
      Unsaved_Files         : access Unsaved_File_T;
      Num_Unsaved_Files     : unsigned;
      Options               : unsigned)
      return Translation_Unit_T
   is
      Source_Filename_Array  : aliased char_array := To_C (Source_Filename);
      Source_Filename_String : constant chars_ptr := To_Chars_Ptr (Source_Filename_Array'Unchecked_Access);
   begin
      return Parse_Translation_Unit_C (C_Idx, Source_Filename_String, Command_Line_Args, Num_Command_Line_Args, Unsaved_Files, Num_Unsaved_Files, Options);
   end Parse_Translation_Unit;

   function Parse_Translation_Unit_2
     (C_Idx                 : Index_T;
      Source_Filename       : String;
      Command_Line_Args     : System.Address;
      Num_Command_Line_Args : int;
      Unsaved_Files         : access Unsaved_File_T;
      Num_Unsaved_Files     : unsigned;
      Options               : unsigned;
      Out_TU                : System.Address)
      return Clang.CX_Error_Code.Error_Code_T
   is
      Source_Filename_Array  : aliased char_array := To_C (Source_Filename);
      Source_Filename_String : constant chars_ptr := To_Chars_Ptr (Source_Filename_Array'Unchecked_Access);
   begin
      return Parse_Translation_Unit_2_C (C_Idx, Source_Filename_String, Command_Line_Args, Num_Command_Line_Args, Unsaved_Files, Num_Unsaved_Files, Options, Out_TU);
   end Parse_Translation_Unit_2;

   function Parse_Translation_Unit_2_Full_Argv
     (C_Idx                 : Index_T;
      Source_Filename       : String;
      Command_Line_Args     : System.Address;
      Num_Command_Line_Args : int;
      Unsaved_Files         : access Unsaved_File_T;
      Num_Unsaved_Files     : unsigned;
      Options               : unsigned;
      Out_TU                : System.Address)
      return Clang.CX_Error_Code.Error_Code_T
   is
      Source_Filename_Array  : aliased char_array := To_C (Source_Filename);
      Source_Filename_String : constant chars_ptr := To_Chars_Ptr (Source_Filename_Array'Unchecked_Access);
   begin
      return Parse_Translation_Unit_2_Full_Argv_C (C_Idx, Source_Filename_String, Command_Line_Args, Num_Command_Line_Args, Unsaved_Files, Num_Unsaved_Files, Options, Out_TU);
   end Parse_Translation_Unit_2_Full_Argv;

   function Save_Translation_Unit
     (TU        : Translation_Unit_T;
      File_Name : String;
      Options   : unsigned)
      return int
   is
      File_Name_Array  : aliased char_array := To_C (File_Name);
      File_Name_String : constant chars_ptr := To_Chars_Ptr (File_Name_Array'Unchecked_Access);
   begin
      return Save_Translation_Unit_C (TU, File_Name_String, Options);
   end Save_Translation_Unit;

   function Get_TU_Resource_Usage_Name
     (Kind : TU_Resource_Usage_Kind_T)
      return String
   is
   begin
      return Value (Get_TU_Resource_Usage_Name_C (Kind));
   end Get_TU_Resource_Usage_Name;

   function Cursor_Is_Null
     (Cursor : Cursor_T)
      return Boolean
   is
   begin
      return (if Cursor_Is_Null_C (Cursor) = 0 then False else True);
   end Cursor_Is_Null;

   function Is_Declaration
     (Arg_1 : Cursor_Kind_T)
      return Boolean
   is
   begin
      return (if Is_Declaration_C (Arg_1) = 0 then False else True);
   end Is_Declaration;

   function Is_Invalid_Declaration
     (Arg_1 : Cursor_T)
      return Boolean
   is
   begin
      return (if Is_Invalid_Declaration_C (Arg_1) = 0 then False else True);
   end Is_Invalid_Declaration;

   function Is_Reference
     (Arg_1 : Cursor_Kind_T)
      return Boolean
   is
   begin
      return (if Is_Reference_C (Arg_1) = 0 then False else True);
   end Is_Reference;

   function Is_Expression
     (Arg_1 : Cursor_Kind_T)
      return Boolean
   is
   begin
      return (if Is_Expression_C (Arg_1) = 0 then False else True);
   end Is_Expression;

   function Is_Statement
     (Arg_1 : Cursor_Kind_T)
      return Boolean
   is
   begin
      return (if Is_Statement_C (Arg_1) = 0 then False else True);
   end Is_Statement;

   function Is_Attribute
     (Arg_1 : Cursor_Kind_T)
      return Boolean
   is
   begin
      return (if Is_Attribute_C (Arg_1) = 0 then False else True);
   end Is_Attribute;

   function Is_Invalid
     (Arg_1 : Cursor_Kind_T)
      return Boolean
   is
   begin
      return (if Is_Invalid_C (Arg_1) = 0 then False else True);
   end Is_Invalid;

   function Is_Translation_Unit
     (Arg_1 : Cursor_Kind_T)
      return Boolean
   is
   begin
      return (if Is_Translation_Unit_C (Arg_1) = 0 then False else True);
   end Is_Translation_Unit;

   function Is_Preprocessing
     (Arg_1 : Cursor_Kind_T)
      return Boolean
   is
   begin
      return (if Is_Preprocessing_C (Arg_1) = 0 then False else True);
   end Is_Preprocessing;

   function Is_Unexposed
     (Arg_1 : Cursor_Kind_T)
      return Boolean
   is
   begin
      return (if Is_Unexposed_C (Arg_1) = 0 then False else True);
   end Is_Unexposed;

   function Is_Const_Qualified_Type
     (T : Type_T)
      return Boolean
   is
   begin
      return (if Is_Const_Qualified_Type_C (T) = 0 then False else True);
   end Is_Const_Qualified_Type;

   function Cursor_Is_Macro_Function_Like
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if Cursor_Is_Macro_Function_Like_C (C) = 0 then False else True);
   end Cursor_Is_Macro_Function_Like;

   function Cursor_Is_Macro_Builtin
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if Cursor_Is_Macro_Builtin_C (C) = 0 then False else True);
   end Cursor_Is_Macro_Builtin;

   function Cursor_Is_Function_Inlined
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if Cursor_Is_Function_Inlined_C (C) = 0 then False else True);
   end Cursor_Is_Function_Inlined;

   function Is_Volatile_Qualified_Type
     (T : Type_T)
      return Boolean
   is
   begin
      return (if Is_Volatile_Qualified_Type_C (T) = 0 then False else True);
   end Is_Volatile_Qualified_Type;

   function Is_Restrict_Qualified_Type
     (T : Type_T)
      return Boolean
   is
   begin
      return (if Is_Restrict_Qualified_Type_C (T) = 0 then False else True);
   end Is_Restrict_Qualified_Type;

   function Is_Function_Type_Variadic
     (T : Type_T)
      return Boolean
   is
   begin
      return (if Is_Function_Type_Variadic_C (T) = 0 then False else True);
   end Is_Function_Type_Variadic;

   function Is_POD_Type
     (T : Type_T)
      return Boolean
   is
   begin
      return (if Is_POD_Type_C (T) = 0 then False else True);
   end Is_POD_Type;

   function Type_Is_Transparent_Tag_Typedef
     (T : Type_T)
      return Boolean
   is
   begin
      return (if Type_Is_Transparent_Tag_Typedef_C (T) = 0 then False else True);
   end Type_Is_Transparent_Tag_Typedef;

   function Type_Get_Offset_Of
     (T : Type_T;
      S : String)
      return Long_Long_Integer
   is
      S_Array  : aliased char_array := To_C (S);
      S_String : constant chars_ptr := To_Chars_Ptr (S_Array'Unchecked_Access);
   begin
      return Type_Get_Offset_Of_C (T, S_String);
   end Type_Get_Offset_Of;

   function Cursor_Is_Anonymous
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if Cursor_Is_Anonymous_C (C) = 0 then False else True);
   end Cursor_Is_Anonymous;

   function Cursor_Is_Anonymous_Record_Decl
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if Cursor_Is_Anonymous_Record_Decl_C (C) = 0 then False else True);
   end Cursor_Is_Anonymous_Record_Decl;

   function Cursor_Is_Inline_Namespace
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if Cursor_Is_Inline_Namespace_C (C) = 0 then False else True);
   end Cursor_Is_Inline_Namespace;

   function Cursor_Is_Bit_Field
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if Cursor_Is_Bit_Field_C (C) = 0 then False else True);
   end Cursor_Is_Bit_Field;

   function Is_Virtual_Base
     (Arg_1 : Cursor_T)
      return Boolean
   is
   begin
      return (if Is_Virtual_Base_C (Arg_1) = 0 then False else True);
   end Is_Virtual_Base;

   function Construct_USR_Obj_C_Class
     (Class_Name : String)
      return Clang.CX_String.String_T
   is
      Class_Name_Array  : aliased char_array := To_C (Class_Name);
      Class_Name_String : constant chars_ptr := To_Chars_Ptr (Class_Name_Array'Unchecked_Access);
   begin
      return Construct_USR_Obj_C_Class_C (Class_Name_String);
   end Construct_USR_Obj_C_Class;

   function Construct_USR_Obj_C_Category
     (Class_Name    : String;
      Category_Name : String)
      return Clang.CX_String.String_T
   is
      Class_Name_Array     : aliased char_array := To_C (Class_Name);
      Class_Name_String    : constant chars_ptr := To_Chars_Ptr (Class_Name_Array'Unchecked_Access);
      Category_Name_Array  : aliased char_array := To_C (Category_Name);
      Category_Name_String : constant chars_ptr := To_Chars_Ptr (Category_Name_Array'Unchecked_Access);
   begin
      return Construct_USR_Obj_C_Category_C (Class_Name_String, Category_Name_String);
   end Construct_USR_Obj_C_Category;

   function Construct_USR_Obj_C_Protocol
     (Protocol_Name : String)
      return Clang.CX_String.String_T
   is
      Protocol_Name_Array  : aliased char_array := To_C (Protocol_Name);
      Protocol_Name_String : constant chars_ptr := To_Chars_Ptr (Protocol_Name_Array'Unchecked_Access);
   begin
      return Construct_USR_Obj_C_Protocol_C (Protocol_Name_String);
   end Construct_USR_Obj_C_Protocol;

   function Construct_USR_Obj_C_Ivar
     (Name      : String;
      Class_USR : Clang.CX_String.String_T)
      return Clang.CX_String.String_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Construct_USR_Obj_C_Ivar_C (Name_String, Class_USR);
   end Construct_USR_Obj_C_Ivar;

   function Construct_USR_Obj_C_Method
     (Name               : String;
      Is_Instance_Method : unsigned;
      Class_USR          : Clang.CX_String.String_T)
      return Clang.CX_String.String_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Construct_USR_Obj_C_Method_C (Name_String, Is_Instance_Method, Class_USR);
   end Construct_USR_Obj_C_Method;

   function Construct_USR_Obj_C_Property
     (Property  : String;
      Class_USR : Clang.CX_String.String_T)
      return Clang.CX_String.String_T
   is
      Property_Array  : aliased char_array := To_C (Property);
      Property_String : constant chars_ptr := To_Chars_Ptr (Property_Array'Unchecked_Access);
   begin
      return Construct_USR_Obj_C_Property_C (Property_String, Class_USR);
   end Construct_USR_Obj_C_Property;

   function Is_Cursor_Definition
     (Arg_1 : Cursor_T)
      return Boolean
   is
   begin
      return (if Is_Cursor_Definition_C (Arg_1) = 0 then False else True);
   end Is_Cursor_Definition;

   function Cursor_Is_Dynamic_Call
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if Cursor_Is_Dynamic_Call_C (C) = 0 then False else True);
   end Cursor_Is_Dynamic_Call;

   function Cursor_Is_Obj_C_Optional
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if Cursor_Is_Obj_C_Optional_C (C) = 0 then False else True);
   end Cursor_Is_Obj_C_Optional;

   function Cursor_Is_Variadic
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if Cursor_Is_Variadic_C (C) = 0 then False else True);
   end Cursor_Is_Variadic;

   function Cursor_Is_External_Symbol
     (C            : Cursor_T;
      Language     : access Clang.CX_String.String_T;
      Defined_In   : access Clang.CX_String.String_T;
      Is_Generated : access unsigned)
      return Boolean
   is
   begin
      return (if Cursor_Is_External_Symbol_C (C, Language, Defined_In, Is_Generated) = 0 then False else True);
   end Cursor_Is_External_Symbol;

   function Module_Is_System
     (Module : Module_T)
      return Boolean
   is
   begin
      return (if Module_Is_System_C (Module) = 0 then False else True);
   end Module_Is_System;

   function CXX_Constructor_Is_Converting_Constructor
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if CXX_Constructor_Is_Converting_Constructor_C (C) = 0 then False else True);
   end CXX_Constructor_Is_Converting_Constructor;

   function CXX_Constructor_Is_Copy_Constructor
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if CXX_Constructor_Is_Copy_Constructor_C (C) = 0 then False else True);
   end CXX_Constructor_Is_Copy_Constructor;

   function CXX_Constructor_Is_Default_Constructor
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if CXX_Constructor_Is_Default_Constructor_C (C) = 0 then False else True);
   end CXX_Constructor_Is_Default_Constructor;

   function CXX_Constructor_Is_Move_Constructor
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if CXX_Constructor_Is_Move_Constructor_C (C) = 0 then False else True);
   end CXX_Constructor_Is_Move_Constructor;

   function CXX_Field_Is_Mutable
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if CXX_Field_Is_Mutable_C (C) = 0 then False else True);
   end CXX_Field_Is_Mutable;

   function CXX_Method_Is_Defaulted
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if CXX_Method_Is_Defaulted_C (C) = 0 then False else True);
   end CXX_Method_Is_Defaulted;

   function CXX_Method_Is_Pure_Virtual
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if CXX_Method_Is_Pure_Virtual_C (C) = 0 then False else True);
   end CXX_Method_Is_Pure_Virtual;

   function CXX_Method_Is_Static
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if CXX_Method_Is_Static_C (C) = 0 then False else True);
   end CXX_Method_Is_Static;

   function CXX_Method_Is_Virtual
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if CXX_Method_Is_Virtual_C (C) = 0 then False else True);
   end CXX_Method_Is_Virtual;

   function CXX_Record_Is_Abstract
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if CXX_Record_Is_Abstract_C (C) = 0 then False else True);
   end CXX_Record_Is_Abstract;

   function Enum_Decl_Is_Scoped
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if Enum_Decl_Is_Scoped_C (C) = 0 then False else True);
   end Enum_Decl_Is_Scoped;

   function CXX_Method_Is_Const
     (C : Cursor_T)
      return Boolean
   is
   begin
      return (if CXX_Method_Is_Const_C (C) = 0 then False else True);
   end CXX_Method_Is_Const;

   function Code_Complete_At
     (TU                : Translation_Unit_T;
      Complete_Filename : String;
      Complete_Line     : unsigned;
      Complete_Column   : unsigned;
      Unsaved_Files     : access Unsaved_File_T;
      Num_Unsaved_Files : unsigned;
      Options           : unsigned)
      return access Code_Complete_Results_T
   is
      Complete_Filename_Array  : aliased char_array := To_C (Complete_Filename);
      Complete_Filename_String : constant chars_ptr := To_Chars_Ptr (Complete_Filename_Array'Unchecked_Access);
   begin
      return Code_Complete_At_C (TU, Complete_Filename_String, Complete_Line, Complete_Column, Unsaved_Files, Num_Unsaved_Files, Options);
   end Code_Complete_At;

   function Eval_Result_Is_Unsigned_Int
     (E : Eval_Result_T)
      return Boolean
   is
   begin
      return (if Eval_Result_Is_Unsigned_Int_C (E) = 0 then False else True);
   end Eval_Result_Is_Unsigned_Int;

   function Eval_Result_Get_As_Str
     (E : Eval_Result_T)
      return String
   is
   begin
      return Value (Eval_Result_Get_As_Str_C (E));
   end Eval_Result_Get_As_Str;

   function Get_Remappings
     (Path : String)
      return Remapping_T
   is
      Path_Array  : aliased char_array := To_C (Path);
      Path_String : constant chars_ptr := To_Chars_Ptr (Path_Array'Unchecked_Access);
   begin
      return Get_Remappings_C (Path_String);
   end Get_Remappings;

   function Index_Is_Entity_Obj_C_Container_Kind
     (Arg_1 : Idx_Entity_Kind_T)
      return Boolean
   is
   begin
      return (if Index_Is_Entity_Obj_C_Container_Kind_C (Arg_1) = 0 then False else True);
   end Index_Is_Entity_Obj_C_Container_Kind;

   function Index_Source_File
     (Arg_1                 : Index_Action_T;
      Client_Data           : Client_Data_T;
      Index_Callbacks       : access IndexerCallbacks;
      Index_Callbacks_Size  : unsigned;
      Index_Options         : unsigned;
      Source_Filename       : String;
      Command_Line_Args     : System.Address;
      Num_Command_Line_Args : int;
      Unsaved_Files         : access Unsaved_File_T;
      Num_Unsaved_Files     : unsigned;
      Out_TU                : System.Address;
      TU_Options            : unsigned)
      return int
   is
      Source_Filename_Array  : aliased char_array := To_C (Source_Filename);
      Source_Filename_String : constant chars_ptr := To_Chars_Ptr (Source_Filename_Array'Unchecked_Access);
   begin
      return Index_Source_File_C (Arg_1, Client_Data, Index_Callbacks, Index_Callbacks_Size, Index_Options, Source_Filename_String, Command_Line_Args, Num_Command_Line_Args, Unsaved_Files, Num_Unsaved_Files, Out_TU, TU_Options);
   end Index_Source_File;

   function Index_Source_File_Full_Argv
     (Arg_1                 : Index_Action_T;
      Client_Data           : Client_Data_T;
      Index_Callbacks       : access IndexerCallbacks;
      Index_Callbacks_Size  : unsigned;
      Index_Options         : unsigned;
      Source_Filename       : String;
      Command_Line_Args     : System.Address;
      Num_Command_Line_Args : int;
      Unsaved_Files         : access Unsaved_File_T;
      Num_Unsaved_Files     : unsigned;
      Out_TU                : System.Address;
      TU_Options            : unsigned)
      return int
   is
      Source_Filename_Array  : aliased char_array := To_C (Source_Filename);
      Source_Filename_String : constant chars_ptr := To_Chars_Ptr (Source_Filename_Array'Unchecked_Access);
   begin
      return Index_Source_File_Full_Argv_C (Arg_1, Client_Data, Index_Callbacks, Index_Callbacks_Size, Index_Options, Source_Filename_String, Command_Line_Args, Num_Command_Line_Args, Unsaved_Files, Num_Unsaved_Files, Out_TU, TU_Options);
   end Index_Source_File_Full_Argv;

end Clang.Index;
