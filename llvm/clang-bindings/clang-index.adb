pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body Clang.Index is

   procedure CX_Index_Set_Invocation_Emission_Path_Option
     (Arg_1 : Index_T;
      Path  : Interfaces.C.Strings.chars_ptr)
   with Import => True,
        Convention => C,
        External_Name => "clang_CXIndex_setInvocationEmissionPathOption";
   procedure CX_Index_Set_Invocation_Emission_Path_Option
     (Arg_1 : Index_T;
      Path  : String)
   is
      Path_Array  : aliased char_array := To_C (Path);
      Path_String : constant chars_ptr := To_Chars_Ptr (Path_Array'Unchecked_Access);
   begin
      CX_Index_Set_Invocation_Emission_Path_Option (Arg_1, Path_String);
   end CX_Index_Set_Invocation_Emission_Path_Option;

   function Is_File_Multiple_Include_Guarded
     (Tu   : Translation_Unit_T;
      File : Clang.CX_File.File_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_isFileMultipleIncludeGuarded";
   function Is_File_Multiple_Include_Guarded
     (Tu   : Translation_Unit_T;
      File : Clang.CX_File.File_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Is_File_Multiple_Include_Guarded (Tu, File);
      return Return_Value /= 0;
   end Is_File_Multiple_Include_Guarded;

   function Get_File
     (Tu        : Translation_Unit_T;
      File_Name : Interfaces.C.Strings.chars_ptr)
      return Clang.CX_File.File_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getFile";
   function Get_File
     (Tu        : Translation_Unit_T;
      File_Name : String)
      return Clang.CX_File.File_T
   is
      Return_Value     : Clang.CX_File.File_T;
      File_Name_Array  : aliased char_array := To_C (File_Name);
      File_Name_String : constant chars_ptr := To_Chars_Ptr (File_Name_Array'Unchecked_Access);
   begin
      Return_Value := Get_File (Tu, File_Name_String);
      return Return_Value;
   end Get_File;

   function Get_File_Contents
     (Tu   : Translation_Unit_T;
      File : Clang.CX_File.File_T;
      Size : access stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "clang_getFileContents";
   function Get_File_Contents
     (Tu   : Translation_Unit_T;
      File : Clang.CX_File.File_T;
      Size : access stddef_h.size_t)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_File_Contents (Tu, File, Size);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_File_Contents;

   function Get_Translation_Unit_Spelling
     (CT_Unit : Translation_Unit_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getTranslationUnitSpelling";
   function Get_Translation_Unit_Spelling
     (CT_Unit : Translation_Unit_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Translation_Unit_Spelling (CT_Unit);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Translation_Unit_Spelling;

   function Create_Translation_Unit_From_Source_File
     (C_Idx                       : Index_T;
      Source_Filename             : Interfaces.C.Strings.chars_ptr;
      Num_Clang_Command_Line_Args : int;
      Command_Line_Args           : System.Address;
      Num_Unsaved_Files           : unsigned;
      Unsaved_Files               : access Unsaved_File_T)
      return Translation_Unit_T
   with Import => True,
        Convention => C,
        External_Name => "clang_createTranslationUnitFromSourceFile";
   function Create_Translation_Unit_From_Source_File
     (C_Idx                       : Index_T;
      Source_Filename             : String;
      Num_Clang_Command_Line_Args : int;
      Command_Line_Args           : System.Address;
      Num_Unsaved_Files           : unsigned;
      Unsaved_Files               : access Unsaved_File_T)
      return Translation_Unit_T
   is
      Return_Value           : Translation_Unit_T;
      Source_Filename_Array  : aliased char_array := To_C (Source_Filename);
      Source_Filename_String : constant chars_ptr := To_Chars_Ptr (Source_Filename_Array'Unchecked_Access);
   begin
      Return_Value := Create_Translation_Unit_From_Source_File (C_Idx, Source_Filename_String, Num_Clang_Command_Line_Args, Command_Line_Args, Num_Unsaved_Files, Unsaved_Files);
      return Return_Value;
   end Create_Translation_Unit_From_Source_File;

   function Create_Translation_Unit
     (C_Idx        : Index_T;
      Ast_Filename : Interfaces.C.Strings.chars_ptr)
      return Translation_Unit_T
   with Import => True,
        Convention => C,
        External_Name => "clang_createTranslationUnit";
   function Create_Translation_Unit
     (C_Idx        : Index_T;
      Ast_Filename : String)
      return Translation_Unit_T
   is
      Return_Value        : Translation_Unit_T;
      Ast_Filename_Array  : aliased char_array := To_C (Ast_Filename);
      Ast_Filename_String : constant chars_ptr := To_Chars_Ptr (Ast_Filename_Array'Unchecked_Access);
   begin
      Return_Value := Create_Translation_Unit (C_Idx, Ast_Filename_String);
      return Return_Value;
   end Create_Translation_Unit;

   function Create_Translation_Unit_2
     (C_Idx        : Index_T;
      Ast_Filename : Interfaces.C.Strings.chars_ptr;
      Out_TU       : System.Address)
      return Clang.CX_Error_Code.Error_Code_T
   with Import => True,
        Convention => C,
        External_Name => "clang_createTranslationUnit2";
   function Create_Translation_Unit_2
     (C_Idx        : Index_T;
      Ast_Filename : String;
      Out_TU       : System.Address)
      return Clang.CX_Error_Code.Error_Code_T
   is
      Return_Value        : Clang.CX_Error_Code.Error_Code_T;
      Ast_Filename_Array  : aliased char_array := To_C (Ast_Filename);
      Ast_Filename_String : constant chars_ptr := To_Chars_Ptr (Ast_Filename_Array'Unchecked_Access);
   begin
      Return_Value := Create_Translation_Unit_2 (C_Idx, Ast_Filename_String, Out_TU);
      return Return_Value;
   end Create_Translation_Unit_2;

   function Parse_Translation_Unit
     (C_Idx                 : Index_T;
      Source_Filename       : Interfaces.C.Strings.chars_ptr;
      Command_Line_Args     : System.Address;
      Num_Command_Line_Args : int;
      Unsaved_Files         : access Unsaved_File_T;
      Num_Unsaved_Files     : unsigned;
      Options               : unsigned)
      return Translation_Unit_T
   with Import => True,
        Convention => C,
        External_Name => "clang_parseTranslationUnit";
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
      Return_Value           : Translation_Unit_T;
      Source_Filename_Array  : aliased char_array := To_C (Source_Filename);
      Source_Filename_String : constant chars_ptr := To_Chars_Ptr (Source_Filename_Array'Unchecked_Access);
   begin
      Return_Value := Parse_Translation_Unit (C_Idx, Source_Filename_String, Command_Line_Args, Num_Command_Line_Args, Unsaved_Files, Num_Unsaved_Files, Options);
      return Return_Value;
   end Parse_Translation_Unit;

   function Parse_Translation_Unit_2
     (C_Idx                 : Index_T;
      Source_Filename       : Interfaces.C.Strings.chars_ptr;
      Command_Line_Args     : System.Address;
      Num_Command_Line_Args : int;
      Unsaved_Files         : access Unsaved_File_T;
      Num_Unsaved_Files     : unsigned;
      Options               : unsigned;
      Out_TU                : System.Address)
      return Clang.CX_Error_Code.Error_Code_T
   with Import => True,
        Convention => C,
        External_Name => "clang_parseTranslationUnit2";
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
      Return_Value           : Clang.CX_Error_Code.Error_Code_T;
      Source_Filename_Array  : aliased char_array := To_C (Source_Filename);
      Source_Filename_String : constant chars_ptr := To_Chars_Ptr (Source_Filename_Array'Unchecked_Access);
   begin
      Return_Value := Parse_Translation_Unit_2 (C_Idx, Source_Filename_String, Command_Line_Args, Num_Command_Line_Args, Unsaved_Files, Num_Unsaved_Files, Options, Out_TU);
      return Return_Value;
   end Parse_Translation_Unit_2;

   function Parse_Translation_Unit_2_Full_Argv
     (C_Idx                 : Index_T;
      Source_Filename       : Interfaces.C.Strings.chars_ptr;
      Command_Line_Args     : System.Address;
      Num_Command_Line_Args : int;
      Unsaved_Files         : access Unsaved_File_T;
      Num_Unsaved_Files     : unsigned;
      Options               : unsigned;
      Out_TU                : System.Address)
      return Clang.CX_Error_Code.Error_Code_T
   with Import => True,
        Convention => C,
        External_Name => "clang_parseTranslationUnit2FullArgv";
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
      Return_Value           : Clang.CX_Error_Code.Error_Code_T;
      Source_Filename_Array  : aliased char_array := To_C (Source_Filename);
      Source_Filename_String : constant chars_ptr := To_Chars_Ptr (Source_Filename_Array'Unchecked_Access);
   begin
      Return_Value := Parse_Translation_Unit_2_Full_Argv (C_Idx, Source_Filename_String, Command_Line_Args, Num_Command_Line_Args, Unsaved_Files, Num_Unsaved_Files, Options, Out_TU);
      return Return_Value;
   end Parse_Translation_Unit_2_Full_Argv;

   function Save_Translation_Unit
     (TU        : Translation_Unit_T;
      File_Name : Interfaces.C.Strings.chars_ptr;
      Options   : unsigned)
      return int
   with Import => True,
        Convention => C,
        External_Name => "clang_saveTranslationUnit";
   function Save_Translation_Unit
     (TU        : Translation_Unit_T;
      File_Name : String;
      Options   : unsigned)
      return int
   is
      Return_Value     : int;
      File_Name_Array  : aliased char_array := To_C (File_Name);
      File_Name_String : constant chars_ptr := To_Chars_Ptr (File_Name_Array'Unchecked_Access);
   begin
      Return_Value := Save_Translation_Unit (TU, File_Name_String, Options);
      return Return_Value;
   end Save_Translation_Unit;

   function Get_TU_Resource_Usage_Name
     (Kind : TU_Resource_Usage_Kind_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "clang_getTUResourceUsageName";
   function Get_TU_Resource_Usage_Name
     (Kind : TU_Resource_Usage_Kind_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_TU_Resource_Usage_Name (Kind);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_TU_Resource_Usage_Name;

   function Target_Info_Get_Triple
     (Info : Target_Info_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_TargetInfo_getTriple";
   function Target_Info_Get_Triple
     (Info : Target_Info_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Target_Info_Get_Triple (Info);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Target_Info_Get_Triple;

   function Cursor_Is_Null
     (Cursor : Cursor_T)
      return int
   with Import => True,
        Convention => C,
        External_Name => "clang_Cursor_isNull";
   function Cursor_Is_Null
     (Cursor : Cursor_T)
      return Boolean
   is
      Return_Value : int;
   begin
      Return_Value := Cursor_Is_Null (Cursor);
      return Return_Value /= 0;
   end Cursor_Is_Null;

   function Is_Declaration
     (Arg_1 : Cursor_Kind_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_isDeclaration";
   function Is_Declaration
     (Arg_1 : Cursor_Kind_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Is_Declaration (Arg_1);
      return Return_Value /= 0;
   end Is_Declaration;

   function Is_Invalid_Declaration
     (Arg_1 : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_isInvalidDeclaration";
   function Is_Invalid_Declaration
     (Arg_1 : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Is_Invalid_Declaration (Arg_1);
      return Return_Value /= 0;
   end Is_Invalid_Declaration;

   function Is_Reference
     (Arg_1 : Cursor_Kind_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_isReference";
   function Is_Reference
     (Arg_1 : Cursor_Kind_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Is_Reference (Arg_1);
      return Return_Value /= 0;
   end Is_Reference;

   function Is_Expression
     (Arg_1 : Cursor_Kind_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_isExpression";
   function Is_Expression
     (Arg_1 : Cursor_Kind_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Is_Expression (Arg_1);
      return Return_Value /= 0;
   end Is_Expression;

   function Is_Statement
     (Arg_1 : Cursor_Kind_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_isStatement";
   function Is_Statement
     (Arg_1 : Cursor_Kind_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Is_Statement (Arg_1);
      return Return_Value /= 0;
   end Is_Statement;

   function Is_Attribute
     (Arg_1 : Cursor_Kind_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_isAttribute";
   function Is_Attribute
     (Arg_1 : Cursor_Kind_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Is_Attribute (Arg_1);
      return Return_Value /= 0;
   end Is_Attribute;

   function Is_Invalid
     (Arg_1 : Cursor_Kind_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_isInvalid";
   function Is_Invalid
     (Arg_1 : Cursor_Kind_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Is_Invalid (Arg_1);
      return Return_Value /= 0;
   end Is_Invalid;

   function Is_Translation_Unit
     (Arg_1 : Cursor_Kind_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_isTranslationUnit";
   function Is_Translation_Unit
     (Arg_1 : Cursor_Kind_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Is_Translation_Unit (Arg_1);
      return Return_Value /= 0;
   end Is_Translation_Unit;

   function Is_Preprocessing
     (Arg_1 : Cursor_Kind_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_isPreprocessing";
   function Is_Preprocessing
     (Arg_1 : Cursor_Kind_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Is_Preprocessing (Arg_1);
      return Return_Value /= 0;
   end Is_Preprocessing;

   function Is_Unexposed
     (Arg_1 : Cursor_Kind_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_isUnexposed";
   function Is_Unexposed
     (Arg_1 : Cursor_Kind_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Is_Unexposed (Arg_1);
      return Return_Value /= 0;
   end Is_Unexposed;

   function Get_Type_Spelling
     (CT : Type_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getTypeSpelling";
   function Get_Type_Spelling
     (CT : Type_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Type_Spelling (CT);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Type_Spelling;

   function Is_Const_Qualified_Type
     (T : Type_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_isConstQualifiedType";
   function Is_Const_Qualified_Type
     (T : Type_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Is_Const_Qualified_Type (T);
      return Return_Value /= 0;
   end Is_Const_Qualified_Type;

   function Cursor_Is_Macro_Function_Like
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_Cursor_isMacroFunctionLike";
   function Cursor_Is_Macro_Function_Like
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Cursor_Is_Macro_Function_Like (C);
      return Return_Value /= 0;
   end Cursor_Is_Macro_Function_Like;

   function Cursor_Is_Macro_Builtin
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_Cursor_isMacroBuiltin";
   function Cursor_Is_Macro_Builtin
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Cursor_Is_Macro_Builtin (C);
      return Return_Value /= 0;
   end Cursor_Is_Macro_Builtin;

   function Cursor_Is_Function_Inlined
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_Cursor_isFunctionInlined";
   function Cursor_Is_Function_Inlined
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Cursor_Is_Function_Inlined (C);
      return Return_Value /= 0;
   end Cursor_Is_Function_Inlined;

   function Is_Volatile_Qualified_Type
     (T : Type_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_isVolatileQualifiedType";
   function Is_Volatile_Qualified_Type
     (T : Type_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Is_Volatile_Qualified_Type (T);
      return Return_Value /= 0;
   end Is_Volatile_Qualified_Type;

   function Is_Restrict_Qualified_Type
     (T : Type_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_isRestrictQualifiedType";
   function Is_Restrict_Qualified_Type
     (T : Type_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Is_Restrict_Qualified_Type (T);
      return Return_Value /= 0;
   end Is_Restrict_Qualified_Type;

   function Get_Typedef_Name
     (CT : Type_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getTypedefName";
   function Get_Typedef_Name
     (CT : Type_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Typedef_Name (CT);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Typedef_Name;

   function Get_Decl_Obj_C_Type_Encoding
     (C : Cursor_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getDeclObjCTypeEncoding";
   function Get_Decl_Obj_C_Type_Encoding
     (C : Cursor_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Decl_Obj_C_Type_Encoding (C);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Decl_Obj_C_Type_Encoding;

   function Type_Get_Obj_C_Encoding
     (C_Type : Type_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_Type_getObjCEncoding";
   function Type_Get_Obj_C_Encoding
     (C_Type : Type_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Type_Get_Obj_C_Encoding (C_Type);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Type_Get_Obj_C_Encoding;

   function Get_Type_Kind_Spelling
     (K : Type_Kind_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getTypeKindSpelling";
   function Get_Type_Kind_Spelling
     (K : Type_Kind_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Type_Kind_Spelling (K);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Type_Kind_Spelling;

   function Is_Function_Type_Variadic
     (T : Type_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_isFunctionTypeVariadic";
   function Is_Function_Type_Variadic
     (T : Type_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Is_Function_Type_Variadic (T);
      return Return_Value /= 0;
   end Is_Function_Type_Variadic;

   function Is_POD_Type
     (T : Type_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_isPODType";
   function Is_POD_Type
     (T : Type_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Is_POD_Type (T);
      return Return_Value /= 0;
   end Is_POD_Type;

   function Type_Is_Transparent_Tag_Typedef
     (T : Type_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_Type_isTransparentTagTypedef";
   function Type_Is_Transparent_Tag_Typedef
     (T : Type_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Type_Is_Transparent_Tag_Typedef (T);
      return Return_Value /= 0;
   end Type_Is_Transparent_Tag_Typedef;

   function Type_Get_Offset_Of
     (T : Type_T;
      S : Interfaces.C.Strings.chars_ptr)
      return Long_Long_Integer
   with Import => True,
        Convention => C,
        External_Name => "clang_Type_getOffsetOf";
   function Type_Get_Offset_Of
     (T : Type_T;
      S : String)
      return Long_Long_Integer
   is
      Return_Value : Long_Long_Integer;
      S_Array      : aliased char_array := To_C (S);
      S_String     : constant chars_ptr := To_Chars_Ptr (S_Array'Unchecked_Access);
   begin
      Return_Value := Type_Get_Offset_Of (T, S_String);
      return Return_Value;
   end Type_Get_Offset_Of;

   function Cursor_Is_Anonymous
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_Cursor_isAnonymous";
   function Cursor_Is_Anonymous
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Cursor_Is_Anonymous (C);
      return Return_Value /= 0;
   end Cursor_Is_Anonymous;

   function Cursor_Is_Anonymous_Record_Decl
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_Cursor_isAnonymousRecordDecl";
   function Cursor_Is_Anonymous_Record_Decl
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Cursor_Is_Anonymous_Record_Decl (C);
      return Return_Value /= 0;
   end Cursor_Is_Anonymous_Record_Decl;

   function Cursor_Is_Inline_Namespace
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_Cursor_isInlineNamespace";
   function Cursor_Is_Inline_Namespace
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Cursor_Is_Inline_Namespace (C);
      return Return_Value /= 0;
   end Cursor_Is_Inline_Namespace;

   function Cursor_Is_Bit_Field
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_Cursor_isBitField";
   function Cursor_Is_Bit_Field
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Cursor_Is_Bit_Field (C);
      return Return_Value /= 0;
   end Cursor_Is_Bit_Field;

   function Is_Virtual_Base
     (Arg_1 : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_isVirtualBase";
   function Is_Virtual_Base
     (Arg_1 : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Is_Virtual_Base (Arg_1);
      return Return_Value /= 0;
   end Is_Virtual_Base;

   function Get_Cursor_USR
     (Arg_1 : Cursor_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getCursorUSR";
   function Get_Cursor_USR
     (Arg_1 : Cursor_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Cursor_USR (Arg_1);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Cursor_USR;

   function Construct_USR_Obj_C_Class
     (Class_Name : Interfaces.C.Strings.chars_ptr)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_constructUSR_ObjCClass";
   function Construct_USR_Obj_C_Class
     (Class_Name : String)
      return Clang.CX_String.String_T
   is
      Return_Value      : Clang.CX_String.String_T;
      Class_Name_Array  : aliased char_array := To_C (Class_Name);
      Class_Name_String : constant chars_ptr := To_Chars_Ptr (Class_Name_Array'Unchecked_Access);
   begin
      Return_Value := Construct_USR_Obj_C_Class (Class_Name_String);
      return Return_Value;
   end Construct_USR_Obj_C_Class;

   function Construct_USR_Obj_C_Class
     (Class_Name : String)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Construct_USR_Obj_C_Class (Class_Name);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Construct_USR_Obj_C_Class;

   function Construct_USR_Obj_C_Category
     (Class_Name    : Interfaces.C.Strings.chars_ptr;
      Category_Name : Interfaces.C.Strings.chars_ptr)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_constructUSR_ObjCCategory";
   function Construct_USR_Obj_C_Category
     (Class_Name    : String;
      Category_Name : String)
      return Clang.CX_String.String_T
   is
      Return_Value         : Clang.CX_String.String_T;
      Class_Name_Array     : aliased char_array := To_C (Class_Name);
      Class_Name_String    : constant chars_ptr := To_Chars_Ptr (Class_Name_Array'Unchecked_Access);
      Category_Name_Array  : aliased char_array := To_C (Category_Name);
      Category_Name_String : constant chars_ptr := To_Chars_Ptr (Category_Name_Array'Unchecked_Access);
   begin
      Return_Value := Construct_USR_Obj_C_Category (Class_Name_String, Category_Name_String);
      return Return_Value;
   end Construct_USR_Obj_C_Category;

   function Construct_USR_Obj_C_Category
     (Class_Name    : String;
      Category_Name : String)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Construct_USR_Obj_C_Category (Class_Name, Category_Name);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Construct_USR_Obj_C_Category;

   function Construct_USR_Obj_C_Protocol
     (Protocol_Name : Interfaces.C.Strings.chars_ptr)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_constructUSR_ObjCProtocol";
   function Construct_USR_Obj_C_Protocol
     (Protocol_Name : String)
      return Clang.CX_String.String_T
   is
      Return_Value         : Clang.CX_String.String_T;
      Protocol_Name_Array  : aliased char_array := To_C (Protocol_Name);
      Protocol_Name_String : constant chars_ptr := To_Chars_Ptr (Protocol_Name_Array'Unchecked_Access);
   begin
      Return_Value := Construct_USR_Obj_C_Protocol (Protocol_Name_String);
      return Return_Value;
   end Construct_USR_Obj_C_Protocol;

   function Construct_USR_Obj_C_Protocol
     (Protocol_Name : String)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Construct_USR_Obj_C_Protocol (Protocol_Name);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Construct_USR_Obj_C_Protocol;

   function Construct_USR_Obj_C_Ivar
     (Name      : Interfaces.C.Strings.chars_ptr;
      Class_USR : Clang.CX_String.String_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_constructUSR_ObjCIvar";
   function Construct_USR_Obj_C_Ivar
     (Name      : String;
      Class_USR : Clang.CX_String.String_T)
      return Clang.CX_String.String_T
   is
      Return_Value : Clang.CX_String.String_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Construct_USR_Obj_C_Ivar (Name_String, Class_USR);
      return Return_Value;
   end Construct_USR_Obj_C_Ivar;

   function Construct_USR_Obj_C_Ivar
     (Name      : String;
      Class_USR : Clang.CX_String.String_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Construct_USR_Obj_C_Ivar (Name, Class_USR);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Construct_USR_Obj_C_Ivar;

   function Construct_USR_Obj_C_Method
     (Name               : Interfaces.C.Strings.chars_ptr;
      Is_Instance_Method : unsigned;
      Class_USR          : Clang.CX_String.String_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_constructUSR_ObjCMethod";
   function Construct_USR_Obj_C_Method
     (Name               : String;
      Is_Instance_Method : unsigned;
      Class_USR          : Clang.CX_String.String_T)
      return Clang.CX_String.String_T
   is
      Return_Value : Clang.CX_String.String_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Construct_USR_Obj_C_Method (Name_String, Is_Instance_Method, Class_USR);
      return Return_Value;
   end Construct_USR_Obj_C_Method;

   function Construct_USR_Obj_C_Method
     (Name               : String;
      Is_Instance_Method : unsigned;
      Class_USR          : Clang.CX_String.String_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Construct_USR_Obj_C_Method (Name, Is_Instance_Method, Class_USR);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Construct_USR_Obj_C_Method;

   function Construct_USR_Obj_C_Property
     (Property  : Interfaces.C.Strings.chars_ptr;
      Class_USR : Clang.CX_String.String_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_constructUSR_ObjCProperty";
   function Construct_USR_Obj_C_Property
     (Property  : String;
      Class_USR : Clang.CX_String.String_T)
      return Clang.CX_String.String_T
   is
      Return_Value    : Clang.CX_String.String_T;
      Property_Array  : aliased char_array := To_C (Property);
      Property_String : constant chars_ptr := To_Chars_Ptr (Property_Array'Unchecked_Access);
   begin
      Return_Value := Construct_USR_Obj_C_Property (Property_String, Class_USR);
      return Return_Value;
   end Construct_USR_Obj_C_Property;

   function Construct_USR_Obj_C_Property
     (Property  : String;
      Class_USR : Clang.CX_String.String_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Construct_USR_Obj_C_Property (Property, Class_USR);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Construct_USR_Obj_C_Property;

   function Get_Cursor_Spelling
     (Arg_1 : Cursor_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getCursorSpelling";
   function Get_Cursor_Spelling
     (Arg_1 : Cursor_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Cursor_Spelling (Arg_1);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Cursor_Spelling;

   function Get_Cursor_Pretty_Printed
     (Cursor : Cursor_T;
      Policy : Printing_Policy_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getCursorPrettyPrinted";
   function Get_Cursor_Pretty_Printed
     (Cursor : Cursor_T;
      Policy : Printing_Policy_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Cursor_Pretty_Printed (Cursor, Policy);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Cursor_Pretty_Printed;

   function Get_Cursor_Display_Name
     (Arg_1 : Cursor_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getCursorDisplayName";
   function Get_Cursor_Display_Name
     (Arg_1 : Cursor_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Cursor_Display_Name (Arg_1);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Cursor_Display_Name;

   function Is_Cursor_Definition
     (Arg_1 : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_isCursorDefinition";
   function Is_Cursor_Definition
     (Arg_1 : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Is_Cursor_Definition (Arg_1);
      return Return_Value /= 0;
   end Is_Cursor_Definition;

   function Cursor_Is_Dynamic_Call
     (C : Cursor_T)
      return int
   with Import => True,
        Convention => C,
        External_Name => "clang_Cursor_isDynamicCall";
   function Cursor_Is_Dynamic_Call
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : int;
   begin
      Return_Value := Cursor_Is_Dynamic_Call (C);
      return Return_Value /= 0;
   end Cursor_Is_Dynamic_Call;

   function Cursor_Get_Obj_C_Property_Getter_Name
     (C : Cursor_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_Cursor_getObjCPropertyGetterName";
   function Cursor_Get_Obj_C_Property_Getter_Name
     (C : Cursor_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Cursor_Get_Obj_C_Property_Getter_Name (C);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Cursor_Get_Obj_C_Property_Getter_Name;

   function Cursor_Get_Obj_C_Property_Setter_Name
     (C : Cursor_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_Cursor_getObjCPropertySetterName";
   function Cursor_Get_Obj_C_Property_Setter_Name
     (C : Cursor_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Cursor_Get_Obj_C_Property_Setter_Name (C);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Cursor_Get_Obj_C_Property_Setter_Name;

   function Cursor_Is_Obj_C_Optional
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_Cursor_isObjCOptional";
   function Cursor_Is_Obj_C_Optional
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Cursor_Is_Obj_C_Optional (C);
      return Return_Value /= 0;
   end Cursor_Is_Obj_C_Optional;

   function Cursor_Is_Variadic
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_Cursor_isVariadic";
   function Cursor_Is_Variadic
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Cursor_Is_Variadic (C);
      return Return_Value /= 0;
   end Cursor_Is_Variadic;

   function Cursor_Is_External_Symbol
     (C            : Cursor_T;
      Language     : access Clang.CX_String.String_T;
      Defined_In   : access Clang.CX_String.String_T;
      Is_Generated : access unsigned)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_Cursor_isExternalSymbol";
   function Cursor_Is_External_Symbol
     (C            : Cursor_T;
      Language     : access Clang.CX_String.String_T;
      Defined_In   : access Clang.CX_String.String_T;
      Is_Generated : access unsigned)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Cursor_Is_External_Symbol (C, Language, Defined_In, Is_Generated);
      return Return_Value /= 0;
   end Cursor_Is_External_Symbol;

   function Cursor_Get_Raw_Comment_Text
     (C : Cursor_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_Cursor_getRawCommentText";
   function Cursor_Get_Raw_Comment_Text
     (C : Cursor_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Cursor_Get_Raw_Comment_Text (C);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Cursor_Get_Raw_Comment_Text;

   function Cursor_Get_Brief_Comment_Text
     (C : Cursor_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_Cursor_getBriefCommentText";
   function Cursor_Get_Brief_Comment_Text
     (C : Cursor_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Cursor_Get_Brief_Comment_Text (C);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Cursor_Get_Brief_Comment_Text;

   function Cursor_Get_Mangling
     (Arg_1 : Cursor_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_Cursor_getMangling";
   function Cursor_Get_Mangling
     (Arg_1 : Cursor_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Cursor_Get_Mangling (Arg_1);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Cursor_Get_Mangling;

   function Module_Get_Name
     (Module : Module_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_Module_getName";
   function Module_Get_Name
     (Module : Module_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Module_Get_Name (Module);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Module_Get_Name;

   function Module_Get_Full_Name
     (Module : Module_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_Module_getFullName";
   function Module_Get_Full_Name
     (Module : Module_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Module_Get_Full_Name (Module);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Module_Get_Full_Name;

   function Module_Is_System
     (Module : Module_T)
      return int
   with Import => True,
        Convention => C,
        External_Name => "clang_Module_isSystem";
   function Module_Is_System
     (Module : Module_T)
      return Boolean
   is
      Return_Value : int;
   begin
      Return_Value := Module_Is_System (Module);
      return Return_Value /= 0;
   end Module_Is_System;

   function CXX_Constructor_Is_Converting_Constructor
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_CXXConstructor_isConvertingConstructor";
   function CXX_Constructor_Is_Converting_Constructor
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := CXX_Constructor_Is_Converting_Constructor (C);
      return Return_Value /= 0;
   end CXX_Constructor_Is_Converting_Constructor;

   function CXX_Constructor_Is_Copy_Constructor
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_CXXConstructor_isCopyConstructor";
   function CXX_Constructor_Is_Copy_Constructor
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := CXX_Constructor_Is_Copy_Constructor (C);
      return Return_Value /= 0;
   end CXX_Constructor_Is_Copy_Constructor;

   function CXX_Constructor_Is_Default_Constructor
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_CXXConstructor_isDefaultConstructor";
   function CXX_Constructor_Is_Default_Constructor
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := CXX_Constructor_Is_Default_Constructor (C);
      return Return_Value /= 0;
   end CXX_Constructor_Is_Default_Constructor;

   function CXX_Constructor_Is_Move_Constructor
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_CXXConstructor_isMoveConstructor";
   function CXX_Constructor_Is_Move_Constructor
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := CXX_Constructor_Is_Move_Constructor (C);
      return Return_Value /= 0;
   end CXX_Constructor_Is_Move_Constructor;

   function CXX_Field_Is_Mutable
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_CXXField_isMutable";
   function CXX_Field_Is_Mutable
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := CXX_Field_Is_Mutable (C);
      return Return_Value /= 0;
   end CXX_Field_Is_Mutable;

   function CXX_Method_Is_Defaulted
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_CXXMethod_isDefaulted";
   function CXX_Method_Is_Defaulted
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := CXX_Method_Is_Defaulted (C);
      return Return_Value /= 0;
   end CXX_Method_Is_Defaulted;

   function CXX_Method_Is_Deleted
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_CXXMethod_isDeleted";
   function CXX_Method_Is_Deleted
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := CXX_Method_Is_Deleted (C);
      return Return_Value /= 0;
   end CXX_Method_Is_Deleted;

   function CXX_Method_Is_Pure_Virtual
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_CXXMethod_isPureVirtual";
   function CXX_Method_Is_Pure_Virtual
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := CXX_Method_Is_Pure_Virtual (C);
      return Return_Value /= 0;
   end CXX_Method_Is_Pure_Virtual;

   function CXX_Method_Is_Static
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_CXXMethod_isStatic";
   function CXX_Method_Is_Static
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := CXX_Method_Is_Static (C);
      return Return_Value /= 0;
   end CXX_Method_Is_Static;

   function CXX_Method_Is_Virtual
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_CXXMethod_isVirtual";
   function CXX_Method_Is_Virtual
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := CXX_Method_Is_Virtual (C);
      return Return_Value /= 0;
   end CXX_Method_Is_Virtual;

   function CXX_Method_Is_Copy_Assignment_Operator
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_CXXMethod_isCopyAssignmentOperator";
   function CXX_Method_Is_Copy_Assignment_Operator
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := CXX_Method_Is_Copy_Assignment_Operator (C);
      return Return_Value /= 0;
   end CXX_Method_Is_Copy_Assignment_Operator;

   function CXX_Method_Is_Move_Assignment_Operator
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_CXXMethod_isMoveAssignmentOperator";
   function CXX_Method_Is_Move_Assignment_Operator
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := CXX_Method_Is_Move_Assignment_Operator (C);
      return Return_Value /= 0;
   end CXX_Method_Is_Move_Assignment_Operator;

   function CXX_Record_Is_Abstract
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_CXXRecord_isAbstract";
   function CXX_Record_Is_Abstract
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := CXX_Record_Is_Abstract (C);
      return Return_Value /= 0;
   end CXX_Record_Is_Abstract;

   function Enum_Decl_Is_Scoped
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_EnumDecl_isScoped";
   function Enum_Decl_Is_Scoped
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Enum_Decl_Is_Scoped (C);
      return Return_Value /= 0;
   end Enum_Decl_Is_Scoped;

   function CXX_Method_Is_Const
     (C : Cursor_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_CXXMethod_isConst";
   function CXX_Method_Is_Const
     (C : Cursor_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := CXX_Method_Is_Const (C);
      return Return_Value /= 0;
   end CXX_Method_Is_Const;

   function Get_Token_Spelling
     (Arg_1 : Translation_Unit_T;
      Arg_2 : Token_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getTokenSpelling";
   function Get_Token_Spelling
     (Arg_1 : Translation_Unit_T;
      Arg_2 : Token_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Token_Spelling (Arg_1, Arg_2);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Token_Spelling;

   function Get_Cursor_Kind_Spelling
     (Kind : Cursor_Kind_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getCursorKindSpelling";
   function Get_Cursor_Kind_Spelling
     (Kind : Cursor_Kind_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Cursor_Kind_Spelling (Kind);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Cursor_Kind_Spelling;

   function Get_Completion_Chunk_Text
     (Completion_String : Completion_String_T;
      Chunk_Number      : unsigned)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getCompletionChunkText";
   function Get_Completion_Chunk_Text
     (Completion_String : Completion_String_T;
      Chunk_Number      : unsigned)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Completion_Chunk_Text (Completion_String, Chunk_Number);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Completion_Chunk_Text;

   function Get_Completion_Annotation
     (Completion_String : Completion_String_T;
      Annotation_Number : unsigned)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getCompletionAnnotation";
   function Get_Completion_Annotation
     (Completion_String : Completion_String_T;
      Annotation_Number : unsigned)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Completion_Annotation (Completion_String, Annotation_Number);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Completion_Annotation;

   function Get_Completion_Parent
     (Completion_String : Completion_String_T;
      Kind              : access Cursor_Kind_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getCompletionParent";
   function Get_Completion_Parent
     (Completion_String : Completion_String_T;
      Kind              : access Cursor_Kind_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Completion_Parent (Completion_String, Kind);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Completion_Parent;

   function Get_Completion_Brief_Comment
     (Completion_String : Completion_String_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getCompletionBriefComment";
   function Get_Completion_Brief_Comment
     (Completion_String : Completion_String_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Completion_Brief_Comment (Completion_String);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Completion_Brief_Comment;

   function Get_Completion_Fix_It
     (Results           : access Code_Complete_Results_T;
      Completion_Index  : unsigned;
      Fixit_Index       : unsigned;
      Replacement_Range : access Clang.CX_Source_Location.Source_Range_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getCompletionFixIt";
   function Get_Completion_Fix_It
     (Results           : access Code_Complete_Results_T;
      Completion_Index  : unsigned;
      Fixit_Index       : unsigned;
      Replacement_Range : access Clang.CX_Source_Location.Source_Range_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Completion_Fix_It (Results, Completion_Index, Fixit_Index, Replacement_Range);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Completion_Fix_It;

   function Code_Complete_At
     (TU                : Translation_Unit_T;
      Complete_Filename : Interfaces.C.Strings.chars_ptr;
      Complete_Line     : unsigned;
      Complete_Column   : unsigned;
      Unsaved_Files     : access Unsaved_File_T;
      Num_Unsaved_Files : unsigned;
      Options           : unsigned)
      return access Code_Complete_Results_T
   with Import => True,
        Convention => C,
        External_Name => "clang_codeCompleteAt";
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
      Return_Value             : access Code_Complete_Results_T;
      Complete_Filename_Array  : aliased char_array := To_C (Complete_Filename);
      Complete_Filename_String : constant chars_ptr := To_Chars_Ptr (Complete_Filename_Array'Unchecked_Access);
   begin
      Return_Value := Code_Complete_At (TU, Complete_Filename_String, Complete_Line, Complete_Column, Unsaved_Files, Num_Unsaved_Files, Options);
      return Return_Value;
   end Code_Complete_At;

   function Code_Complete_Get_Container_USR
     (Results : access Code_Complete_Results_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_codeCompleteGetContainerUSR";
   function Code_Complete_Get_Container_USR
     (Results : access Code_Complete_Results_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Code_Complete_Get_Container_USR (Results);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Code_Complete_Get_Container_USR;

   function Code_Complete_Get_Obj_C_Selector
     (Results : access Code_Complete_Results_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_codeCompleteGetObjCSelector";
   function Code_Complete_Get_Obj_C_Selector
     (Results : access Code_Complete_Results_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Code_Complete_Get_Obj_C_Selector (Results);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Code_Complete_Get_Obj_C_Selector;

   function Get_Clang_Version
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getClangVersion";
   function Get_Clang_Version
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Clang_Version;
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Clang_Version;

   function Eval_Result_Is_Unsigned_Int
     (E : Eval_Result_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_EvalResult_isUnsignedInt";
   function Eval_Result_Is_Unsigned_Int
     (E : Eval_Result_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Eval_Result_Is_Unsigned_Int (E);
      return Return_Value /= 0;
   end Eval_Result_Is_Unsigned_Int;

   function Eval_Result_Get_As_Str
     (E : Eval_Result_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "clang_EvalResult_getAsStr";
   function Eval_Result_Get_As_Str
     (E : Eval_Result_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Eval_Result_Get_As_Str (E);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Eval_Result_Get_As_Str;

   function Get_Remappings
     (Path : Interfaces.C.Strings.chars_ptr)
      return Remapping_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getRemappings";
   function Get_Remappings
     (Path : String)
      return Remapping_T
   is
      Return_Value : Remapping_T;
      Path_Array   : aliased char_array := To_C (Path);
      Path_String  : constant chars_ptr := To_Chars_Ptr (Path_Array'Unchecked_Access);
   begin
      Return_Value := Get_Remappings (Path_String);
      return Return_Value;
   end Get_Remappings;

   function Index_Is_Entity_Obj_C_Container_Kind
     (Arg_1 : Idx_Entity_Kind_T)
      return int
   with Import => True,
        Convention => C,
        External_Name => "clang_index_isEntityObjCContainerKind";
   function Index_Is_Entity_Obj_C_Container_Kind
     (Arg_1 : Idx_Entity_Kind_T)
      return Boolean
   is
      Return_Value : int;
   begin
      Return_Value := Index_Is_Entity_Obj_C_Container_Kind (Arg_1);
      return Return_Value /= 0;
   end Index_Is_Entity_Obj_C_Container_Kind;

   function Index_Source_File
     (Arg_1                 : Index_Action_T;
      Client_Data           : Client_Data_T;
      Index_Callbacks       : access IndexerCallbacks;
      Index_Callbacks_Size  : unsigned;
      Index_Options         : unsigned;
      Source_Filename       : Interfaces.C.Strings.chars_ptr;
      Command_Line_Args     : System.Address;
      Num_Command_Line_Args : int;
      Unsaved_Files         : access Unsaved_File_T;
      Num_Unsaved_Files     : unsigned;
      Out_TU                : System.Address;
      TU_Options            : unsigned)
      return int
   with Import => True,
        Convention => C,
        External_Name => "clang_indexSourceFile";
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
      Return_Value           : int;
      Source_Filename_Array  : aliased char_array := To_C (Source_Filename);
      Source_Filename_String : constant chars_ptr := To_Chars_Ptr (Source_Filename_Array'Unchecked_Access);
   begin
      Return_Value := Index_Source_File (Arg_1, Client_Data, Index_Callbacks, Index_Callbacks_Size, Index_Options, Source_Filename_String, Command_Line_Args, Num_Command_Line_Args, Unsaved_Files, Num_Unsaved_Files, Out_TU, TU_Options);
      return Return_Value;
   end Index_Source_File;

   function Index_Source_File_Full_Argv
     (Arg_1                 : Index_Action_T;
      Client_Data           : Client_Data_T;
      Index_Callbacks       : access IndexerCallbacks;
      Index_Callbacks_Size  : unsigned;
      Index_Options         : unsigned;
      Source_Filename       : Interfaces.C.Strings.chars_ptr;
      Command_Line_Args     : System.Address;
      Num_Command_Line_Args : int;
      Unsaved_Files         : access Unsaved_File_T;
      Num_Unsaved_Files     : unsigned;
      Out_TU                : System.Address;
      TU_Options            : unsigned)
      return int
   with Import => True,
        Convention => C,
        External_Name => "clang_indexSourceFileFullArgv";
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
      Return_Value           : int;
      Source_Filename_Array  : aliased char_array := To_C (Source_Filename);
      Source_Filename_String : constant chars_ptr := To_Chars_Ptr (Source_Filename_Array'Unchecked_Access);
   begin
      Return_Value := Index_Source_File_Full_Argv (Arg_1, Client_Data, Index_Callbacks, Index_Callbacks_Size, Index_Options, Source_Filename_String, Command_Line_Args, Num_Command_Line_Args, Unsaved_Files, Num_Unsaved_Files, Out_TU, TU_Options);
      return Return_Value;
   end Index_Source_File_Full_Argv;

end Clang.Index;
