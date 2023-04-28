pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Core is

   function Create_Message
     (Message : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMCreateMessage";
   function Create_Message
     (Message : String)
      return String
   is
      Return_Value   : Interfaces.C.Strings.chars_ptr;
      Message_Array  : aliased char_array := To_C (Message);
      Message_String : constant chars_ptr := To_Chars_Ptr (Message_Array'Unchecked_Access);
   begin
      Return_Value := Create_Message (Message_String);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Create_Message;

   procedure Dispose_Message
     (Message : Interfaces.C.Strings.chars_ptr)
   with Import => True,
        Convention => C,
        External_Name => "LLVMDisposeMessage";
   procedure Dispose_Message
     (Message : String)
   is
      Message_Array  : aliased char_array := To_C (Message);
      Message_String : constant chars_ptr := To_Chars_Ptr (Message_Array'Unchecked_Access);
   begin
      Dispose_Message (Message_String);
   end Dispose_Message;

   function Context_Should_Discard_Value_Names
     (C : LLVM.Types.Context_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMContextShouldDiscardValueNames";
   function Context_Should_Discard_Value_Names
     (C : LLVM.Types.Context_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Context_Should_Discard_Value_Names (C);
      return Return_Value /= 0;
   end Context_Should_Discard_Value_Names;

   procedure Context_Set_Discard_Value_Names
     (C       : LLVM.Types.Context_T;
      Discard : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMContextSetDiscardValueNames";
   procedure Context_Set_Discard_Value_Names
     (C       : LLVM.Types.Context_T;
      Discard : Boolean)
   is
      Discard_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Discard);
   begin
      Context_Set_Discard_Value_Names (C, Discard_Bool);
   end Context_Set_Discard_Value_Names;

   procedure Context_Set_Opaque_Pointers
     (C               : LLVM.Types.Context_T;
      Opaque_Pointers : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMContextSetOpaquePointers";
   procedure Context_Set_Opaque_Pointers
     (C               : LLVM.Types.Context_T;
      Opaque_Pointers : Boolean)
   is
      Opaque_Pointers_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Opaque_Pointers);
   begin
      Context_Set_Opaque_Pointers (C, Opaque_Pointers_Bool);
   end Context_Set_Opaque_Pointers;

   function Get_Diag_Info_Description
     (DI : LLVM.Types.Diagnostic_Info_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetDiagInfoDescription";
   function Get_Diag_Info_Description
     (DI : LLVM.Types.Diagnostic_Info_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Diag_Info_Description (DI);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Diag_Info_Description;

   function Get_MD_Kind_ID_In_Context
     (C     : LLVM.Types.Context_T;
      Name  : Interfaces.C.Strings.chars_ptr;
      S_Len : unsigned)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetMDKindIDInContext";
   function Get_MD_Kind_ID_In_Context
     (C     : LLVM.Types.Context_T;
      Name  : String;
      S_Len : unsigned)
      return unsigned
   is
      Return_Value : unsigned;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Get_MD_Kind_ID_In_Context (C, Name_String, S_Len);
      return Return_Value;
   end Get_MD_Kind_ID_In_Context;

   function Get_MD_Kind_ID
     (Name  : Interfaces.C.Strings.chars_ptr;
      S_Len : unsigned)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetMDKindID";
   function Get_MD_Kind_ID
     (Name  : String;
      S_Len : unsigned)
      return unsigned
   is
      Return_Value : unsigned;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Get_MD_Kind_ID (Name_String, S_Len);
      return Return_Value;
   end Get_MD_Kind_ID;

   function Get_Enum_Attribute_Kind_For_Name
     (Name  : Interfaces.C.Strings.chars_ptr;
      S_Len : stddef_h.size_t)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetEnumAttributeKindForName";
   function Get_Enum_Attribute_Kind_For_Name
     (Name  : String;
      S_Len : stddef_h.size_t)
      return unsigned
   is
      Return_Value : unsigned;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Get_Enum_Attribute_Kind_For_Name (Name_String, S_Len);
      return Return_Value;
   end Get_Enum_Attribute_Kind_For_Name;

   function Create_String_Attribute
     (C        : LLVM.Types.Context_T;
      K        : Interfaces.C.Strings.chars_ptr;
      K_Length : unsigned;
      V        : Interfaces.C.Strings.chars_ptr;
      V_Length : unsigned)
      return LLVM.Types.Attribute_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMCreateStringAttribute";
   function Create_String_Attribute
     (C        : LLVM.Types.Context_T;
      K        : String;
      K_Length : unsigned;
      V        : String;
      V_Length : unsigned)
      return LLVM.Types.Attribute_T
   is
      Return_Value : LLVM.Types.Attribute_T;
      K_Array      : aliased char_array := To_C (K);
      K_String     : constant chars_ptr := To_Chars_Ptr (K_Array'Unchecked_Access);
      V_Array      : aliased char_array := To_C (V);
      V_String     : constant chars_ptr := To_Chars_Ptr (V_Array'Unchecked_Access);
   begin
      Return_Value := Create_String_Attribute (C, K_String, K_Length, V_String, V_Length);
      return Return_Value;
   end Create_String_Attribute;

   function Get_String_Attribute_Kind
     (A      : LLVM.Types.Attribute_T;
      Length : access unsigned)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetStringAttributeKind";
   function Get_String_Attribute_Kind
     (A      : LLVM.Types.Attribute_T;
      Length : access unsigned)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_String_Attribute_Kind (A, Length);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_String_Attribute_Kind;

   function Get_String_Attribute_Value
     (A      : LLVM.Types.Attribute_T;
      Length : access unsigned)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetStringAttributeValue";
   function Get_String_Attribute_Value
     (A      : LLVM.Types.Attribute_T;
      Length : access unsigned)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_String_Attribute_Value (A, Length);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_String_Attribute_Value;

   function Is_Enum_Attribute
     (A : LLVM.Types.Attribute_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsEnumAttribute";
   function Is_Enum_Attribute
     (A : LLVM.Types.Attribute_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Enum_Attribute (A);
      return Return_Value /= 0;
   end Is_Enum_Attribute;

   function Is_String_Attribute
     (A : LLVM.Types.Attribute_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsStringAttribute";
   function Is_String_Attribute
     (A : LLVM.Types.Attribute_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_String_Attribute (A);
      return Return_Value /= 0;
   end Is_String_Attribute;

   function Is_Type_Attribute
     (A : LLVM.Types.Attribute_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsTypeAttribute";
   function Is_Type_Attribute
     (A : LLVM.Types.Attribute_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Type_Attribute (A);
      return Return_Value /= 0;
   end Is_Type_Attribute;

   function Get_Type_By_Name_2
     (C    : LLVM.Types.Context_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Type_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetTypeByName2";
   function Get_Type_By_Name_2
     (C    : LLVM.Types.Context_T;
      Name : String)
      return LLVM.Types.Type_T
   is
      Return_Value : LLVM.Types.Type_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Get_Type_By_Name_2 (C, Name_String);
      return Return_Value;
   end Get_Type_By_Name_2;

   function Module_Create_With_Name
     (Module_ID : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Module_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMModuleCreateWithName";
   function Module_Create_With_Name
     (Module_ID : String)
      return LLVM.Types.Module_T
   is
      Return_Value     : LLVM.Types.Module_T;
      Module_ID_Array  : aliased char_array := To_C (Module_ID);
      Module_ID_String : constant chars_ptr := To_Chars_Ptr (Module_ID_Array'Unchecked_Access);
   begin
      Return_Value := Module_Create_With_Name (Module_ID_String);
      return Return_Value;
   end Module_Create_With_Name;

   function Module_Create_With_Name_In_Context
     (Module_ID : Interfaces.C.Strings.chars_ptr;
      C         : LLVM.Types.Context_T)
      return LLVM.Types.Module_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMModuleCreateWithNameInContext";
   function Module_Create_With_Name_In_Context
     (Module_ID : String;
      C         : LLVM.Types.Context_T)
      return LLVM.Types.Module_T
   is
      Return_Value     : LLVM.Types.Module_T;
      Module_ID_Array  : aliased char_array := To_C (Module_ID);
      Module_ID_String : constant chars_ptr := To_Chars_Ptr (Module_ID_Array'Unchecked_Access);
   begin
      Return_Value := Module_Create_With_Name_In_Context (Module_ID_String, C);
      return Return_Value;
   end Module_Create_With_Name_In_Context;

   function Get_Module_Identifier
     (M   : LLVM.Types.Module_T;
      Len : access stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetModuleIdentifier";
   function Get_Module_Identifier
     (M   : LLVM.Types.Module_T;
      Len : access stddef_h.size_t)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Module_Identifier (M, Len);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Module_Identifier;

   procedure Set_Module_Identifier
     (M     : LLVM.Types.Module_T;
      Ident : Interfaces.C.Strings.chars_ptr;
      Len   : stddef_h.size_t)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetModuleIdentifier";
   procedure Set_Module_Identifier
     (M     : LLVM.Types.Module_T;
      Ident : String;
      Len   : stddef_h.size_t)
   is
      Ident_Array  : aliased char_array := To_C (Ident);
      Ident_String : constant chars_ptr := To_Chars_Ptr (Ident_Array'Unchecked_Access);
   begin
      Set_Module_Identifier (M, Ident_String, Len);
   end Set_Module_Identifier;

   function Get_Source_File_Name
     (M   : LLVM.Types.Module_T;
      Len : access stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetSourceFileName";
   function Get_Source_File_Name
     (M   : LLVM.Types.Module_T;
      Len : access stddef_h.size_t)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Source_File_Name (M, Len);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Source_File_Name;

   procedure Set_Source_File_Name
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr;
      Len  : stddef_h.size_t)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetSourceFileName";
   procedure Set_Source_File_Name
     (M    : LLVM.Types.Module_T;
      Name : String;
      Len  : stddef_h.size_t)
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Set_Source_File_Name (M, Name_String, Len);
   end Set_Source_File_Name;

   function Get_Data_Layout_Str
     (M : LLVM.Types.Module_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetDataLayoutStr";
   function Get_Data_Layout_Str
     (M : LLVM.Types.Module_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Data_Layout_Str (M);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Data_Layout_Str;

   function Get_Data_Layout
     (M : LLVM.Types.Module_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetDataLayout";
   function Get_Data_Layout
     (M : LLVM.Types.Module_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Data_Layout (M);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Data_Layout;

   procedure Set_Data_Layout
     (M               : LLVM.Types.Module_T;
      Data_Layout_Str : Interfaces.C.Strings.chars_ptr)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetDataLayout";
   procedure Set_Data_Layout
     (M               : LLVM.Types.Module_T;
      Data_Layout_Str : String)
   is
      Data_Layout_Str_Array  : aliased char_array := To_C (Data_Layout_Str);
      Data_Layout_Str_String : constant chars_ptr := To_Chars_Ptr (Data_Layout_Str_Array'Unchecked_Access);
   begin
      Set_Data_Layout (M, Data_Layout_Str_String);
   end Set_Data_Layout;

   function Get_Target
     (M : LLVM.Types.Module_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetTarget";
   function Get_Target
     (M : LLVM.Types.Module_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Target (M);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Target;

   procedure Set_Target
     (M      : LLVM.Types.Module_T;
      Triple : Interfaces.C.Strings.chars_ptr)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetTarget";
   procedure Set_Target
     (M      : LLVM.Types.Module_T;
      Triple : String)
   is
      Triple_Array  : aliased char_array := To_C (Triple);
      Triple_String : constant chars_ptr := To_Chars_Ptr (Triple_Array'Unchecked_Access);
   begin
      Set_Target (M, Triple_String);
   end Set_Target;

   function Module_Flag_Entries_Get_Key
     (Entries : access LLVM.Types.Opaque_Module_Flag_Entry_Impl_T;
      Index   : unsigned;
      Len     : access stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMModuleFlagEntriesGetKey";
   function Module_Flag_Entries_Get_Key
     (Entries : access LLVM.Types.Opaque_Module_Flag_Entry_Impl_T;
      Index   : unsigned;
      Len     : access stddef_h.size_t)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Module_Flag_Entries_Get_Key (Entries, Index, Len);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Module_Flag_Entries_Get_Key;

   function Get_Module_Flag
     (M       : LLVM.Types.Module_T;
      Key     : Interfaces.C.Strings.chars_ptr;
      Key_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetModuleFlag";
   function Get_Module_Flag
     (M       : LLVM.Types.Module_T;
      Key     : String;
      Key_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value : LLVM.Types.Metadata_T;
      Key_Array    : aliased char_array := To_C (Key);
      Key_String   : constant chars_ptr := To_Chars_Ptr (Key_Array'Unchecked_Access);
   begin
      Return_Value := Get_Module_Flag (M, Key_String, Key_Len);
      return Return_Value;
   end Get_Module_Flag;

   procedure Add_Module_Flag
     (M        : LLVM.Types.Module_T;
      Behavior : Module_Flag_Behavior_T;
      Key      : Interfaces.C.Strings.chars_ptr;
      Key_Len  : stddef_h.size_t;
      Val      : LLVM.Types.Metadata_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMAddModuleFlag";
   procedure Add_Module_Flag
     (M        : LLVM.Types.Module_T;
      Behavior : Module_Flag_Behavior_T;
      Key      : String;
      Key_Len  : stddef_h.size_t;
      Val      : LLVM.Types.Metadata_T)
   is
      Key_Array  : aliased char_array := To_C (Key);
      Key_String : constant chars_ptr := To_Chars_Ptr (Key_Array'Unchecked_Access);
   begin
      Add_Module_Flag (M, Behavior, Key_String, Key_Len, Val);
   end Add_Module_Flag;

   function Print_Module_To_File
     (M             : LLVM.Types.Module_T;
      Filename      : Interfaces.C.Strings.chars_ptr;
      Error_Message : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMPrintModuleToFile";
   function Print_Module_To_File
     (M             : LLVM.Types.Module_T;
      Filename      : String;
      Error_Message : System.Address)
      return LLVM.Types.Bool_T
   is
      Return_Value    : LLVM.Types.Bool_T;
      Filename_Array  : aliased char_array := To_C (Filename);
      Filename_String : constant chars_ptr := To_Chars_Ptr (Filename_Array'Unchecked_Access);
   begin
      Return_Value := Print_Module_To_File (M, Filename_String, Error_Message);
      return Return_Value;
   end Print_Module_To_File;

   function Print_Module_To_File
     (M             : LLVM.Types.Module_T;
      Filename      : String;
      Error_Message : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Print_Module_To_File (M, Filename, Error_Message);
      return Return_Value /= 0;
   end Print_Module_To_File;

   function Print_Module_To_String
     (M : LLVM.Types.Module_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMPrintModuleToString";
   function Print_Module_To_String
     (M : LLVM.Types.Module_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Print_Module_To_String (M);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Print_Module_To_String;

   function Get_Module_Inline_Asm
     (M   : LLVM.Types.Module_T;
      Len : access stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetModuleInlineAsm";
   function Get_Module_Inline_Asm
     (M   : LLVM.Types.Module_T;
      Len : access stddef_h.size_t)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Module_Inline_Asm (M, Len);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Module_Inline_Asm;

   procedure Set_Module_Inline_Asm_2
     (M   : LLVM.Types.Module_T;
      Asm : Interfaces.C.Strings.chars_ptr;
      Len : stddef_h.size_t)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetModuleInlineAsm2";
   procedure Set_Module_Inline_Asm_2
     (M   : LLVM.Types.Module_T;
      Asm : String;
      Len : stddef_h.size_t)
   is
      Asm_Array  : aliased char_array := To_C (Asm);
      Asm_String : constant chars_ptr := To_Chars_Ptr (Asm_Array'Unchecked_Access);
   begin
      Set_Module_Inline_Asm_2 (M, Asm_String, Len);
   end Set_Module_Inline_Asm_2;

   procedure Append_Module_Inline_Asm
     (M   : LLVM.Types.Module_T;
      Asm : Interfaces.C.Strings.chars_ptr;
      Len : stddef_h.size_t)
   with Import => True,
        Convention => C,
        External_Name => "LLVMAppendModuleInlineAsm";
   procedure Append_Module_Inline_Asm
     (M   : LLVM.Types.Module_T;
      Asm : String;
      Len : stddef_h.size_t)
   is
      Asm_Array  : aliased char_array := To_C (Asm);
      Asm_String : constant chars_ptr := To_Chars_Ptr (Asm_Array'Unchecked_Access);
   begin
      Append_Module_Inline_Asm (M, Asm_String, Len);
   end Append_Module_Inline_Asm;

   function Get_Inline_Asm
     (Ty               : LLVM.Types.Type_T;
      Asm_String       : Interfaces.C.Strings.chars_ptr;
      Asm_String_Size  : stddef_h.size_t;
      Constraints      : Interfaces.C.Strings.chars_ptr;
      Constraints_Size : stddef_h.size_t;
      Has_Side_Effects : LLVM.Types.Bool_T;
      Is_Align_Stack   : LLVM.Types.Bool_T;
      Dialect          : Inline_Asm_Dialect_T;
      Can_Throw        : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetInlineAsm";
   function Get_Inline_Asm
     (Ty               : LLVM.Types.Type_T;
      Asm_String       : String;
      Asm_String_Size  : stddef_h.size_t;
      Constraints      : String;
      Constraints_Size : stddef_h.size_t;
      Has_Side_Effects : LLVM.Types.Bool_T;
      Is_Align_Stack   : LLVM.Types.Bool_T;
      Dialect          : Inline_Asm_Dialect_T;
      Can_Throw        : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T
   is
      Return_Value       : LLVM.Types.Value_T;
      Asm_String_Array   : aliased char_array := To_C (Asm_String);
      Asm_String_String  : constant chars_ptr := To_Chars_Ptr (Asm_String_Array'Unchecked_Access);
      Constraints_Array  : aliased char_array := To_C (Constraints);
      Constraints_String : constant chars_ptr := To_Chars_Ptr (Constraints_Array'Unchecked_Access);
   begin
      Return_Value := Get_Inline_Asm (Ty, Asm_String_String, Asm_String_Size, Constraints_String, Constraints_Size, Has_Side_Effects, Is_Align_Stack, Dialect, Can_Throw);
      return Return_Value;
   end Get_Inline_Asm;

   function Get_Inline_Asm
     (Ty               : LLVM.Types.Type_T;
      Asm_String       : String;
      Asm_String_Size  : stddef_h.size_t;
      Constraints      : String;
      Constraints_Size : stddef_h.size_t;
      Has_Side_Effects : Boolean;
      Is_Align_Stack   : Boolean;
      Dialect          : Inline_Asm_Dialect_T;
      Can_Throw        : Boolean)
      return LLVM.Types.Value_T
   is
      Return_Value          : LLVM.Types.Value_T;
      Has_Side_Effects_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Has_Side_Effects);
      Is_Align_Stack_Bool   : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Align_Stack);
      Can_Throw_Bool        : constant LLVM.Types.Bool_T := Boolean'Pos (Can_Throw);
   begin
      Return_Value := Get_Inline_Asm (Ty, Asm_String, Asm_String_Size, Constraints, Constraints_Size, Has_Side_Effects_Bool, Is_Align_Stack_Bool, Dialect, Can_Throw_Bool);
      return Return_Value;
   end Get_Inline_Asm;

   function Get_Type_By_Name
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Type_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetTypeByName";
   function Get_Type_By_Name
     (M    : LLVM.Types.Module_T;
      Name : String)
      return LLVM.Types.Type_T
   is
      Return_Value : LLVM.Types.Type_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Get_Type_By_Name (M, Name_String);
      return Return_Value;
   end Get_Type_By_Name;

   function Get_Named_Metadata
     (M        : LLVM.Types.Module_T;
      Name     : Interfaces.C.Strings.chars_ptr;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Named_MD_Node_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetNamedMetadata";
   function Get_Named_Metadata
     (M        : LLVM.Types.Module_T;
      Name     : String;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Named_MD_Node_T
   is
      Return_Value : LLVM.Types.Named_MD_Node_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Get_Named_Metadata (M, Name_String, Name_Len);
      return Return_Value;
   end Get_Named_Metadata;

   function Get_Or_Insert_Named_Metadata
     (M        : LLVM.Types.Module_T;
      Name     : Interfaces.C.Strings.chars_ptr;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Named_MD_Node_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetOrInsertNamedMetadata";
   function Get_Or_Insert_Named_Metadata
     (M        : LLVM.Types.Module_T;
      Name     : String;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Named_MD_Node_T
   is
      Return_Value : LLVM.Types.Named_MD_Node_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Get_Or_Insert_Named_Metadata (M, Name_String, Name_Len);
      return Return_Value;
   end Get_Or_Insert_Named_Metadata;

   function Get_Named_Metadata_Name
     (Named_MD : LLVM.Types.Named_MD_Node_T;
      Name_Len : access stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetNamedMetadataName";
   function Get_Named_Metadata_Name
     (Named_MD : LLVM.Types.Named_MD_Node_T;
      Name_Len : access stddef_h.size_t)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Named_Metadata_Name (Named_MD, Name_Len);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Named_Metadata_Name;

   function Get_Named_Metadata_Num_Operands
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetNamedMetadataNumOperands";
   function Get_Named_Metadata_Num_Operands
     (M    : LLVM.Types.Module_T;
      Name : String)
      return unsigned
   is
      Return_Value : unsigned;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Get_Named_Metadata_Num_Operands (M, Name_String);
      return Return_Value;
   end Get_Named_Metadata_Num_Operands;

   procedure Get_Named_Metadata_Operands
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr;
      Dest : System.Address)
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetNamedMetadataOperands";
   procedure Get_Named_Metadata_Operands
     (M    : LLVM.Types.Module_T;
      Name : String;
      Dest : System.Address)
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Get_Named_Metadata_Operands (M, Name_String, Dest);
   end Get_Named_Metadata_Operands;

   procedure Add_Named_Metadata_Operand
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr;
      Val  : LLVM.Types.Value_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMAddNamedMetadataOperand";
   procedure Add_Named_Metadata_Operand
     (M    : LLVM.Types.Module_T;
      Name : String;
      Val  : LLVM.Types.Value_T)
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Add_Named_Metadata_Operand (M, Name_String, Val);
   end Add_Named_Metadata_Operand;

   function Get_Debug_Loc_Directory
     (Val    : LLVM.Types.Value_T;
      Length : access unsigned)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetDebugLocDirectory";
   function Get_Debug_Loc_Directory
     (Val    : LLVM.Types.Value_T;
      Length : access unsigned)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Debug_Loc_Directory (Val, Length);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Debug_Loc_Directory;

   function Get_Debug_Loc_Filename
     (Val    : LLVM.Types.Value_T;
      Length : access unsigned)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetDebugLocFilename";
   function Get_Debug_Loc_Filename
     (Val    : LLVM.Types.Value_T;
      Length : access unsigned)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Debug_Loc_Filename (Val, Length);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Debug_Loc_Filename;

   function Add_Function
     (M           : LLVM.Types.Module_T;
      Name        : Interfaces.C.Strings.chars_ptr;
      Function_Ty : LLVM.Types.Type_T)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMAddFunction";
   function Add_Function
     (M           : LLVM.Types.Module_T;
      Name        : String;
      Function_Ty : LLVM.Types.Type_T)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Add_Function (M, Name_String, Function_Ty);
      return Return_Value;
   end Add_Function;

   function Get_Named_Function
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetNamedFunction";
   function Get_Named_Function
     (M    : LLVM.Types.Module_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Get_Named_Function (M, Name_String);
      return Return_Value;
   end Get_Named_Function;

   procedure Set_Module_Inline_Asm
     (M   : LLVM.Types.Module_T;
      Asm : Interfaces.C.Strings.chars_ptr)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetModuleInlineAsm";
   procedure Set_Module_Inline_Asm
     (M   : LLVM.Types.Module_T;
      Asm : String)
   is
      Asm_Array  : aliased char_array := To_C (Asm);
      Asm_String : constant chars_ptr := To_Chars_Ptr (Asm_Array'Unchecked_Access);
   begin
      Set_Module_Inline_Asm (M, Asm_String);
   end Set_Module_Inline_Asm;

   function Type_Is_Sized
     (Ty : LLVM.Types.Type_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMTypeIsSized";
   function Type_Is_Sized
     (Ty : LLVM.Types.Type_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Type_Is_Sized (Ty);
      return Return_Value /= 0;
   end Type_Is_Sized;

   function Print_Type_To_String
     (Val : LLVM.Types.Type_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMPrintTypeToString";
   function Print_Type_To_String
     (Val : LLVM.Types.Type_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Print_Type_To_String (Val);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Print_Type_To_String;

   function Function_Type
     (Return_Type : LLVM.Types.Type_T;
      Param_Types : System.Address;
      Param_Count : unsigned;
      Is_Var_Arg  : LLVM.Types.Bool_T)
      return LLVM.Types.Type_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMFunctionType";
   function Function_Type
     (Return_Type : LLVM.Types.Type_T;
      Param_Types : System.Address;
      Param_Count : unsigned;
      Is_Var_Arg  : Boolean)
      return LLVM.Types.Type_T
   is
      Return_Value    : LLVM.Types.Type_T;
      Is_Var_Arg_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Var_Arg);
   begin
      Return_Value := Function_Type (Return_Type, Param_Types, Param_Count, Is_Var_Arg_Bool);
      return Return_Value;
   end Function_Type;

   function Is_Function_Var_Arg
     (Function_Ty : LLVM.Types.Type_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsFunctionVarArg";
   function Is_Function_Var_Arg
     (Function_Ty : LLVM.Types.Type_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Function_Var_Arg (Function_Ty);
      return Return_Value /= 0;
   end Is_Function_Var_Arg;

   function Struct_Type_In_Context
     (C             : LLVM.Types.Context_T;
      Element_Types : System.Address;
      Element_Count : unsigned;
      Packed        : LLVM.Types.Bool_T)
      return LLVM.Types.Type_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMStructTypeInContext";
   function Struct_Type_In_Context
     (C             : LLVM.Types.Context_T;
      Element_Types : System.Address;
      Element_Count : unsigned;
      Packed        : Boolean)
      return LLVM.Types.Type_T
   is
      Return_Value : LLVM.Types.Type_T;
      Packed_Bool  : constant LLVM.Types.Bool_T := Boolean'Pos (Packed);
   begin
      Return_Value := Struct_Type_In_Context (C, Element_Types, Element_Count, Packed_Bool);
      return Return_Value;
   end Struct_Type_In_Context;

   function Struct_Type
     (Element_Types : System.Address;
      Element_Count : unsigned;
      Packed        : LLVM.Types.Bool_T)
      return LLVM.Types.Type_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMStructType";
   function Struct_Type
     (Element_Types : System.Address;
      Element_Count : unsigned;
      Packed        : Boolean)
      return LLVM.Types.Type_T
   is
      Return_Value : LLVM.Types.Type_T;
      Packed_Bool  : constant LLVM.Types.Bool_T := Boolean'Pos (Packed);
   begin
      Return_Value := Struct_Type (Element_Types, Element_Count, Packed_Bool);
      return Return_Value;
   end Struct_Type;

   function Struct_Create_Named
     (C    : LLVM.Types.Context_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Type_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMStructCreateNamed";
   function Struct_Create_Named
     (C    : LLVM.Types.Context_T;
      Name : String)
      return LLVM.Types.Type_T
   is
      Return_Value : LLVM.Types.Type_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Struct_Create_Named (C, Name_String);
      return Return_Value;
   end Struct_Create_Named;

   function Get_Struct_Name
     (Ty : LLVM.Types.Type_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetStructName";
   function Get_Struct_Name
     (Ty : LLVM.Types.Type_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Struct_Name (Ty);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Struct_Name;

   procedure Struct_Set_Body
     (Struct_Ty     : LLVM.Types.Type_T;
      Element_Types : System.Address;
      Element_Count : unsigned;
      Packed        : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMStructSetBody";
   procedure Struct_Set_Body
     (Struct_Ty     : LLVM.Types.Type_T;
      Element_Types : System.Address;
      Element_Count : unsigned;
      Packed        : Boolean)
   is
      Packed_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Packed);
   begin
      Struct_Set_Body (Struct_Ty, Element_Types, Element_Count, Packed_Bool);
   end Struct_Set_Body;

   function Is_Packed_Struct
     (Struct_Ty : LLVM.Types.Type_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsPackedStruct";
   function Is_Packed_Struct
     (Struct_Ty : LLVM.Types.Type_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Packed_Struct (Struct_Ty);
      return Return_Value /= 0;
   end Is_Packed_Struct;

   function Is_Opaque_Struct
     (Struct_Ty : LLVM.Types.Type_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsOpaqueStruct";
   function Is_Opaque_Struct
     (Struct_Ty : LLVM.Types.Type_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Opaque_Struct (Struct_Ty);
      return Return_Value /= 0;
   end Is_Opaque_Struct;

   function Is_Literal_Struct
     (Struct_Ty : LLVM.Types.Type_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsLiteralStruct";
   function Is_Literal_Struct
     (Struct_Ty : LLVM.Types.Type_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Literal_Struct (Struct_Ty);
      return Return_Value /= 0;
   end Is_Literal_Struct;

   function Pointer_Type_Is_Opaque
     (Ty : LLVM.Types.Type_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMPointerTypeIsOpaque";
   function Pointer_Type_Is_Opaque
     (Ty : LLVM.Types.Type_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Pointer_Type_Is_Opaque (Ty);
      return Return_Value /= 0;
   end Pointer_Type_Is_Opaque;

   function Target_Ext_Type_In_Context
     (C                : LLVM.Types.Context_T;
      Name             : Interfaces.C.Strings.chars_ptr;
      Type_Params      : System.Address;
      Type_Param_Count : unsigned;
      Int_Params       : access unsigned;
      Int_Param_Count  : unsigned)
      return LLVM.Types.Type_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMTargetExtTypeInContext";
   function Target_Ext_Type_In_Context
     (C                : LLVM.Types.Context_T;
      Name             : String;
      Type_Params      : System.Address;
      Type_Param_Count : unsigned;
      Int_Params       : access unsigned;
      Int_Param_Count  : unsigned)
      return LLVM.Types.Type_T
   is
      Return_Value : LLVM.Types.Type_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Target_Ext_Type_In_Context (C, Name_String, Type_Params, Type_Param_Count, Int_Params, Int_Param_Count);
      return Return_Value;
   end Target_Ext_Type_In_Context;

   function Get_Value_Name_2
     (Val    : LLVM.Types.Value_T;
      Length : access stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetValueName2";
   function Get_Value_Name_2
     (Val    : LLVM.Types.Value_T;
      Length : access stddef_h.size_t)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Value_Name_2 (Val, Length);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Value_Name_2;

   procedure Set_Value_Name_2
     (Val      : LLVM.Types.Value_T;
      Name     : Interfaces.C.Strings.chars_ptr;
      Name_Len : stddef_h.size_t)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetValueName2";
   procedure Set_Value_Name_2
     (Val      : LLVM.Types.Value_T;
      Name     : String;
      Name_Len : stddef_h.size_t)
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Set_Value_Name_2 (Val, Name_String, Name_Len);
   end Set_Value_Name_2;

   function Print_Value_To_String
     (Val : LLVM.Types.Value_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMPrintValueToString";
   function Print_Value_To_String
     (Val : LLVM.Types.Value_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Print_Value_To_String (Val);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Print_Value_To_String;

   function Is_Constant
     (Val : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsConstant";
   function Is_Constant
     (Val : LLVM.Types.Value_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Constant (Val);
      return Return_Value /= 0;
   end Is_Constant;

   function Is_Undef
     (Val : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsUndef";
   function Is_Undef
     (Val : LLVM.Types.Value_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Undef (Val);
      return Return_Value /= 0;
   end Is_Undef;

   function Is_Poison
     (Val : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsPoison";
   function Is_Poison
     (Val : LLVM.Types.Value_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Poison (Val);
      return Return_Value /= 0;
   end Is_Poison;

   function Get_Value_Name
     (Val : LLVM.Types.Value_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetValueName";
   function Get_Value_Name
     (Val : LLVM.Types.Value_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Value_Name (Val);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Value_Name;

   procedure Set_Value_Name
     (Val  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetValueName";
   procedure Set_Value_Name
     (Val  : LLVM.Types.Value_T;
      Name : String)
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Set_Value_Name (Val, Name_String);
   end Set_Value_Name;

   function Is_Null
     (Val : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsNull";
   function Is_Null
     (Val : LLVM.Types.Value_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Null (Val);
      return Return_Value /= 0;
   end Is_Null;

   function Const_Int
     (Int_Ty      : LLVM.Types.Type_T;
      N           : Extensions.unsigned_long_long;
      Sign_Extend : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMConstInt";
   function Const_Int
     (Int_Ty      : LLVM.Types.Type_T;
      N           : Extensions.unsigned_long_long;
      Sign_Extend : Boolean)
      return LLVM.Types.Value_T
   is
      Return_Value     : LLVM.Types.Value_T;
      Sign_Extend_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Sign_Extend);
   begin
      Return_Value := Const_Int (Int_Ty, N, Sign_Extend_Bool);
      return Return_Value;
   end Const_Int;

   function Const_Int_Of_String
     (Int_Ty : LLVM.Types.Type_T;
      Text   : Interfaces.C.Strings.chars_ptr;
      Radix  : stdint_h.uint8_t)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMConstIntOfString";
   function Const_Int_Of_String
     (Int_Ty : LLVM.Types.Type_T;
      Text   : String;
      Radix  : stdint_h.uint8_t)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Text_Array   : aliased char_array := To_C (Text);
      Text_String  : constant chars_ptr := To_Chars_Ptr (Text_Array'Unchecked_Access);
   begin
      Return_Value := Const_Int_Of_String (Int_Ty, Text_String, Radix);
      return Return_Value;
   end Const_Int_Of_String;

   function Const_Int_Of_String_And_Size
     (Int_Ty : LLVM.Types.Type_T;
      Text   : Interfaces.C.Strings.chars_ptr;
      S_Len  : unsigned;
      Radix  : stdint_h.uint8_t)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMConstIntOfStringAndSize";
   function Const_Int_Of_String_And_Size
     (Int_Ty : LLVM.Types.Type_T;
      Text   : String;
      S_Len  : unsigned;
      Radix  : stdint_h.uint8_t)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Text_Array   : aliased char_array := To_C (Text);
      Text_String  : constant chars_ptr := To_Chars_Ptr (Text_Array'Unchecked_Access);
   begin
      Return_Value := Const_Int_Of_String_And_Size (Int_Ty, Text_String, S_Len, Radix);
      return Return_Value;
   end Const_Int_Of_String_And_Size;

   function Const_Real_Of_String
     (Real_Ty : LLVM.Types.Type_T;
      Text    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMConstRealOfString";
   function Const_Real_Of_String
     (Real_Ty : LLVM.Types.Type_T;
      Text    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Text_Array   : aliased char_array := To_C (Text);
      Text_String  : constant chars_ptr := To_Chars_Ptr (Text_Array'Unchecked_Access);
   begin
      Return_Value := Const_Real_Of_String (Real_Ty, Text_String);
      return Return_Value;
   end Const_Real_Of_String;

   function Const_Real_Of_String_And_Size
     (Real_Ty : LLVM.Types.Type_T;
      Text    : Interfaces.C.Strings.chars_ptr;
      S_Len   : unsigned)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMConstRealOfStringAndSize";
   function Const_Real_Of_String_And_Size
     (Real_Ty : LLVM.Types.Type_T;
      Text    : String;
      S_Len   : unsigned)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Text_Array   : aliased char_array := To_C (Text);
      Text_String  : constant chars_ptr := To_Chars_Ptr (Text_Array'Unchecked_Access);
   begin
      Return_Value := Const_Real_Of_String_And_Size (Real_Ty, Text_String, S_Len);
      return Return_Value;
   end Const_Real_Of_String_And_Size;

   function Const_String_In_Context
     (C                   : LLVM.Types.Context_T;
      Str                 : Interfaces.C.Strings.chars_ptr;
      Length              : unsigned;
      Dont_Null_Terminate : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMConstStringInContext";
   function Const_String_In_Context
     (C                   : LLVM.Types.Context_T;
      Str                 : String;
      Length              : unsigned;
      Dont_Null_Terminate : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Str_Array    : aliased char_array := To_C (Str);
      Str_String   : constant chars_ptr := To_Chars_Ptr (Str_Array'Unchecked_Access);
   begin
      Return_Value := Const_String_In_Context (C, Str_String, Length, Dont_Null_Terminate);
      return Return_Value;
   end Const_String_In_Context;

   function Const_String_In_Context
     (C                   : LLVM.Types.Context_T;
      Str                 : String;
      Length              : unsigned;
      Dont_Null_Terminate : Boolean)
      return LLVM.Types.Value_T
   is
      Return_Value             : LLVM.Types.Value_T;
      Dont_Null_Terminate_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Dont_Null_Terminate);
   begin
      Return_Value := Const_String_In_Context (C, Str, Length, Dont_Null_Terminate_Bool);
      return Return_Value;
   end Const_String_In_Context;

   function Const_String
     (Str                 : Interfaces.C.Strings.chars_ptr;
      Length              : unsigned;
      Dont_Null_Terminate : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMConstString";
   function Const_String
     (Str                 : String;
      Length              : unsigned;
      Dont_Null_Terminate : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Str_Array    : aliased char_array := To_C (Str);
      Str_String   : constant chars_ptr := To_Chars_Ptr (Str_Array'Unchecked_Access);
   begin
      Return_Value := Const_String (Str_String, Length, Dont_Null_Terminate);
      return Return_Value;
   end Const_String;

   function Const_String
     (Str                 : String;
      Length              : unsigned;
      Dont_Null_Terminate : Boolean)
      return LLVM.Types.Value_T
   is
      Return_Value             : LLVM.Types.Value_T;
      Dont_Null_Terminate_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Dont_Null_Terminate);
   begin
      Return_Value := Const_String (Str, Length, Dont_Null_Terminate_Bool);
      return Return_Value;
   end Const_String;

   function Is_Constant_String
     (C : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsConstantString";
   function Is_Constant_String
     (C : LLVM.Types.Value_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Constant_String (C);
      return Return_Value /= 0;
   end Is_Constant_String;

   function Get_As_String
     (C      : LLVM.Types.Value_T;
      Length : access stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetAsString";
   function Get_As_String
     (C      : LLVM.Types.Value_T;
      Length : access stddef_h.size_t)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_As_String (C, Length);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_As_String;

   function Const_Struct_In_Context
     (C             : LLVM.Types.Context_T;
      Constant_Vals : System.Address;
      Count         : unsigned;
      Packed        : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMConstStructInContext";
   function Const_Struct_In_Context
     (C             : LLVM.Types.Context_T;
      Constant_Vals : System.Address;
      Count         : unsigned;
      Packed        : Boolean)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Packed_Bool  : constant LLVM.Types.Bool_T := Boolean'Pos (Packed);
   begin
      Return_Value := Const_Struct_In_Context (C, Constant_Vals, Count, Packed_Bool);
      return Return_Value;
   end Const_Struct_In_Context;

   function Const_Struct
     (Constant_Vals : System.Address;
      Count         : unsigned;
      Packed        : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMConstStruct";
   function Const_Struct
     (Constant_Vals : System.Address;
      Count         : unsigned;
      Packed        : Boolean)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Packed_Bool  : constant LLVM.Types.Bool_T := Boolean'Pos (Packed);
   begin
      Return_Value := Const_Struct (Constant_Vals, Count, Packed_Bool);
      return Return_Value;
   end Const_Struct;

   function Const_Int_Cast
     (Constant_Val : LLVM.Types.Value_T;
      To_Type      : LLVM.Types.Type_T;
      Is_Signed    : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMConstIntCast";
   function Const_Int_Cast
     (Constant_Val : LLVM.Types.Value_T;
      To_Type      : LLVM.Types.Type_T;
      Is_Signed    : Boolean)
      return LLVM.Types.Value_T
   is
      Return_Value   : LLVM.Types.Value_T;
      Is_Signed_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Signed);
   begin
      Return_Value := Const_Int_Cast (Constant_Val, To_Type, Is_Signed_Bool);
      return Return_Value;
   end Const_Int_Cast;

   function Const_Inline_Asm
     (Ty               : LLVM.Types.Type_T;
      Asm_String       : Interfaces.C.Strings.chars_ptr;
      Constraints      : Interfaces.C.Strings.chars_ptr;
      Has_Side_Effects : LLVM.Types.Bool_T;
      Is_Align_Stack   : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMConstInlineAsm";
   function Const_Inline_Asm
     (Ty               : LLVM.Types.Type_T;
      Asm_String       : String;
      Constraints      : String;
      Has_Side_Effects : LLVM.Types.Bool_T;
      Is_Align_Stack   : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T
   is
      Return_Value       : LLVM.Types.Value_T;
      Asm_String_Array   : aliased char_array := To_C (Asm_String);
      Asm_String_String  : constant chars_ptr := To_Chars_Ptr (Asm_String_Array'Unchecked_Access);
      Constraints_Array  : aliased char_array := To_C (Constraints);
      Constraints_String : constant chars_ptr := To_Chars_Ptr (Constraints_Array'Unchecked_Access);
   begin
      Return_Value := Const_Inline_Asm (Ty, Asm_String_String, Constraints_String, Has_Side_Effects, Is_Align_Stack);
      return Return_Value;
   end Const_Inline_Asm;

   function Const_Inline_Asm
     (Ty               : LLVM.Types.Type_T;
      Asm_String       : String;
      Constraints      : String;
      Has_Side_Effects : Boolean;
      Is_Align_Stack   : Boolean)
      return LLVM.Types.Value_T
   is
      Return_Value          : LLVM.Types.Value_T;
      Has_Side_Effects_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Has_Side_Effects);
      Is_Align_Stack_Bool   : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Align_Stack);
   begin
      Return_Value := Const_Inline_Asm (Ty, Asm_String, Constraints, Has_Side_Effects_Bool, Is_Align_Stack_Bool);
      return Return_Value;
   end Const_Inline_Asm;

   function Is_Declaration
     (Global : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsDeclaration";
   function Is_Declaration
     (Global : LLVM.Types.Value_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Declaration (Global);
      return Return_Value /= 0;
   end Is_Declaration;

   function Get_Section
     (Global : LLVM.Types.Value_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetSection";
   function Get_Section
     (Global : LLVM.Types.Value_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Section (Global);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Section;

   procedure Set_Section
     (Global  : LLVM.Types.Value_T;
      Section : Interfaces.C.Strings.chars_ptr)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetSection";
   procedure Set_Section
     (Global  : LLVM.Types.Value_T;
      Section : String)
   is
      Section_Array  : aliased char_array := To_C (Section);
      Section_String : constant chars_ptr := To_Chars_Ptr (Section_Array'Unchecked_Access);
   begin
      Set_Section (Global, Section_String);
   end Set_Section;

   function Has_Unnamed_Addr
     (Global : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMHasUnnamedAddr";
   function Has_Unnamed_Addr
     (Global : LLVM.Types.Value_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Has_Unnamed_Addr (Global);
      return Return_Value /= 0;
   end Has_Unnamed_Addr;

   procedure Set_Unnamed_Addr
     (Global           : LLVM.Types.Value_T;
      Has_Unnamed_Addr : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetUnnamedAddr";
   procedure Set_Unnamed_Addr
     (Global           : LLVM.Types.Value_T;
      Has_Unnamed_Addr : Boolean)
   is
      Has_Unnamed_Addr_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Has_Unnamed_Addr);
   begin
      Set_Unnamed_Addr (Global, Has_Unnamed_Addr_Bool);
   end Set_Unnamed_Addr;

   function Add_Global
     (M    : LLVM.Types.Module_T;
      Ty   : LLVM.Types.Type_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMAddGlobal";
   function Add_Global
     (M    : LLVM.Types.Module_T;
      Ty   : LLVM.Types.Type_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Add_Global (M, Ty, Name_String);
      return Return_Value;
   end Add_Global;

   function Add_Global_In_Address_Space
     (M             : LLVM.Types.Module_T;
      Ty            : LLVM.Types.Type_T;
      Name          : Interfaces.C.Strings.chars_ptr;
      Address_Space : unsigned)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMAddGlobalInAddressSpace";
   function Add_Global_In_Address_Space
     (M             : LLVM.Types.Module_T;
      Ty            : LLVM.Types.Type_T;
      Name          : String;
      Address_Space : unsigned)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Add_Global_In_Address_Space (M, Ty, Name_String, Address_Space);
      return Return_Value;
   end Add_Global_In_Address_Space;

   function Get_Named_Global
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetNamedGlobal";
   function Get_Named_Global
     (M    : LLVM.Types.Module_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Get_Named_Global (M, Name_String);
      return Return_Value;
   end Get_Named_Global;

   function Is_Thread_Local
     (Global_Var : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsThreadLocal";
   function Is_Thread_Local
     (Global_Var : LLVM.Types.Value_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Thread_Local (Global_Var);
      return Return_Value /= 0;
   end Is_Thread_Local;

   procedure Set_Thread_Local
     (Global_Var      : LLVM.Types.Value_T;
      Is_Thread_Local : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetThreadLocal";
   procedure Set_Thread_Local
     (Global_Var      : LLVM.Types.Value_T;
      Is_Thread_Local : Boolean)
   is
      Is_Thread_Local_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Thread_Local);
   begin
      Set_Thread_Local (Global_Var, Is_Thread_Local_Bool);
   end Set_Thread_Local;

   function Is_Global_Constant
     (Global_Var : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsGlobalConstant";
   function Is_Global_Constant
     (Global_Var : LLVM.Types.Value_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Global_Constant (Global_Var);
      return Return_Value /= 0;
   end Is_Global_Constant;

   procedure Set_Global_Constant
     (Global_Var  : LLVM.Types.Value_T;
      Is_Constant : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetGlobalConstant";
   procedure Set_Global_Constant
     (Global_Var  : LLVM.Types.Value_T;
      Is_Constant : Boolean)
   is
      Is_Constant_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Constant);
   begin
      Set_Global_Constant (Global_Var, Is_Constant_Bool);
   end Set_Global_Constant;

   function Is_Externally_Initialized
     (Global_Var : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsExternallyInitialized";
   function Is_Externally_Initialized
     (Global_Var : LLVM.Types.Value_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Externally_Initialized (Global_Var);
      return Return_Value /= 0;
   end Is_Externally_Initialized;

   procedure Set_Externally_Initialized
     (Global_Var  : LLVM.Types.Value_T;
      Is_Ext_Init : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetExternallyInitialized";
   procedure Set_Externally_Initialized
     (Global_Var  : LLVM.Types.Value_T;
      Is_Ext_Init : Boolean)
   is
      Is_Ext_Init_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Ext_Init);
   begin
      Set_Externally_Initialized (Global_Var, Is_Ext_Init_Bool);
   end Set_Externally_Initialized;

   function Add_Alias_2
     (M          : LLVM.Types.Module_T;
      Value_Ty   : LLVM.Types.Type_T;
      Addr_Space : unsigned;
      Aliasee    : LLVM.Types.Value_T;
      Name       : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMAddAlias2";
   function Add_Alias_2
     (M          : LLVM.Types.Module_T;
      Value_Ty   : LLVM.Types.Type_T;
      Addr_Space : unsigned;
      Aliasee    : LLVM.Types.Value_T;
      Name       : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Add_Alias_2 (M, Value_Ty, Addr_Space, Aliasee, Name_String);
      return Return_Value;
   end Add_Alias_2;

   function Get_Named_Global_Alias
     (M        : LLVM.Types.Module_T;
      Name     : Interfaces.C.Strings.chars_ptr;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetNamedGlobalAlias";
   function Get_Named_Global_Alias
     (M        : LLVM.Types.Module_T;
      Name     : String;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Get_Named_Global_Alias (M, Name_String, Name_Len);
      return Return_Value;
   end Get_Named_Global_Alias;

   function Has_Personality_Fn
     (Fn : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMHasPersonalityFn";
   function Has_Personality_Fn
     (Fn : LLVM.Types.Value_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Has_Personality_Fn (Fn);
      return Return_Value /= 0;
   end Has_Personality_Fn;

   function Lookup_Intrinsic_ID
     (Name     : Interfaces.C.Strings.chars_ptr;
      Name_Len : stddef_h.size_t)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "LLVMLookupIntrinsicID";
   function Lookup_Intrinsic_ID
     (Name     : String;
      Name_Len : stddef_h.size_t)
      return unsigned
   is
      Return_Value : unsigned;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Lookup_Intrinsic_ID (Name_String, Name_Len);
      return Return_Value;
   end Lookup_Intrinsic_ID;

   function Intrinsic_Get_Name
     (ID          : unsigned;
      Name_Length : access stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMIntrinsicGetName";
   function Intrinsic_Get_Name
     (ID          : unsigned;
      Name_Length : access stddef_h.size_t)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Intrinsic_Get_Name (ID, Name_Length);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Intrinsic_Get_Name;

   function Intrinsic_Copy_Overloaded_Name
     (ID          : unsigned;
      Param_Types : System.Address;
      Param_Count : stddef_h.size_t;
      Name_Length : access stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMIntrinsicCopyOverloadedName";
   function Intrinsic_Copy_Overloaded_Name
     (ID          : unsigned;
      Param_Types : System.Address;
      Param_Count : stddef_h.size_t;
      Name_Length : access stddef_h.size_t)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Intrinsic_Copy_Overloaded_Name (ID, Param_Types, Param_Count, Name_Length);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Intrinsic_Copy_Overloaded_Name;

   function Intrinsic_Copy_Overloaded_Name_2
     (C_Mod       : LLVM.Types.Module_T;
      ID          : unsigned;
      Param_Types : System.Address;
      Param_Count : stddef_h.size_t;
      Name_Length : access stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMIntrinsicCopyOverloadedName2";
   function Intrinsic_Copy_Overloaded_Name_2
     (C_Mod       : LLVM.Types.Module_T;
      ID          : unsigned;
      Param_Types : System.Address;
      Param_Count : stddef_h.size_t;
      Name_Length : access stddef_h.size_t)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Intrinsic_Copy_Overloaded_Name_2 (C_Mod, ID, Param_Types, Param_Count, Name_Length);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Intrinsic_Copy_Overloaded_Name_2;

   function Intrinsic_Is_Overloaded
     (ID : unsigned)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIntrinsicIsOverloaded";
   function Intrinsic_Is_Overloaded
     (ID : unsigned)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Intrinsic_Is_Overloaded (ID);
      return Return_Value /= 0;
   end Intrinsic_Is_Overloaded;

   function Get_GC
     (Fn : LLVM.Types.Value_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetGC";
   function Get_GC
     (Fn : LLVM.Types.Value_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_GC (Fn);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_GC;

   procedure Set_GC
     (Fn   : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetGC";
   procedure Set_GC
     (Fn   : LLVM.Types.Value_T;
      Name : String)
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Set_GC (Fn, Name_String);
   end Set_GC;

   function Get_String_Attribute_At_Index
     (F     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : Interfaces.C.Strings.chars_ptr;
      K_Len : unsigned)
      return LLVM.Types.Attribute_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetStringAttributeAtIndex";
   function Get_String_Attribute_At_Index
     (F     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : String;
      K_Len : unsigned)
      return LLVM.Types.Attribute_T
   is
      Return_Value : LLVM.Types.Attribute_T;
      K_Array      : aliased char_array := To_C (K);
      K_String     : constant chars_ptr := To_Chars_Ptr (K_Array'Unchecked_Access);
   begin
      Return_Value := Get_String_Attribute_At_Index (F, Idx, K_String, K_Len);
      return Return_Value;
   end Get_String_Attribute_At_Index;

   procedure Remove_String_Attribute_At_Index
     (F     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : Interfaces.C.Strings.chars_ptr;
      K_Len : unsigned)
   with Import => True,
        Convention => C,
        External_Name => "LLVMRemoveStringAttributeAtIndex";
   procedure Remove_String_Attribute_At_Index
     (F     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : String;
      K_Len : unsigned)
   is
      K_Array  : aliased char_array := To_C (K);
      K_String : constant chars_ptr := To_Chars_Ptr (K_Array'Unchecked_Access);
   begin
      Remove_String_Attribute_At_Index (F, Idx, K_String, K_Len);
   end Remove_String_Attribute_At_Index;

   procedure Add_Target_Dependent_Function_Attr
     (Fn : LLVM.Types.Value_T;
      A  : Interfaces.C.Strings.chars_ptr;
      V  : Interfaces.C.Strings.chars_ptr)
   with Import => True,
        Convention => C,
        External_Name => "LLVMAddTargetDependentFunctionAttr";
   procedure Add_Target_Dependent_Function_Attr
     (Fn : LLVM.Types.Value_T;
      A  : String;
      V  : String)
   is
      A_Array  : aliased char_array := To_C (A);
      A_String : constant chars_ptr := To_Chars_Ptr (A_Array'Unchecked_Access);
      V_Array  : aliased char_array := To_C (V);
      V_String : constant chars_ptr := To_Chars_Ptr (V_Array'Unchecked_Access);
   begin
      Add_Target_Dependent_Function_Attr (Fn, A_String, V_String);
   end Add_Target_Dependent_Function_Attr;

   function Add_Global_I_Func
     (M          : LLVM.Types.Module_T;
      Name       : Interfaces.C.Strings.chars_ptr;
      Name_Len   : stddef_h.size_t;
      Ty         : LLVM.Types.Type_T;
      Addr_Space : unsigned;
      Resolver   : LLVM.Types.Value_T)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMAddGlobalIFunc";
   function Add_Global_I_Func
     (M          : LLVM.Types.Module_T;
      Name       : String;
      Name_Len   : stddef_h.size_t;
      Ty         : LLVM.Types.Type_T;
      Addr_Space : unsigned;
      Resolver   : LLVM.Types.Value_T)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Add_Global_I_Func (M, Name_String, Name_Len, Ty, Addr_Space, Resolver);
      return Return_Value;
   end Add_Global_I_Func;

   function Get_Named_Global_I_Func
     (M        : LLVM.Types.Module_T;
      Name     : Interfaces.C.Strings.chars_ptr;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetNamedGlobalIFunc";
   function Get_Named_Global_I_Func
     (M        : LLVM.Types.Module_T;
      Name     : String;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Get_Named_Global_I_Func (M, Name_String, Name_Len);
      return Return_Value;
   end Get_Named_Global_I_Func;

   function MD_String_In_Context_2
     (C     : LLVM.Types.Context_T;
      Str   : Interfaces.C.Strings.chars_ptr;
      S_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMMDStringInContext2";
   function MD_String_In_Context_2
     (C     : LLVM.Types.Context_T;
      Str   : String;
      S_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T
   is
      Return_Value : LLVM.Types.Metadata_T;
      Str_Array    : aliased char_array := To_C (Str);
      Str_String   : constant chars_ptr := To_Chars_Ptr (Str_Array'Unchecked_Access);
   begin
      Return_Value := MD_String_In_Context_2 (C, Str_String, S_Len);
      return Return_Value;
   end MD_String_In_Context_2;

   function Get_MD_String
     (V      : LLVM.Types.Value_T;
      Length : access unsigned)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetMDString";
   function Get_MD_String
     (V      : LLVM.Types.Value_T;
      Length : access unsigned)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_MD_String (V, Length);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_MD_String;

   function MD_String_In_Context
     (C     : LLVM.Types.Context_T;
      Str   : Interfaces.C.Strings.chars_ptr;
      S_Len : unsigned)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMMDStringInContext";
   function MD_String_In_Context
     (C     : LLVM.Types.Context_T;
      Str   : String;
      S_Len : unsigned)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Str_Array    : aliased char_array := To_C (Str);
      Str_String   : constant chars_ptr := To_Chars_Ptr (Str_Array'Unchecked_Access);
   begin
      Return_Value := MD_String_In_Context (C, Str_String, S_Len);
      return Return_Value;
   end MD_String_In_Context;

   function MD_String
     (Str   : Interfaces.C.Strings.chars_ptr;
      S_Len : unsigned)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMMDString";
   function MD_String
     (Str   : String;
      S_Len : unsigned)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Str_Array    : aliased char_array := To_C (Str);
      Str_String   : constant chars_ptr := To_Chars_Ptr (Str_Array'Unchecked_Access);
   begin
      Return_Value := MD_String (Str_String, S_Len);
      return Return_Value;
   end MD_String;

   function Value_Is_Basic_Block
     (Val : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMValueIsBasicBlock";
   function Value_Is_Basic_Block
     (Val : LLVM.Types.Value_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Value_Is_Basic_Block (Val);
      return Return_Value /= 0;
   end Value_Is_Basic_Block;

   function Get_Basic_Block_Name
     (BB : LLVM.Types.Basic_Block_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetBasicBlockName";
   function Get_Basic_Block_Name
     (BB : LLVM.Types.Basic_Block_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Basic_Block_Name (BB);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Basic_Block_Name;

   function Create_Basic_Block_In_Context
     (C    : LLVM.Types.Context_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Basic_Block_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMCreateBasicBlockInContext";
   function Create_Basic_Block_In_Context
     (C    : LLVM.Types.Context_T;
      Name : String)
      return LLVM.Types.Basic_Block_T
   is
      Return_Value : LLVM.Types.Basic_Block_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Create_Basic_Block_In_Context (C, Name_String);
      return Return_Value;
   end Create_Basic_Block_In_Context;

   function Append_Basic_Block_In_Context
     (C    : LLVM.Types.Context_T;
      Fn   : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Basic_Block_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMAppendBasicBlockInContext";
   function Append_Basic_Block_In_Context
     (C    : LLVM.Types.Context_T;
      Fn   : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Basic_Block_T
   is
      Return_Value : LLVM.Types.Basic_Block_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Append_Basic_Block_In_Context (C, Fn, Name_String);
      return Return_Value;
   end Append_Basic_Block_In_Context;

   function Append_Basic_Block
     (Fn   : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Basic_Block_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMAppendBasicBlock";
   function Append_Basic_Block
     (Fn   : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Basic_Block_T
   is
      Return_Value : LLVM.Types.Basic_Block_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Append_Basic_Block (Fn, Name_String);
      return Return_Value;
   end Append_Basic_Block;

   function Insert_Basic_Block_In_Context
     (C    : LLVM.Types.Context_T;
      BB   : LLVM.Types.Basic_Block_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Basic_Block_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMInsertBasicBlockInContext";
   function Insert_Basic_Block_In_Context
     (C    : LLVM.Types.Context_T;
      BB   : LLVM.Types.Basic_Block_T;
      Name : String)
      return LLVM.Types.Basic_Block_T
   is
      Return_Value : LLVM.Types.Basic_Block_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Insert_Basic_Block_In_Context (C, BB, Name_String);
      return Return_Value;
   end Insert_Basic_Block_In_Context;

   function Insert_Basic_Block
     (Insert_Before_BB : LLVM.Types.Basic_Block_T;
      Name             : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Basic_Block_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMInsertBasicBlock";
   function Insert_Basic_Block
     (Insert_Before_BB : LLVM.Types.Basic_Block_T;
      Name             : String)
      return LLVM.Types.Basic_Block_T
   is
      Return_Value : LLVM.Types.Basic_Block_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Insert_Basic_Block (Insert_Before_BB, Name_String);
      return Return_Value;
   end Insert_Basic_Block;

   function Get_Call_Site_String_Attribute
     (C     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : Interfaces.C.Strings.chars_ptr;
      K_Len : unsigned)
      return LLVM.Types.Attribute_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetCallSiteStringAttribute";
   function Get_Call_Site_String_Attribute
     (C     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : String;
      K_Len : unsigned)
      return LLVM.Types.Attribute_T
   is
      Return_Value : LLVM.Types.Attribute_T;
      K_Array      : aliased char_array := To_C (K);
      K_String     : constant chars_ptr := To_Chars_Ptr (K_Array'Unchecked_Access);
   begin
      Return_Value := Get_Call_Site_String_Attribute (C, Idx, K_String, K_Len);
      return Return_Value;
   end Get_Call_Site_String_Attribute;

   procedure Remove_Call_Site_String_Attribute
     (C     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : Interfaces.C.Strings.chars_ptr;
      K_Len : unsigned)
   with Import => True,
        Convention => C,
        External_Name => "LLVMRemoveCallSiteStringAttribute";
   procedure Remove_Call_Site_String_Attribute
     (C     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : String;
      K_Len : unsigned)
   is
      K_Array  : aliased char_array := To_C (K);
      K_String : constant chars_ptr := To_Chars_Ptr (K_Array'Unchecked_Access);
   begin
      Remove_Call_Site_String_Attribute (C, Idx, K_String, K_Len);
   end Remove_Call_Site_String_Attribute;

   function Is_Tail_Call
     (Call_Inst : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsTailCall";
   function Is_Tail_Call
     (Call_Inst : LLVM.Types.Value_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Tail_Call (Call_Inst);
      return Return_Value /= 0;
   end Is_Tail_Call;

   procedure Set_Tail_Call
     (Call_Inst    : LLVM.Types.Value_T;
      Is_Tail_Call : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetTailCall";
   procedure Set_Tail_Call
     (Call_Inst    : LLVM.Types.Value_T;
      Is_Tail_Call : Boolean)
   is
      Is_Tail_Call_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Tail_Call);
   begin
      Set_Tail_Call (Call_Inst, Is_Tail_Call_Bool);
   end Set_Tail_Call;

   function Is_Conditional
     (Branch : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsConditional";
   function Is_Conditional
     (Branch : LLVM.Types.Value_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Conditional (Branch);
      return Return_Value /= 0;
   end Is_Conditional;

   function Is_In_Bounds
     (GEP : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsInBounds";
   function Is_In_Bounds
     (GEP : LLVM.Types.Value_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_In_Bounds (GEP);
      return Return_Value /= 0;
   end Is_In_Bounds;

   procedure Set_Is_In_Bounds
     (GEP       : LLVM.Types.Value_T;
      In_Bounds : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetIsInBounds";
   procedure Set_Is_In_Bounds
     (GEP       : LLVM.Types.Value_T;
      In_Bounds : Boolean)
   is
      In_Bounds_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (In_Bounds);
   begin
      Set_Is_In_Bounds (GEP, In_Bounds_Bool);
   end Set_Is_In_Bounds;

   procedure Insert_Into_Builder_With_Name
     (Builder : LLVM.Types.Builder_T;
      Instr   : LLVM.Types.Value_T;
      Name    : Interfaces.C.Strings.chars_ptr)
   with Import => True,
        Convention => C,
        External_Name => "LLVMInsertIntoBuilderWithName";
   procedure Insert_Into_With_Name
     (Builder : LLVM.Types.Builder_T;
      Instr   : LLVM.Types.Value_T;
      Name    : String)
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Insert_Into_Builder_With_Name (Builder, Instr, Name_String);
   end Insert_Into_With_Name;

   function Build_Invoke_2
     (Arg_1    : LLVM.Types.Builder_T;
      Ty       : LLVM.Types.Type_T;
      Fn       : LLVM.Types.Value_T;
      Args     : System.Address;
      Num_Args : unsigned;
      C_Then   : LLVM.Types.Basic_Block_T;
      Catch    : LLVM.Types.Basic_Block_T;
      Name     : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildInvoke2";
   function Invoke_2
     (Arg_1    : LLVM.Types.Builder_T;
      Ty       : LLVM.Types.Type_T;
      Fn       : LLVM.Types.Value_T;
      Args     : System.Address;
      Num_Args : unsigned;
      C_Then   : LLVM.Types.Basic_Block_T;
      Catch    : LLVM.Types.Basic_Block_T;
      Name     : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Invoke_2 (Arg_1, Ty, Fn, Args, Num_Args, C_Then, Catch, Name_String);
      return Return_Value;
   end Invoke_2;

   function Build_Landing_Pad
     (B           : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pers_Fn     : LLVM.Types.Value_T;
      Num_Clauses : unsigned;
      Name        : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildLandingPad";
   function Landing_Pad
     (B           : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pers_Fn     : LLVM.Types.Value_T;
      Num_Clauses : unsigned;
      Name        : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Landing_Pad (B, Ty, Pers_Fn, Num_Clauses, Name_String);
      return Return_Value;
   end Landing_Pad;

   function Build_Catch_Pad
     (B          : LLVM.Types.Builder_T;
      Parent_Pad : LLVM.Types.Value_T;
      Args       : System.Address;
      Num_Args   : unsigned;
      Name       : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildCatchPad";
   function Catch_Pad
     (B          : LLVM.Types.Builder_T;
      Parent_Pad : LLVM.Types.Value_T;
      Args       : System.Address;
      Num_Args   : unsigned;
      Name       : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Catch_Pad (B, Parent_Pad, Args, Num_Args, Name_String);
      return Return_Value;
   end Catch_Pad;

   function Build_Cleanup_Pad
     (B          : LLVM.Types.Builder_T;
      Parent_Pad : LLVM.Types.Value_T;
      Args       : System.Address;
      Num_Args   : unsigned;
      Name       : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildCleanupPad";
   function Cleanup_Pad
     (B          : LLVM.Types.Builder_T;
      Parent_Pad : LLVM.Types.Value_T;
      Args       : System.Address;
      Num_Args   : unsigned;
      Name       : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Cleanup_Pad (B, Parent_Pad, Args, Num_Args, Name_String);
      return Return_Value;
   end Cleanup_Pad;

   function Build_Catch_Switch
     (B            : LLVM.Types.Builder_T;
      Parent_Pad   : LLVM.Types.Value_T;
      Unwind_BB    : LLVM.Types.Basic_Block_T;
      Num_Handlers : unsigned;
      Name         : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildCatchSwitch";
   function Catch_Switch
     (B            : LLVM.Types.Builder_T;
      Parent_Pad   : LLVM.Types.Value_T;
      Unwind_BB    : LLVM.Types.Basic_Block_T;
      Num_Handlers : unsigned;
      Name         : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Catch_Switch (B, Parent_Pad, Unwind_BB, Num_Handlers, Name_String);
      return Return_Value;
   end Catch_Switch;

   function Is_Cleanup
     (Landing_Pad : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsCleanup";
   function Is_Cleanup
     (Landing_Pad : LLVM.Types.Value_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Cleanup (Landing_Pad);
      return Return_Value /= 0;
   end Is_Cleanup;

   procedure Set_Cleanup
     (Landing_Pad : LLVM.Types.Value_T;
      Val         : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetCleanup";
   procedure Set_Cleanup
     (Landing_Pad : LLVM.Types.Value_T;
      Val         : Boolean)
   is
      Val_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Val);
   begin
      Set_Cleanup (Landing_Pad, Val_Bool);
   end Set_Cleanup;

   function Build_Add
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildAdd";
   function Add
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Add (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end Add;

   function Build_NSW_Add
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildNSWAdd";
   function NSW_Add
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_NSW_Add (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end NSW_Add;

   function Build_NUW_Add
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildNUWAdd";
   function NUW_Add
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_NUW_Add (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end NUW_Add;

   function Build_F_Add
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildFAdd";
   function F_Add
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_F_Add (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end F_Add;

   function Build_Sub
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildSub";
   function Sub
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Sub (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end Sub;

   function Build_NSW_Sub
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildNSWSub";
   function NSW_Sub
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_NSW_Sub (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end NSW_Sub;

   function Build_NUW_Sub
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildNUWSub";
   function NUW_Sub
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_NUW_Sub (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end NUW_Sub;

   function Build_F_Sub
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildFSub";
   function F_Sub
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_F_Sub (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end F_Sub;

   function Build_Mul
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildMul";
   function Mul
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Mul (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end Mul;

   function Build_NSW_Mul
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildNSWMul";
   function NSW_Mul
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_NSW_Mul (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end NSW_Mul;

   function Build_NUW_Mul
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildNUWMul";
   function NUW_Mul
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_NUW_Mul (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end NUW_Mul;

   function Build_F_Mul
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildFMul";
   function F_Mul
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_F_Mul (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end F_Mul;

   function Build_U_Div
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildUDiv";
   function U_Div
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_U_Div (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end U_Div;

   function Build_Exact_U_Div
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildExactUDiv";
   function Exact_U_Div
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Exact_U_Div (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end Exact_U_Div;

   function Build_S_Div
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildSDiv";
   function S_Div
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_S_Div (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end S_Div;

   function Build_Exact_S_Div
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildExactSDiv";
   function Exact_S_Div
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Exact_S_Div (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end Exact_S_Div;

   function Build_F_Div
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildFDiv";
   function F_Div
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_F_Div (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end F_Div;

   function Build_U_Rem
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildURem";
   function U_Rem
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_U_Rem (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end U_Rem;

   function Build_S_Rem
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildSRem";
   function S_Rem
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_S_Rem (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end S_Rem;

   function Build_F_Rem
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildFRem";
   function F_Rem
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_F_Rem (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end F_Rem;

   function Build_Shl
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildShl";
   function Shl
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Shl (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end Shl;

   function Build_L_Shr
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildLShr";
   function L_Shr
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_L_Shr (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end L_Shr;

   function Build_A_Shr
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildAShr";
   function A_Shr
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_A_Shr (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end A_Shr;

   function Build_And
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildAnd";
   function Build_And
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_And (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end Build_And;

   function Build_Or
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildOr";
   function Build_Or
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Or (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end Build_Or;

   function Build_Xor
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildXor";
   function Build_Xor
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Xor (Arg_1, LHS, RHS, Name_String);
      return Return_Value;
   end Build_Xor;

   function Build_Bin_Op
     (B    : LLVM.Types.Builder_T;
      Op   : Opcode_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildBinOp";
   function Bin_Op
     (B    : LLVM.Types.Builder_T;
      Op   : Opcode_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Bin_Op (B, Op, LHS, RHS, Name_String);
      return Return_Value;
   end Bin_Op;

   function Build_Neg
     (Arg_1 : LLVM.Types.Builder_T;
      V     : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildNeg";
   function Neg
     (Arg_1 : LLVM.Types.Builder_T;
      V     : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Neg (Arg_1, V, Name_String);
      return Return_Value;
   end Neg;

   function Build_NSW_Neg
     (B    : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildNSWNeg";
   function NSW_Neg
     (B    : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_NSW_Neg (B, V, Name_String);
      return Return_Value;
   end NSW_Neg;

   function Build_NUW_Neg
     (B    : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildNUWNeg";
   function NUW_Neg
     (B    : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_NUW_Neg (B, V, Name_String);
      return Return_Value;
   end NUW_Neg;

   function Build_F_Neg
     (Arg_1 : LLVM.Types.Builder_T;
      V     : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildFNeg";
   function F_Neg
     (Arg_1 : LLVM.Types.Builder_T;
      V     : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_F_Neg (Arg_1, V, Name_String);
      return Return_Value;
   end F_Neg;

   function Build_Not
     (Arg_1 : LLVM.Types.Builder_T;
      V     : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildNot";
   function Build_Not
     (Arg_1 : LLVM.Types.Builder_T;
      V     : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Not (Arg_1, V, Name_String);
      return Return_Value;
   end Build_Not;

   function Build_Malloc
     (Arg_1 : LLVM.Types.Builder_T;
      Ty    : LLVM.Types.Type_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildMalloc";
   function Malloc
     (Arg_1 : LLVM.Types.Builder_T;
      Ty    : LLVM.Types.Type_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Malloc (Arg_1, Ty, Name_String);
      return Return_Value;
   end Malloc;

   function Build_Array_Malloc
     (Arg_1 : LLVM.Types.Builder_T;
      Ty    : LLVM.Types.Type_T;
      Val   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildArrayMalloc";
   function Array_Malloc
     (Arg_1 : LLVM.Types.Builder_T;
      Ty    : LLVM.Types.Type_T;
      Val   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Array_Malloc (Arg_1, Ty, Val, Name_String);
      return Return_Value;
   end Array_Malloc;

   function Build_Alloca
     (Arg_1 : LLVM.Types.Builder_T;
      Ty    : LLVM.Types.Type_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildAlloca";
   function Alloca
     (Arg_1 : LLVM.Types.Builder_T;
      Ty    : LLVM.Types.Type_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Alloca (Arg_1, Ty, Name_String);
      return Return_Value;
   end Alloca;

   function Build_Array_Alloca
     (Arg_1 : LLVM.Types.Builder_T;
      Ty    : LLVM.Types.Type_T;
      Val   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildArrayAlloca";
   function Array_Alloca
     (Arg_1 : LLVM.Types.Builder_T;
      Ty    : LLVM.Types.Type_T;
      Val   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Array_Alloca (Arg_1, Ty, Val, Name_String);
      return Return_Value;
   end Array_Alloca;

   function Build_Load_2
     (Arg_1       : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pointer_Val : LLVM.Types.Value_T;
      Name        : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildLoad2";
   function Load_2
     (Arg_1       : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pointer_Val : LLVM.Types.Value_T;
      Name        : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Load_2 (Arg_1, Ty, Pointer_Val, Name_String);
      return Return_Value;
   end Load_2;

   function Build_GEP2
     (B           : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pointer     : LLVM.Types.Value_T;
      Indices     : System.Address;
      Num_Indices : unsigned;
      Name        : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildGEP2";
   function GEP2
     (B           : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pointer     : LLVM.Types.Value_T;
      Indices     : System.Address;
      Num_Indices : unsigned;
      Name        : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_GEP2 (B, Ty, Pointer, Indices, Num_Indices, Name_String);
      return Return_Value;
   end GEP2;

   function Build_In_Bounds_GEP2
     (B           : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pointer     : LLVM.Types.Value_T;
      Indices     : System.Address;
      Num_Indices : unsigned;
      Name        : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildInBoundsGEP2";
   function In_Bounds_GEP2
     (B           : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pointer     : LLVM.Types.Value_T;
      Indices     : System.Address;
      Num_Indices : unsigned;
      Name        : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_In_Bounds_GEP2 (B, Ty, Pointer, Indices, Num_Indices, Name_String);
      return Return_Value;
   end In_Bounds_GEP2;

   function Build_Struct_GEP2
     (B       : LLVM.Types.Builder_T;
      Ty      : LLVM.Types.Type_T;
      Pointer : LLVM.Types.Value_T;
      Idx     : unsigned;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildStructGEP2";
   function Struct_GEP2
     (B       : LLVM.Types.Builder_T;
      Ty      : LLVM.Types.Type_T;
      Pointer : LLVM.Types.Value_T;
      Idx     : unsigned;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Struct_GEP2 (B, Ty, Pointer, Idx, Name_String);
      return Return_Value;
   end Struct_GEP2;

   function Build_Global_String
     (B    : LLVM.Types.Builder_T;
      Str  : Interfaces.C.Strings.chars_ptr;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildGlobalString";
   function Global_String
     (B    : LLVM.Types.Builder_T;
      Str  : String;
      Name : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Str_Array    : aliased char_array := To_C (Str);
      Str_String   : constant chars_ptr := To_Chars_Ptr (Str_Array'Unchecked_Access);
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Global_String (B, Str_String, Name_String);
      return Return_Value;
   end Global_String;

   function Build_Global_String_Ptr
     (B    : LLVM.Types.Builder_T;
      Str  : Interfaces.C.Strings.chars_ptr;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildGlobalStringPtr";
   function Global_String_Ptr
     (B    : LLVM.Types.Builder_T;
      Str  : String;
      Name : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Str_Array    : aliased char_array := To_C (Str);
      Str_String   : constant chars_ptr := To_Chars_Ptr (Str_Array'Unchecked_Access);
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Global_String_Ptr (B, Str_String, Name_String);
      return Return_Value;
   end Global_String_Ptr;

   function Get_Volatile
     (Memory_Access_Inst : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetVolatile";
   function Get_Volatile
     (Memory_Access_Inst : LLVM.Types.Value_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Get_Volatile (Memory_Access_Inst);
      return Return_Value /= 0;
   end Get_Volatile;

   procedure Set_Volatile
     (Memory_Access_Inst : LLVM.Types.Value_T;
      Is_Volatile        : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetVolatile";
   procedure Set_Volatile
     (Memory_Access_Inst : LLVM.Types.Value_T;
      Is_Volatile        : Boolean)
   is
      Is_Volatile_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Volatile);
   begin
      Set_Volatile (Memory_Access_Inst, Is_Volatile_Bool);
   end Set_Volatile;

   function Get_Weak
     (Cmp_Xchg_Inst : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetWeak";
   function Get_Weak
     (Cmp_Xchg_Inst : LLVM.Types.Value_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Get_Weak (Cmp_Xchg_Inst);
      return Return_Value /= 0;
   end Get_Weak;

   procedure Set_Weak
     (Cmp_Xchg_Inst : LLVM.Types.Value_T;
      Is_Weak       : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetWeak";
   procedure Set_Weak
     (Cmp_Xchg_Inst : LLVM.Types.Value_T;
      Is_Weak       : Boolean)
   is
      Is_Weak_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Weak);
   begin
      Set_Weak (Cmp_Xchg_Inst, Is_Weak_Bool);
   end Set_Weak;

   function Build_Trunc
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildTrunc";
   function Trunc
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Trunc (Arg_1, Val, Dest_Ty, Name_String);
      return Return_Value;
   end Trunc;

   function Build_Z_Ext
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildZExt";
   function Z_Ext
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Z_Ext (Arg_1, Val, Dest_Ty, Name_String);
      return Return_Value;
   end Z_Ext;

   function Build_S_Ext
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildSExt";
   function S_Ext
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_S_Ext (Arg_1, Val, Dest_Ty, Name_String);
      return Return_Value;
   end S_Ext;

   function Build_FP_To_UI
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildFPToUI";
   function FP_To_UI
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_FP_To_UI (Arg_1, Val, Dest_Ty, Name_String);
      return Return_Value;
   end FP_To_UI;

   function Build_FP_To_SI
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildFPToSI";
   function FP_To_SI
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_FP_To_SI (Arg_1, Val, Dest_Ty, Name_String);
      return Return_Value;
   end FP_To_SI;

   function Build_UI_To_FP
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildUIToFP";
   function UI_To_FP
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_UI_To_FP (Arg_1, Val, Dest_Ty, Name_String);
      return Return_Value;
   end UI_To_FP;

   function Build_SI_To_FP
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildSIToFP";
   function SI_To_FP
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_SI_To_FP (Arg_1, Val, Dest_Ty, Name_String);
      return Return_Value;
   end SI_To_FP;

   function Build_FP_Trunc
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildFPTrunc";
   function FP_Trunc
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_FP_Trunc (Arg_1, Val, Dest_Ty, Name_String);
      return Return_Value;
   end FP_Trunc;

   function Build_FP_Ext
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildFPExt";
   function FP_Ext
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_FP_Ext (Arg_1, Val, Dest_Ty, Name_String);
      return Return_Value;
   end FP_Ext;

   function Build_Ptr_To_Int
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildPtrToInt";
   function Ptr_To_Int
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Ptr_To_Int (Arg_1, Val, Dest_Ty, Name_String);
      return Return_Value;
   end Ptr_To_Int;

   function Build_Int_To_Ptr
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildIntToPtr";
   function Int_To_Ptr
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Int_To_Ptr (Arg_1, Val, Dest_Ty, Name_String);
      return Return_Value;
   end Int_To_Ptr;

   function Build_Bit_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildBitCast";
   function Bit_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Bit_Cast (Arg_1, Val, Dest_Ty, Name_String);
      return Return_Value;
   end Bit_Cast;

   function Build_Addr_Space_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildAddrSpaceCast";
   function Addr_Space_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Addr_Space_Cast (Arg_1, Val, Dest_Ty, Name_String);
      return Return_Value;
   end Addr_Space_Cast;

   function Build_Z_Ext_Or_Bit_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildZExtOrBitCast";
   function Z_Ext_Or_Bit_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Z_Ext_Or_Bit_Cast (Arg_1, Val, Dest_Ty, Name_String);
      return Return_Value;
   end Z_Ext_Or_Bit_Cast;

   function Build_S_Ext_Or_Bit_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildSExtOrBitCast";
   function S_Ext_Or_Bit_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_S_Ext_Or_Bit_Cast (Arg_1, Val, Dest_Ty, Name_String);
      return Return_Value;
   end S_Ext_Or_Bit_Cast;

   function Build_Trunc_Or_Bit_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildTruncOrBitCast";
   function Trunc_Or_Bit_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Trunc_Or_Bit_Cast (Arg_1, Val, Dest_Ty, Name_String);
      return Return_Value;
   end Trunc_Or_Bit_Cast;

   function Build_Cast
     (B       : LLVM.Types.Builder_T;
      Op      : Opcode_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildCast";
   function Cast
     (B       : LLVM.Types.Builder_T;
      Op      : Opcode_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Cast (B, Op, Val, Dest_Ty, Name_String);
      return Return_Value;
   end Cast;

   function Build_Pointer_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildPointerCast";
   function Pointer_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Pointer_Cast (Arg_1, Val, Dest_Ty, Name_String);
      return Return_Value;
   end Pointer_Cast;

   function Build_Int_Cast_2
     (Arg_1     : LLVM.Types.Builder_T;
      Val       : LLVM.Types.Value_T;
      Dest_Ty   : LLVM.Types.Type_T;
      Is_Signed : LLVM.Types.Bool_T;
      Name      : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildIntCast2";
   function Int_Cast_2
     (Arg_1     : LLVM.Types.Builder_T;
      Val       : LLVM.Types.Value_T;
      Dest_Ty   : LLVM.Types.Type_T;
      Is_Signed : LLVM.Types.Bool_T;
      Name      : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Int_Cast_2 (Arg_1, Val, Dest_Ty, Is_Signed, Name_String);
      return Return_Value;
   end Int_Cast_2;

   function Int_Cast_2
     (Arg_1     : LLVM.Types.Builder_T;
      Val       : LLVM.Types.Value_T;
      Dest_Ty   : LLVM.Types.Type_T;
      Is_Signed : Boolean;
      Name      : String)
      return LLVM.Types.Value_T
   is
      Return_Value   : LLVM.Types.Value_T;
      Is_Signed_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Signed);
   begin
      Return_Value := Int_Cast_2 (Arg_1, Val, Dest_Ty, Is_Signed_Bool, Name);
      return Return_Value;
   end Int_Cast_2;

   function Build_FP_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildFPCast";
   function FP_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_FP_Cast (Arg_1, Val, Dest_Ty, Name_String);
      return Return_Value;
   end FP_Cast;

   function Build_Int_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildIntCast";
   function Int_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Int_Cast (Arg_1, Val, Dest_Ty, Name_String);
      return Return_Value;
   end Int_Cast;

   function Get_Cast_Opcode
     (Src            : LLVM.Types.Value_T;
      Src_Is_Signed  : LLVM.Types.Bool_T;
      Dest_Ty        : LLVM.Types.Type_T;
      Dest_Is_Signed : LLVM.Types.Bool_T)
      return Opcode_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetCastOpcode";
   function Get_Cast_Opcode
     (Src            : LLVM.Types.Value_T;
      Src_Is_Signed  : Boolean;
      Dest_Ty        : LLVM.Types.Type_T;
      Dest_Is_Signed : Boolean)
      return Opcode_T
   is
      Return_Value        : Opcode_T;
      Src_Is_Signed_Bool  : constant LLVM.Types.Bool_T := Boolean'Pos (Src_Is_Signed);
      Dest_Is_Signed_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Dest_Is_Signed);
   begin
      Return_Value := Get_Cast_Opcode (Src, Src_Is_Signed_Bool, Dest_Ty, Dest_Is_Signed_Bool);
      return Return_Value;
   end Get_Cast_Opcode;

   function Build_I_Cmp
     (Arg_1 : LLVM.Types.Builder_T;
      Op    : Int_Predicate_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildICmp";
   function I_Cmp
     (Arg_1 : LLVM.Types.Builder_T;
      Op    : Int_Predicate_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_I_Cmp (Arg_1, Op, LHS, RHS, Name_String);
      return Return_Value;
   end I_Cmp;

   function Build_F_Cmp
     (Arg_1 : LLVM.Types.Builder_T;
      Op    : Real_Predicate_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildFCmp";
   function F_Cmp
     (Arg_1 : LLVM.Types.Builder_T;
      Op    : Real_Predicate_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_F_Cmp (Arg_1, Op, LHS, RHS, Name_String);
      return Return_Value;
   end F_Cmp;

   function Build_Phi
     (Arg_1 : LLVM.Types.Builder_T;
      Ty    : LLVM.Types.Type_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildPhi";
   function Phi
     (Arg_1 : LLVM.Types.Builder_T;
      Ty    : LLVM.Types.Type_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Phi (Arg_1, Ty, Name_String);
      return Return_Value;
   end Phi;

   function Build_Call_2
     (Arg_1    : LLVM.Types.Builder_T;
      Arg_2    : LLVM.Types.Type_T;
      Fn       : LLVM.Types.Value_T;
      Args     : System.Address;
      Num_Args : unsigned;
      Name     : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildCall2";
   function Call_2
     (Arg_1    : LLVM.Types.Builder_T;
      Arg_2    : LLVM.Types.Type_T;
      Fn       : LLVM.Types.Value_T;
      Args     : System.Address;
      Num_Args : unsigned;
      Name     : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Call_2 (Arg_1, Arg_2, Fn, Args, Num_Args, Name_String);
      return Return_Value;
   end Call_2;

   function Build_Select
     (Arg_1  : LLVM.Types.Builder_T;
      C_If   : LLVM.Types.Value_T;
      C_Then : LLVM.Types.Value_T;
      C_Else : LLVM.Types.Value_T;
      Name   : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildSelect";
   function Build_Select
     (Arg_1  : LLVM.Types.Builder_T;
      C_If   : LLVM.Types.Value_T;
      C_Then : LLVM.Types.Value_T;
      C_Else : LLVM.Types.Value_T;
      Name   : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Select (Arg_1, C_If, C_Then, C_Else, Name_String);
      return Return_Value;
   end Build_Select;

   function Build_VA_Arg
     (Arg_1 : LLVM.Types.Builder_T;
      List  : LLVM.Types.Value_T;
      Ty    : LLVM.Types.Type_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildVAArg";
   function VA_Arg
     (Arg_1 : LLVM.Types.Builder_T;
      List  : LLVM.Types.Value_T;
      Ty    : LLVM.Types.Type_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_VA_Arg (Arg_1, List, Ty, Name_String);
      return Return_Value;
   end VA_Arg;

   function Build_Extract_Element
     (Arg_1   : LLVM.Types.Builder_T;
      Vec_Val : LLVM.Types.Value_T;
      Index   : LLVM.Types.Value_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildExtractElement";
   function Extract_Element
     (Arg_1   : LLVM.Types.Builder_T;
      Vec_Val : LLVM.Types.Value_T;
      Index   : LLVM.Types.Value_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Extract_Element (Arg_1, Vec_Val, Index, Name_String);
      return Return_Value;
   end Extract_Element;

   function Build_Insert_Element
     (Arg_1   : LLVM.Types.Builder_T;
      Vec_Val : LLVM.Types.Value_T;
      Elt_Val : LLVM.Types.Value_T;
      Index   : LLVM.Types.Value_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildInsertElement";
   function Insert_Element
     (Arg_1   : LLVM.Types.Builder_T;
      Vec_Val : LLVM.Types.Value_T;
      Elt_Val : LLVM.Types.Value_T;
      Index   : LLVM.Types.Value_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Insert_Element (Arg_1, Vec_Val, Elt_Val, Index, Name_String);
      return Return_Value;
   end Insert_Element;

   function Build_Shuffle_Vector
     (Arg_1 : LLVM.Types.Builder_T;
      V1    : LLVM.Types.Value_T;
      V2    : LLVM.Types.Value_T;
      Mask  : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildShuffleVector";
   function Shuffle_Vector
     (Arg_1 : LLVM.Types.Builder_T;
      V1    : LLVM.Types.Value_T;
      V2    : LLVM.Types.Value_T;
      Mask  : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Shuffle_Vector (Arg_1, V1, V2, Mask, Name_String);
      return Return_Value;
   end Shuffle_Vector;

   function Build_Extract_Value
     (Arg_1   : LLVM.Types.Builder_T;
      Agg_Val : LLVM.Types.Value_T;
      Index   : unsigned;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildExtractValue";
   function Extract_Value
     (Arg_1   : LLVM.Types.Builder_T;
      Agg_Val : LLVM.Types.Value_T;
      Index   : unsigned;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Extract_Value (Arg_1, Agg_Val, Index, Name_String);
      return Return_Value;
   end Extract_Value;

   function Build_Insert_Value
     (Arg_1   : LLVM.Types.Builder_T;
      Agg_Val : LLVM.Types.Value_T;
      Elt_Val : LLVM.Types.Value_T;
      Index   : unsigned;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildInsertValue";
   function Insert_Value
     (Arg_1   : LLVM.Types.Builder_T;
      Agg_Val : LLVM.Types.Value_T;
      Elt_Val : LLVM.Types.Value_T;
      Index   : unsigned;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Insert_Value (Arg_1, Agg_Val, Elt_Val, Index, Name_String);
      return Return_Value;
   end Insert_Value;

   function Build_Freeze
     (Arg_1 : LLVM.Types.Builder_T;
      Val   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildFreeze";
   function Freeze
     (Arg_1 : LLVM.Types.Builder_T;
      Val   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Freeze (Arg_1, Val, Name_String);
      return Return_Value;
   end Freeze;

   function Build_Is_Null
     (Arg_1 : LLVM.Types.Builder_T;
      Val   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildIsNull";
   function Is_Null
     (Arg_1 : LLVM.Types.Builder_T;
      Val   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Is_Null (Arg_1, Val, Name_String);
      return Return_Value;
   end Is_Null;

   function Build_Is_Not_Null
     (Arg_1 : LLVM.Types.Builder_T;
      Val   : LLVM.Types.Value_T;
      Name  : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildIsNotNull";
   function Is_Not_Null
     (Arg_1 : LLVM.Types.Builder_T;
      Val   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Is_Not_Null (Arg_1, Val, Name_String);
      return Return_Value;
   end Is_Not_Null;

   function Build_Ptr_Diff_2
     (Arg_1   : LLVM.Types.Builder_T;
      Elem_Ty : LLVM.Types.Type_T;
      LHS     : LLVM.Types.Value_T;
      RHS     : LLVM.Types.Value_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildPtrDiff2";
   function Ptr_Diff_2
     (Arg_1   : LLVM.Types.Builder_T;
      Elem_Ty : LLVM.Types.Type_T;
      LHS     : LLVM.Types.Value_T;
      RHS     : LLVM.Types.Value_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Ptr_Diff_2 (Arg_1, Elem_Ty, LHS, RHS, Name_String);
      return Return_Value;
   end Ptr_Diff_2;

   function Build_Fence
     (B             : LLVM.Types.Builder_T;
      Ordering      : Atomic_Ordering_T;
      Single_Thread : LLVM.Types.Bool_T;
      Name          : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildFence";
   function Fence
     (B             : LLVM.Types.Builder_T;
      Ordering      : Atomic_Ordering_T;
      Single_Thread : LLVM.Types.Bool_T;
      Name          : String)
      return LLVM.Types.Value_T
   is
      Return_Value : LLVM.Types.Value_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Build_Fence (B, Ordering, Single_Thread, Name_String);
      return Return_Value;
   end Fence;

   function Fence
     (B             : LLVM.Types.Builder_T;
      Ordering      : Atomic_Ordering_T;
      Single_Thread : Boolean;
      Name          : String)
      return LLVM.Types.Value_T
   is
      Return_Value       : LLVM.Types.Value_T;
      Single_Thread_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Single_Thread);
   begin
      Return_Value := Fence (B, Ordering, Single_Thread_Bool, Name);
      return Return_Value;
   end Fence;

   function Build_Atomic_RMW
     (B             : LLVM.Types.Builder_T;
      Op            : Atomic_RMW_Bin_Op_T;
      PTR           : LLVM.Types.Value_T;
      Val           : LLVM.Types.Value_T;
      Ordering      : Atomic_Ordering_T;
      Single_Thread : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildAtomicRMW";
   function Atomic_RMW
     (B             : LLVM.Types.Builder_T;
      Op            : Atomic_RMW_Bin_Op_T;
      PTR           : LLVM.Types.Value_T;
      Val           : LLVM.Types.Value_T;
      Ordering      : Atomic_Ordering_T;
      Single_Thread : Boolean)
      return LLVM.Types.Value_T
   is
      Return_Value       : LLVM.Types.Value_T;
      Single_Thread_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Single_Thread);
   begin
      Return_Value := Build_Atomic_RMW (B, Op, PTR, Val, Ordering, Single_Thread_Bool);
      return Return_Value;
   end Atomic_RMW;

   function Build_Atomic_Cmp_Xchg
     (B                : LLVM.Types.Builder_T;
      Ptr              : LLVM.Types.Value_T;
      Cmp              : LLVM.Types.Value_T;
      C_New            : LLVM.Types.Value_T;
      Success_Ordering : Atomic_Ordering_T;
      Failure_Ordering : Atomic_Ordering_T;
      Single_Thread    : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMBuildAtomicCmpXchg";
   function Atomic_Cmp_Xchg
     (B                : LLVM.Types.Builder_T;
      Ptr              : LLVM.Types.Value_T;
      Cmp              : LLVM.Types.Value_T;
      C_New            : LLVM.Types.Value_T;
      Success_Ordering : Atomic_Ordering_T;
      Failure_Ordering : Atomic_Ordering_T;
      Single_Thread    : Boolean)
      return LLVM.Types.Value_T
   is
      Return_Value       : LLVM.Types.Value_T;
      Single_Thread_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Single_Thread);
   begin
      Return_Value := Build_Atomic_Cmp_Xchg (B, Ptr, Cmp, C_New, Success_Ordering, Failure_Ordering, Single_Thread_Bool);
      return Return_Value;
   end Atomic_Cmp_Xchg;

   function Is_Atomic_Single_Thread
     (Atomic_Inst : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsAtomicSingleThread";
   function Is_Atomic_Single_Thread
     (Atomic_Inst : LLVM.Types.Value_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Atomic_Single_Thread (Atomic_Inst);
      return Return_Value /= 0;
   end Is_Atomic_Single_Thread;

   procedure Set_Atomic_Single_Thread
     (Atomic_Inst   : LLVM.Types.Value_T;
      Single_Thread : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetAtomicSingleThread";
   procedure Set_Atomic_Single_Thread
     (Atomic_Inst   : LLVM.Types.Value_T;
      Single_Thread : Boolean)
   is
      Single_Thread_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Single_Thread);
   begin
      Set_Atomic_Single_Thread (Atomic_Inst, Single_Thread_Bool);
   end Set_Atomic_Single_Thread;

   function Create_Memory_Buffer_With_Contents_Of_File
     (Path        : Interfaces.C.Strings.chars_ptr;
      Out_Mem_Buf : System.Address;
      Out_Message : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMCreateMemoryBufferWithContentsOfFile";
   function Create_Memory_Buffer_With_Contents_Of_File
     (Path        : String;
      Out_Mem_Buf : System.Address;
      Out_Message : System.Address)
      return LLVM.Types.Bool_T
   is
      Return_Value : LLVM.Types.Bool_T;
      Path_Array   : aliased char_array := To_C (Path);
      Path_String  : constant chars_ptr := To_Chars_Ptr (Path_Array'Unchecked_Access);
   begin
      Return_Value := Create_Memory_Buffer_With_Contents_Of_File (Path_String, Out_Mem_Buf, Out_Message);
      return Return_Value;
   end Create_Memory_Buffer_With_Contents_Of_File;

   function Create_Memory_Buffer_With_Contents_Of_File
     (Path        : String;
      Out_Mem_Buf : System.Address;
      Out_Message : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Create_Memory_Buffer_With_Contents_Of_File (Path, Out_Mem_Buf, Out_Message);
      return Return_Value /= 0;
   end Create_Memory_Buffer_With_Contents_Of_File;

   function Create_Memory_Buffer_With_STDIN
     (Out_Mem_Buf : System.Address;
      Out_Message : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMCreateMemoryBufferWithSTDIN";
   function Create_Memory_Buffer_With_STDIN
     (Out_Mem_Buf : System.Address;
      Out_Message : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Create_Memory_Buffer_With_STDIN (Out_Mem_Buf, Out_Message);
      return Return_Value /= 0;
   end Create_Memory_Buffer_With_STDIN;

   function Create_Memory_Buffer_With_Memory_Range
     (Input_Data               : Interfaces.C.Strings.chars_ptr;
      Input_Data_Length        : stddef_h.size_t;
      Buffer_Name              : Interfaces.C.Strings.chars_ptr;
      Requires_Null_Terminator : LLVM.Types.Bool_T)
      return LLVM.Types.Memory_Buffer_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMCreateMemoryBufferWithMemoryRange";
   function Create_Memory_Buffer_With_Memory_Range
     (Input_Data               : String;
      Input_Data_Length        : stddef_h.size_t;
      Buffer_Name              : String;
      Requires_Null_Terminator : LLVM.Types.Bool_T)
      return LLVM.Types.Memory_Buffer_T
   is
      Return_Value       : LLVM.Types.Memory_Buffer_T;
      Input_Data_Array   : aliased char_array := To_C (Input_Data);
      Input_Data_String  : constant chars_ptr := To_Chars_Ptr (Input_Data_Array'Unchecked_Access);
      Buffer_Name_Array  : aliased char_array := To_C (Buffer_Name);
      Buffer_Name_String : constant chars_ptr := To_Chars_Ptr (Buffer_Name_Array'Unchecked_Access);
   begin
      Return_Value := Create_Memory_Buffer_With_Memory_Range (Input_Data_String, Input_Data_Length, Buffer_Name_String, Requires_Null_Terminator);
      return Return_Value;
   end Create_Memory_Buffer_With_Memory_Range;

   function Create_Memory_Buffer_With_Memory_Range
     (Input_Data               : String;
      Input_Data_Length        : stddef_h.size_t;
      Buffer_Name              : String;
      Requires_Null_Terminator : Boolean)
      return LLVM.Types.Memory_Buffer_T
   is
      Return_Value                  : LLVM.Types.Memory_Buffer_T;
      Requires_Null_Terminator_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Requires_Null_Terminator);
   begin
      Return_Value := Create_Memory_Buffer_With_Memory_Range (Input_Data, Input_Data_Length, Buffer_Name, Requires_Null_Terminator_Bool);
      return Return_Value;
   end Create_Memory_Buffer_With_Memory_Range;

   function Create_Memory_Buffer_With_Memory_Range_Copy
     (Input_Data        : Interfaces.C.Strings.chars_ptr;
      Input_Data_Length : stddef_h.size_t;
      Buffer_Name       : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Memory_Buffer_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMCreateMemoryBufferWithMemoryRangeCopy";
   function Create_Memory_Buffer_With_Memory_Range_Copy
     (Input_Data        : String;
      Input_Data_Length : stddef_h.size_t;
      Buffer_Name       : String)
      return LLVM.Types.Memory_Buffer_T
   is
      Return_Value       : LLVM.Types.Memory_Buffer_T;
      Input_Data_Array   : aliased char_array := To_C (Input_Data);
      Input_Data_String  : constant chars_ptr := To_Chars_Ptr (Input_Data_Array'Unchecked_Access);
      Buffer_Name_Array  : aliased char_array := To_C (Buffer_Name);
      Buffer_Name_String : constant chars_ptr := To_Chars_Ptr (Buffer_Name_Array'Unchecked_Access);
   begin
      Return_Value := Create_Memory_Buffer_With_Memory_Range_Copy (Input_Data_String, Input_Data_Length, Buffer_Name_String);
      return Return_Value;
   end Create_Memory_Buffer_With_Memory_Range_Copy;

   function Get_Buffer_Start
     (Mem_Buf : LLVM.Types.Memory_Buffer_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetBufferStart";
   function Get_Buffer_Start
     (Mem_Buf : LLVM.Types.Memory_Buffer_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Buffer_Start (Mem_Buf);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Buffer_Start;

   function Run_Pass_Manager
     (PM : LLVM.Types.Pass_Manager_T;
      M  : LLVM.Types.Module_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMRunPassManager";
   function Run_Pass_Manager
     (PM : LLVM.Types.Pass_Manager_T;
      M  : LLVM.Types.Module_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Run_Pass_Manager (PM, M);
      return Return_Value /= 0;
   end Run_Pass_Manager;

   function Initialize_Function_Pass_Manager
     (FPM : LLVM.Types.Pass_Manager_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMInitializeFunctionPassManager";
   function Initialize_Function_Pass_Manager
     (FPM : LLVM.Types.Pass_Manager_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Initialize_Function_Pass_Manager (FPM);
      return Return_Value /= 0;
   end Initialize_Function_Pass_Manager;

   function Run_Function_Pass_Manager
     (FPM : LLVM.Types.Pass_Manager_T;
      F   : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMRunFunctionPassManager";
   function Run_Function_Pass_Manager
     (FPM : LLVM.Types.Pass_Manager_T;
      F   : LLVM.Types.Value_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Run_Function_Pass_Manager (FPM, F);
      return Return_Value /= 0;
   end Run_Function_Pass_Manager;

   function Finalize_Function_Pass_Manager
     (FPM : LLVM.Types.Pass_Manager_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMFinalizeFunctionPassManager";
   function Finalize_Function_Pass_Manager
     (FPM : LLVM.Types.Pass_Manager_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Finalize_Function_Pass_Manager (FPM);
      return Return_Value /= 0;
   end Finalize_Function_Pass_Manager;

   function Start_Multithreaded
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMStartMultithreaded";
   function Start_Multithreaded
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Start_Multithreaded;
      return Return_Value /= 0;
   end Start_Multithreaded;

   function Is_Multithreaded
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsMultithreaded";
   function Is_Multithreaded
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Multithreaded;
      return Return_Value /= 0;
   end Is_Multithreaded;

end LLVM.Core;
