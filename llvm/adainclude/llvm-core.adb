pragma Ada_2005;
pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

with LLVM.Types; use LLVM.Types;

package body LLVM.Core is

   function Create_Message
     (Message : String)
      return String
   is
      Message_Array  : aliased char_array := To_C (Message);
      Message_String : constant chars_ptr := To_Chars_Ptr (Message_Array'Unchecked_Access);
   begin
      return Value (Create_Message_C (Message_String));
   end Create_Message;

   function Get_Diag_Info_Description
     (DI : LLVM.Types.Diagnostic_Info_T)
      return String
   is
   begin
      return Value (Get_Diag_Info_Description_C (DI));
   end Get_Diag_Info_Description;

   function Get_MD_Kind_ID_In_Context
     (C     : LLVM.Types.Context_T;
      Name  : String;
      S_Len : unsigned)
      return unsigned
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Get_MD_Kind_ID_In_Context_C (C, Name_String, S_Len);
   end Get_MD_Kind_ID_In_Context;

   function Get_MD_Kind_ID
     (Name  : String;
      S_Len : unsigned)
      return unsigned
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Get_MD_Kind_ID_C (Name_String, S_Len);
   end Get_MD_Kind_ID;

   function Get_Enum_Attribute_Kind_For_Name
     (Name  : String;
      S_Len : stddef_h.size_t)
      return unsigned
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Get_Enum_Attribute_Kind_For_Name_C (Name_String, S_Len);
   end Get_Enum_Attribute_Kind_For_Name;

   function Create_String_Attribute
     (C        : LLVM.Types.Context_T;
      K        : String;
      K_Length : unsigned;
      V        : String;
      V_Length : unsigned)
      return LLVM.Types.Attribute_T
   is
      K_Array  : aliased char_array := To_C (K);
      K_String : constant chars_ptr := To_Chars_Ptr (K_Array'Unchecked_Access);
      V_Array  : aliased char_array := To_C (V);
      V_String : constant chars_ptr := To_Chars_Ptr (V_Array'Unchecked_Access);
   begin
      return Create_String_Attribute_C (C, K_String, K_Length, V_String, V_Length);
   end Create_String_Attribute;

   function Get_String_Attribute_Kind
     (A      : LLVM.Types.Attribute_T;
      Length : unsigned)
      return String
   is
   begin
      return Value (Get_String_Attribute_Kind_C (A, Length));
   end Get_String_Attribute_Kind;

   function Get_String_Attribute_Value
     (A      : LLVM.Types.Attribute_T;
      Length : unsigned)
      return String
   is
   begin
      return Value (Get_String_Attribute_Value_C (A, Length));
   end Get_String_Attribute_Value;

   function Is_Enum_Attribute
     (A : LLVM.Types.Attribute_T)
      return Boolean
   is
   begin
      return Is_Enum_Attribute_C (A) /= 0;
   end Is_Enum_Attribute;

   function Is_String_Attribute
     (A : LLVM.Types.Attribute_T)
      return Boolean
   is
   begin
      return Is_String_Attribute_C (A) /= 0;
   end Is_String_Attribute;

   function Module_Create_With_Name
     (Module_ID : String)
      return LLVM.Types.Module_T
   is
      Module_ID_Array  : aliased char_array := To_C (Module_ID);
      Module_ID_String : constant chars_ptr := To_Chars_Ptr (Module_ID_Array'Unchecked_Access);
   begin
      return Module_Create_With_Name_C (Module_ID_String);
   end Module_Create_With_Name;

   function Module_Create_With_Name_In_Context
     (Module_ID : String;
      C         : LLVM.Types.Context_T)
      return LLVM.Types.Module_T
   is
      Module_ID_Array  : aliased char_array := To_C (Module_ID);
      Module_ID_String : constant chars_ptr := To_Chars_Ptr (Module_ID_Array'Unchecked_Access);
   begin
      return Module_Create_With_Name_In_Context_C (Module_ID_String, C);
   end Module_Create_With_Name_In_Context;

   function Get_Module_Identifier
     (M   : LLVM.Types.Module_T;
      Len : stddef_h.size_t)
      return String
   is
   begin
      return Value (Get_Module_Identifier_C (M, Len));
   end Get_Module_Identifier;

   function Get_Data_Layout_Str
     (M : LLVM.Types.Module_T)
      return String
   is
   begin
      return Value (Get_Data_Layout_Str_C (M));
   end Get_Data_Layout_Str;

   function Get_Data_Layout
     (M : LLVM.Types.Module_T)
      return String
   is
   begin
      return Value (Get_Data_Layout_C (M));
   end Get_Data_Layout;

   function Get_Target
     (M : LLVM.Types.Module_T)
      return String
   is
   begin
      return Value (Get_Target_C (M));
   end Get_Target;

   function Print_Module_To_File
     (M             : LLVM.Types.Module_T;
      Filename      : String;
      Error_Message : System.Address)
      return Boolean
   is
      Filename_Array  : aliased char_array := To_C (Filename);
      Filename_String : constant chars_ptr := To_Chars_Ptr (Filename_Array'Unchecked_Access);
   begin
      return Print_Module_To_File_C (M, Filename_String, Error_Message) /= 0;
   end Print_Module_To_File;

   function Print_Module_To_String
     (M : LLVM.Types.Module_T)
      return String
   is
   begin
      return Value (Print_Module_To_String_C (M));
   end Print_Module_To_String;

   function Get_Type_By_Name
     (M    : LLVM.Types.Module_T;
      Name : String)
      return LLVM.Types.Type_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Get_Type_By_Name_C (M, Name_String);
   end Get_Type_By_Name;

   function Get_Named_Metadata_Num_Operands
     (M    : LLVM.Types.Module_T;
      Name : String)
      return unsigned
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Get_Named_Metadata_Num_Operands_C (M, Name_String);
   end Get_Named_Metadata_Num_Operands;

   function Add_Function
     (M           : LLVM.Types.Module_T;
      Name        : String;
      Function_Ty : LLVM.Types.Type_T)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Add_Function_C (M, Name_String, Function_Ty);
   end Add_Function;

   function Get_Named_Function
     (M    : LLVM.Types.Module_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Get_Named_Function_C (M, Name_String);
   end Get_Named_Function;

   function Type_Is_Sized
     (Ty : LLVM.Types.Type_T)
      return Boolean
   is
   begin
      return Type_Is_Sized_C (Ty) /= 0;
   end Type_Is_Sized;

   function Print_Type_To_String
     (Val : LLVM.Types.Type_T)
      return String
   is
   begin
      return Value (Print_Type_To_String_C (Val));
   end Print_Type_To_String;

   function Function_Type
     (Return_Type : LLVM.Types.Type_T;
      Param_Types : System.Address;
      Param_Count : unsigned;
      Is_Var_Arg  : Boolean)
      return LLVM.Types.Type_T
   is
      Is_Var_Arg_Bool : constant Bool_T := Boolean'Pos (Is_Var_Arg);
   begin
      return Function_Type_C (Return_Type, Param_Types, Param_Count, Is_Var_Arg_Bool);
   end Function_Type;

   function Is_Function_Var_Arg
     (Function_Ty : LLVM.Types.Type_T)
      return Boolean
   is
   begin
      return Is_Function_Var_Arg_C (Function_Ty) /= 0;
   end Is_Function_Var_Arg;

   function Struct_Type_In_Context
     (C             : LLVM.Types.Context_T;
      Element_Types : System.Address;
      Element_Count : unsigned;
      Packed        : Boolean)
      return LLVM.Types.Type_T
   is
      Packed_Bool : constant Bool_T := Boolean'Pos (Packed);
   begin
      return Struct_Type_In_Context_C (C, Element_Types, Element_Count, Packed_Bool);
   end Struct_Type_In_Context;

   function Struct_Type
     (Element_Types : System.Address;
      Element_Count : unsigned;
      Packed        : Boolean)
      return LLVM.Types.Type_T
   is
      Packed_Bool : constant Bool_T := Boolean'Pos (Packed);
   begin
      return Struct_Type_C (Element_Types, Element_Count, Packed_Bool);
   end Struct_Type;

   function Struct_Create_Named
     (C    : LLVM.Types.Context_T;
      Name : String)
      return LLVM.Types.Type_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Struct_Create_Named_C (C, Name_String);
   end Struct_Create_Named;

   function Get_Struct_Name
     (Ty : LLVM.Types.Type_T)
      return String
   is
   begin
      return Value (Get_Struct_Name_C (Ty));
   end Get_Struct_Name;

   function Is_Packed_Struct
     (Struct_Ty : LLVM.Types.Type_T)
      return Boolean
   is
   begin
      return Is_Packed_Struct_C (Struct_Ty) /= 0;
   end Is_Packed_Struct;

   function Is_Opaque_Struct
     (Struct_Ty : LLVM.Types.Type_T)
      return Boolean
   is
   begin
      return Is_Opaque_Struct_C (Struct_Ty) /= 0;
   end Is_Opaque_Struct;

   function Get_Value_Name
     (Val : LLVM.Types.Value_T)
      return String
   is
   begin
      return Value (Get_Value_Name_C (Val));
   end Get_Value_Name;

   function Print_Value_To_String
     (Val : LLVM.Types.Value_T)
      return String
   is
   begin
      return Value (Print_Value_To_String_C (Val));
   end Print_Value_To_String;

   function Is_Constant
     (Val : LLVM.Types.Value_T)
      return Boolean
   is
   begin
      return Is_Constant_C (Val) /= 0;
   end Is_Constant;

   function Is_Undef
     (Val : LLVM.Types.Value_T)
      return Boolean
   is
   begin
      return Is_Undef_C (Val) /= 0;
   end Is_Undef;

   function Is_Null
     (Val : LLVM.Types.Value_T)
      return Boolean
   is
   begin
      return Is_Null_C (Val) /= 0;
   end Is_Null;

   function Const_Int
     (Int_Ty      : LLVM.Types.Type_T;
      N           : Extensions.unsigned_long_long;
      Sign_Extend : Boolean)
      return LLVM.Types.Value_T
   is
      Sign_Extend_Bool : constant Bool_T := Boolean'Pos (Sign_Extend);
   begin
      return Const_Int_C (Int_Ty, N, Sign_Extend_Bool);
   end Const_Int;

   function Const_Int_Of_String
     (Int_Ty : LLVM.Types.Type_T;
      Text   : String;
      Radix  : stdint_h.uint8_t)
      return LLVM.Types.Value_T
   is
      Text_Array  : aliased char_array := To_C (Text);
      Text_String : constant chars_ptr := To_Chars_Ptr (Text_Array'Unchecked_Access);
   begin
      return Const_Int_Of_String_C (Int_Ty, Text_String, Radix);
   end Const_Int_Of_String;

   function Const_Int_Of_String_And_Size
     (Int_Ty : LLVM.Types.Type_T;
      Text   : String;
      S_Len  : unsigned;
      Radix  : stdint_h.uint8_t)
      return LLVM.Types.Value_T
   is
      Text_Array  : aliased char_array := To_C (Text);
      Text_String : constant chars_ptr := To_Chars_Ptr (Text_Array'Unchecked_Access);
   begin
      return Const_Int_Of_String_And_Size_C (Int_Ty, Text_String, S_Len, Radix);
   end Const_Int_Of_String_And_Size;

   function Const_Real_Of_String
     (Real_Ty : LLVM.Types.Type_T;
      Text    : String)
      return LLVM.Types.Value_T
   is
      Text_Array  : aliased char_array := To_C (Text);
      Text_String : constant chars_ptr := To_Chars_Ptr (Text_Array'Unchecked_Access);
   begin
      return Const_Real_Of_String_C (Real_Ty, Text_String);
   end Const_Real_Of_String;

   function Const_Real_Of_String_And_Size
     (Real_Ty : LLVM.Types.Type_T;
      Text    : String;
      S_Len   : unsigned)
      return LLVM.Types.Value_T
   is
      Text_Array  : aliased char_array := To_C (Text);
      Text_String : constant chars_ptr := To_Chars_Ptr (Text_Array'Unchecked_Access);
   begin
      return Const_Real_Of_String_And_Size_C (Real_Ty, Text_String, S_Len);
   end Const_Real_Of_String_And_Size;

   function Const_Real_Get_Double
     (Constant_Val : LLVM.Types.Value_T;
      loses_Info   : Boolean)
      return double
   is
      loses_Info_Bool : constant Bool_T := Boolean'Pos (loses_Info);
   begin
      return Const_Real_Get_Double_C (Constant_Val, loses_Info_Bool);
   end Const_Real_Get_Double;

   function Const_String_In_Context
     (C                   : LLVM.Types.Context_T;
      Str                 : String;
      Length              : unsigned;
      Dont_Null_Terminate : Boolean)
      return LLVM.Types.Value_T
   is
      Str_Array                : aliased char_array := To_C (Str);
      Str_String               : constant chars_ptr := To_Chars_Ptr (Str_Array'Unchecked_Access);
      Dont_Null_Terminate_Bool : constant Bool_T := Boolean'Pos (Dont_Null_Terminate);
   begin
      return Const_String_In_Context_C (C, Str_String, Length, Dont_Null_Terminate_Bool);
   end Const_String_In_Context;

   function Const_String
     (Str                 : String;
      Length              : unsigned;
      Dont_Null_Terminate : Boolean)
      return LLVM.Types.Value_T
   is
      Str_Array                : aliased char_array := To_C (Str);
      Str_String               : constant chars_ptr := To_Chars_Ptr (Str_Array'Unchecked_Access);
      Dont_Null_Terminate_Bool : constant Bool_T := Boolean'Pos (Dont_Null_Terminate);
   begin
      return Const_String_C (Str_String, Length, Dont_Null_Terminate_Bool);
   end Const_String;

   function Is_Constant_String
     (c : LLVM.Types.Value_T)
      return Boolean
   is
   begin
      return Is_Constant_String_C (c) /= 0;
   end Is_Constant_String;

   function Get_As_String
     (c      : LLVM.Types.Value_T;
      Length : stddef_h.size_t)
      return String
   is
   begin
      return Value (Get_As_String_C (c, Length));
   end Get_As_String;

   function Const_Struct_In_Context
     (C             : LLVM.Types.Context_T;
      Constant_Vals : System.Address;
      Count         : unsigned;
      Packed        : Boolean)
      return LLVM.Types.Value_T
   is
      Packed_Bool : constant Bool_T := Boolean'Pos (Packed);
   begin
      return Const_Struct_In_Context_C (C, Constant_Vals, Count, Packed_Bool);
   end Const_Struct_In_Context;

   function Const_Struct
     (Constant_Vals : System.Address;
      Count         : unsigned;
      Packed        : Boolean)
      return LLVM.Types.Value_T
   is
      Packed_Bool : constant Bool_T := Boolean'Pos (Packed);
   begin
      return Const_Struct_C (Constant_Vals, Count, Packed_Bool);
   end Const_Struct;

   function Const_Int_Cast
     (Constant_Val : LLVM.Types.Value_T;
      To_Type      : LLVM.Types.Type_T;
      is_Signed    : Boolean)
      return LLVM.Types.Value_T
   is
      is_Signed_Bool : constant Bool_T := Boolean'Pos (is_Signed);
   begin
      return Const_Int_Cast_C (Constant_Val, To_Type, is_Signed_Bool);
   end Const_Int_Cast;

   function Const_Inline_Asm
     (Ty               : LLVM.Types.Type_T;
      Asm_String       : String;
      Constraints      : String;
      Has_Side_Effects : Boolean;
      Is_Align_Stack   : Boolean)
      return LLVM.Types.Value_T
   is
      Asm_String_Array      : aliased char_array := To_C (Asm_String);
      Asm_String_String     : constant chars_ptr := To_Chars_Ptr (Asm_String_Array'Unchecked_Access);
      Constraints_Array     : aliased char_array := To_C (Constraints);
      Constraints_String    : constant chars_ptr := To_Chars_Ptr (Constraints_Array'Unchecked_Access);
      Has_Side_Effects_Bool : constant Bool_T := Boolean'Pos (Has_Side_Effects);
      Is_Align_Stack_Bool   : constant Bool_T := Boolean'Pos (Is_Align_Stack);
   begin
      return Const_Inline_Asm_C (Ty, Asm_String_String, Constraints_String, Has_Side_Effects_Bool, Is_Align_Stack_Bool);
   end Const_Inline_Asm;

   function Is_Declaration
     (Global : LLVM.Types.Value_T)
      return Boolean
   is
   begin
      return Is_Declaration_C (Global) /= 0;
   end Is_Declaration;

   function Get_Section
     (Global : LLVM.Types.Value_T)
      return String
   is
   begin
      return Value (Get_Section_C (Global));
   end Get_Section;

   function Has_Unnamed_Addr
     (Global : LLVM.Types.Value_T)
      return Boolean
   is
   begin
      return Has_Unnamed_Addr_C (Global) /= 0;
   end Has_Unnamed_Addr;

   function Add_Global
     (M    : LLVM.Types.Module_T;
      Ty   : LLVM.Types.Type_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Add_Global_C (M, Ty, Name_String);
   end Add_Global;

   function Add_Global_In_Address_Space
     (M             : LLVM.Types.Module_T;
      Ty            : LLVM.Types.Type_T;
      Name          : String;
      Address_Space : unsigned)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Add_Global_In_Address_Space_C (M, Ty, Name_String, Address_Space);
   end Add_Global_In_Address_Space;

   function Get_Named_Global
     (M    : LLVM.Types.Module_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Get_Named_Global_C (M, Name_String);
   end Get_Named_Global;

   function Is_Thread_Local
     (Global_Var : LLVM.Types.Value_T)
      return Boolean
   is
   begin
      return Is_Thread_Local_C (Global_Var) /= 0;
   end Is_Thread_Local;

   function Is_Global_Constant
     (Global_Var : LLVM.Types.Value_T)
      return Boolean
   is
   begin
      return Is_Global_Constant_C (Global_Var) /= 0;
   end Is_Global_Constant;

   function Is_Externally_Initialized
     (Global_Var : LLVM.Types.Value_T)
      return Boolean
   is
   begin
      return Is_Externally_Initialized_C (Global_Var) /= 0;
   end Is_Externally_Initialized;

   function Add_Alias
     (M       : LLVM.Types.Module_T;
      Ty      : LLVM.Types.Type_T;
      Aliasee : LLVM.Types.Value_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Add_Alias_C (M, Ty, Aliasee, Name_String);
   end Add_Alias;

   function Has_Personality_Fn
     (Fn : LLVM.Types.Value_T)
      return Boolean
   is
   begin
      return Has_Personality_Fn_C (Fn) /= 0;
   end Has_Personality_Fn;

   function Get_GC
     (Fn : LLVM.Types.Value_T)
      return String
   is
   begin
      return Value (Get_GC_C (Fn));
   end Get_GC;

   function Get_String_Attribute_At_Index
     (F     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : String;
      K_Len : unsigned)
      return LLVM.Types.Attribute_T
   is
      K_Array  : aliased char_array := To_C (K);
      K_String : constant chars_ptr := To_Chars_Ptr (K_Array'Unchecked_Access);
   begin
      return Get_String_Attribute_At_Index_C (F, Idx, K_String, K_Len);
   end Get_String_Attribute_At_Index;

   function MD_String_In_Context
     (C     : LLVM.Types.Context_T;
      Str   : String;
      S_Len : unsigned)
      return LLVM.Types.Value_T
   is
      Str_Array  : aliased char_array := To_C (Str);
      Str_String : constant chars_ptr := To_Chars_Ptr (Str_Array'Unchecked_Access);
   begin
      return MD_String_In_Context_C (C, Str_String, S_Len);
   end MD_String_In_Context;

   function MD_String
     (Str   : String;
      S_Len : unsigned)
      return LLVM.Types.Value_T
   is
      Str_Array  : aliased char_array := To_C (Str);
      Str_String : constant chars_ptr := To_Chars_Ptr (Str_Array'Unchecked_Access);
   begin
      return MD_String_C (Str_String, S_Len);
   end MD_String;

   function Get_MD_String
     (V      : LLVM.Types.Value_T;
      Length : unsigned)
      return String
   is
   begin
      return Value (Get_MD_String_C (V, Length));
   end Get_MD_String;

   function Value_Is_Basic_Block
     (Val : LLVM.Types.Value_T)
      return Boolean
   is
   begin
      return Value_Is_Basic_Block_C (Val) /= 0;
   end Value_Is_Basic_Block;

   function Get_Basic_Block_Name
     (BB : LLVM.Types.Basic_Block_T)
      return String
   is
   begin
      return Value (Get_Basic_Block_Name_C (BB));
   end Get_Basic_Block_Name;

   function Append_Basic_Block_In_Context
     (C    : LLVM.Types.Context_T;
      Fn   : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Basic_Block_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Append_Basic_Block_In_Context_C (C, Fn, Name_String);
   end Append_Basic_Block_In_Context;

   function Append_Basic_Block
     (Fn   : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Basic_Block_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Append_Basic_Block_C (Fn, Name_String);
   end Append_Basic_Block;

   function Insert_Basic_Block_In_Context
     (C    : LLVM.Types.Context_T;
      BB   : LLVM.Types.Basic_Block_T;
      Name : String)
      return LLVM.Types.Basic_Block_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Insert_Basic_Block_In_Context_C (C, BB, Name_String);
   end Insert_Basic_Block_In_Context;

   function Insert_Basic_Block
     (Insert_Before_BB : LLVM.Types.Basic_Block_T;
      Name             : String)
      return LLVM.Types.Basic_Block_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Insert_Basic_Block_C (Insert_Before_BB, Name_String);
   end Insert_Basic_Block;

   function Get_Call_Site_String_Attribute
     (C     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : String;
      K_Len : unsigned)
      return LLVM.Types.Attribute_T
   is
      K_Array  : aliased char_array := To_C (K);
      K_String : constant chars_ptr := To_Chars_Ptr (K_Array'Unchecked_Access);
   begin
      return Get_Call_Site_String_Attribute_C (C, Idx, K_String, K_Len);
   end Get_Call_Site_String_Attribute;

   function Is_Tail_Call
     (Call_Inst : LLVM.Types.Value_T)
      return Boolean
   is
   begin
      return Is_Tail_Call_C (Call_Inst) /= 0;
   end Is_Tail_Call;

   function Is_Conditional
     (Branch : LLVM.Types.Value_T)
      return Boolean
   is
   begin
      return Is_Conditional_C (Branch) /= 0;
   end Is_Conditional;

   function Is_In_Bounds
     (GEP : LLVM.Types.Value_T)
      return Boolean
   is
   begin
      return Is_In_Bounds_C (GEP) /= 0;
   end Is_In_Bounds;

   function Invoke
     (arg1     : LLVM.Types.Builder_T;
      Fn       : LLVM.Types.Value_T;
      Args     : System.Address;
      Num_Args : unsigned;
      C_Then   : LLVM.Types.Basic_Block_T;
      Catch    : LLVM.Types.Basic_Block_T;
      Name     : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Invoke_C (arg1, Fn, Args, Num_Args, C_Then, Catch, Name_String);
   end Invoke;

   function Landing_Pad
     (B           : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pers_Fn     : LLVM.Types.Value_T;
      Num_Clauses : unsigned;
      Name        : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Landing_Pad_C (B, Ty, Pers_Fn, Num_Clauses, Name_String);
   end Landing_Pad;

   function Is_Cleanup
     (Landing_Pad : LLVM.Types.Value_T)
      return Boolean
   is
   begin
      return Is_Cleanup_C (Landing_Pad) /= 0;
   end Is_Cleanup;

   function Add
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Add_C (arg1, LHS, RHS, Name_String);
   end Add;

   function NSW_Add
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_NSW_Add_C (arg1, LHS, RHS, Name_String);
   end NSW_Add;

   function NUW_Add
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_NUW_Add_C (arg1, LHS, RHS, Name_String);
   end NUW_Add;

   function F_Add
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_F_Add_C (arg1, LHS, RHS, Name_String);
   end F_Add;

   function Sub
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Sub_C (arg1, LHS, RHS, Name_String);
   end Sub;

   function NSW_Sub
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_NSW_Sub_C (arg1, LHS, RHS, Name_String);
   end NSW_Sub;

   function NUW_Sub
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_NUW_Sub_C (arg1, LHS, RHS, Name_String);
   end NUW_Sub;

   function F_Sub
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_F_Sub_C (arg1, LHS, RHS, Name_String);
   end F_Sub;

   function Mul
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Mul_C (arg1, LHS, RHS, Name_String);
   end Mul;

   function NSW_Mul
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_NSW_Mul_C (arg1, LHS, RHS, Name_String);
   end NSW_Mul;

   function NUW_Mul
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_NUW_Mul_C (arg1, LHS, RHS, Name_String);
   end NUW_Mul;

   function F_Mul
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_F_Mul_C (arg1, LHS, RHS, Name_String);
   end F_Mul;

   function U_Div
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_U_Div_C (arg1, LHS, RHS, Name_String);
   end U_Div;

   function Exact_U_Div
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Exact_U_Div_C (arg1, LHS, RHS, Name_String);
   end Exact_U_Div;

   function S_Div
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_S_Div_C (arg1, LHS, RHS, Name_String);
   end S_Div;

   function Exact_S_Div
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Exact_S_Div_C (arg1, LHS, RHS, Name_String);
   end Exact_S_Div;

   function F_Div
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_F_Div_C (arg1, LHS, RHS, Name_String);
   end F_Div;

   function U_Rem
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_U_Rem_C (arg1, LHS, RHS, Name_String);
   end U_Rem;

   function S_Rem
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_S_Rem_C (arg1, LHS, RHS, Name_String);
   end S_Rem;

   function F_Rem
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_F_Rem_C (arg1, LHS, RHS, Name_String);
   end F_Rem;

   function Shl
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Shl_C (arg1, LHS, RHS, Name_String);
   end Shl;

   function L_Shr
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_L_Shr_C (arg1, LHS, RHS, Name_String);
   end L_Shr;

   function A_Shr
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_A_Shr_C (arg1, LHS, RHS, Name_String);
   end A_Shr;

   function Build_And
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_And_C (arg1, LHS, RHS, Name_String);
   end Build_And;

   function Build_Or
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Or_C (arg1, LHS, RHS, Name_String);
   end Build_Or;

   function Build_Xor
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Xor_C (arg1, LHS, RHS, Name_String);
   end Build_Xor;

   function Bin_Op
     (B    : LLVM.Types.Builder_T;
      Op   : Opcode_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Bin_Op_C (B, Op, LHS, RHS, Name_String);
   end Bin_Op;

   function Neg
     (arg1 : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Neg_C (arg1, V, Name_String);
   end Neg;

   function NSW_Neg
     (B    : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_NSW_Neg_C (B, V, Name_String);
   end NSW_Neg;

   function NUW_Neg
     (B    : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_NUW_Neg_C (B, V, Name_String);
   end NUW_Neg;

   function F_Neg
     (arg1 : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_F_Neg_C (arg1, V, Name_String);
   end F_Neg;

   function Build_Not
     (arg1 : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Not_C (arg1, V, Name_String);
   end Build_Not;

   function Malloc
     (arg1 : LLVM.Types.Builder_T;
      Ty   : LLVM.Types.Type_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Malloc_C (arg1, Ty, Name_String);
   end Malloc;

   function Array_Malloc
     (arg1 : LLVM.Types.Builder_T;
      Ty   : LLVM.Types.Type_T;
      Val  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Array_Malloc_C (arg1, Ty, Val, Name_String);
   end Array_Malloc;

   function Alloca
     (arg1 : LLVM.Types.Builder_T;
      Ty   : LLVM.Types.Type_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Alloca_C (arg1, Ty, Name_String);
   end Alloca;

   function Array_Alloca
     (arg1 : LLVM.Types.Builder_T;
      Ty   : LLVM.Types.Type_T;
      Val  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Array_Alloca_C (arg1, Ty, Val, Name_String);
   end Array_Alloca;

   function Load
     (arg1        : LLVM.Types.Builder_T;
      Pointer_Val : LLVM.Types.Value_T;
      Name        : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Load_C (arg1, Pointer_Val, Name_String);
   end Load;

   function GEP
     (B           : LLVM.Types.Builder_T;
      Pointer     : LLVM.Types.Value_T;
      Indices     : System.Address;
      Num_Indices : unsigned;
      Name        : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_GEP_C (B, Pointer, Indices, Num_Indices, Name_String);
   end GEP;

   function In_Bounds_GEP
     (B           : LLVM.Types.Builder_T;
      Pointer     : LLVM.Types.Value_T;
      Indices     : System.Address;
      Num_Indices : unsigned;
      Name        : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_In_Bounds_GEP_C (B, Pointer, Indices, Num_Indices, Name_String);
   end In_Bounds_GEP;

   function Struct_GEP
     (B       : LLVM.Types.Builder_T;
      Pointer : LLVM.Types.Value_T;
      Idx     : unsigned;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Struct_GEP_C (B, Pointer, Idx, Name_String);
   end Struct_GEP;

   function Global_String
     (B    : LLVM.Types.Builder_T;
      Str  : String;
      Name : String)
      return LLVM.Types.Value_T
   is
      Str_Array   : aliased char_array := To_C (Str);
      Str_String  : constant chars_ptr := To_Chars_Ptr (Str_Array'Unchecked_Access);
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Global_String_C (B, Str_String, Name_String);
   end Global_String;

   function Global_String_Ptr
     (B    : LLVM.Types.Builder_T;
      Str  : String;
      Name : String)
      return LLVM.Types.Value_T
   is
      Str_Array   : aliased char_array := To_C (Str);
      Str_String  : constant chars_ptr := To_Chars_Ptr (Str_Array'Unchecked_Access);
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Global_String_Ptr_C (B, Str_String, Name_String);
   end Global_String_Ptr;

   function Get_Volatile
     (Memory_Access_Inst : LLVM.Types.Value_T)
      return Boolean
   is
   begin
      return Get_Volatile_C (Memory_Access_Inst) /= 0;
   end Get_Volatile;

   function Trunc
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Trunc_C (arg1, Val, Dest_Ty, Name_String);
   end Trunc;

   function Z_Ext
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Z_Ext_C (arg1, Val, Dest_Ty, Name_String);
   end Z_Ext;

   function S_Ext
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_S_Ext_C (arg1, Val, Dest_Ty, Name_String);
   end S_Ext;

   function FP_To_UI
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_FP_To_UI_C (arg1, Val, Dest_Ty, Name_String);
   end FP_To_UI;

   function FP_To_SI
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_FP_To_SI_C (arg1, Val, Dest_Ty, Name_String);
   end FP_To_SI;

   function UI_To_FP
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_UI_To_FP_C (arg1, Val, Dest_Ty, Name_String);
   end UI_To_FP;

   function SI_To_FP
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_SI_To_FP_C (arg1, Val, Dest_Ty, Name_String);
   end SI_To_FP;

   function FP_Trunc
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_FP_Trunc_C (arg1, Val, Dest_Ty, Name_String);
   end FP_Trunc;

   function FP_Ext
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_FP_Ext_C (arg1, Val, Dest_Ty, Name_String);
   end FP_Ext;

   function Ptr_To_Int
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Ptr_To_Int_C (arg1, Val, Dest_Ty, Name_String);
   end Ptr_To_Int;

   function Int_To_Ptr
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Int_To_Ptr_C (arg1, Val, Dest_Ty, Name_String);
   end Int_To_Ptr;

   function Bit_Cast
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Bit_Cast_C (arg1, Val, Dest_Ty, Name_String);
   end Bit_Cast;

   function Addr_Space_Cast
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Addr_Space_Cast_C (arg1, Val, Dest_Ty, Name_String);
   end Addr_Space_Cast;

   function Z_Ext_Or_Bit_Cast
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Z_Ext_Or_Bit_Cast_C (arg1, Val, Dest_Ty, Name_String);
   end Z_Ext_Or_Bit_Cast;

   function S_Ext_Or_Bit_Cast
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_S_Ext_Or_Bit_Cast_C (arg1, Val, Dest_Ty, Name_String);
   end S_Ext_Or_Bit_Cast;

   function Trunc_Or_Bit_Cast
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Trunc_Or_Bit_Cast_C (arg1, Val, Dest_Ty, Name_String);
   end Trunc_Or_Bit_Cast;

   function Cast
     (B       : LLVM.Types.Builder_T;
      Op      : Opcode_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Cast_C (B, Op, Val, Dest_Ty, Name_String);
   end Cast;

   function Pointer_Cast
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Pointer_Cast_C (arg1, Val, Dest_Ty, Name_String);
   end Pointer_Cast;

   function Int_Cast
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Int_Cast_C (arg1, Val, Dest_Ty, Name_String);
   end Int_Cast;

   function FP_Cast
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_FP_Cast_C (arg1, Val, Dest_Ty, Name_String);
   end FP_Cast;

   function I_Cmp
     (arg1 : LLVM.Types.Builder_T;
      Op   : Int_Predicate_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_I_Cmp_C (arg1, Op, LHS, RHS, Name_String);
   end I_Cmp;

   function F_Cmp
     (arg1 : LLVM.Types.Builder_T;
      Op   : Real_Predicate_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_F_Cmp_C (arg1, Op, LHS, RHS, Name_String);
   end F_Cmp;

   function Phi
     (arg1 : LLVM.Types.Builder_T;
      Ty   : LLVM.Types.Type_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Phi_C (arg1, Ty, Name_String);
   end Phi;

   function Call
     (arg1     : LLVM.Types.Builder_T;
      Fn       : LLVM.Types.Value_T;
      Args     : System.Address;
      Num_Args : unsigned;
      Name     : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Call_C (arg1, Fn, Args, Num_Args, Name_String);
   end Call;

   function Build_Select
     (arg1   : LLVM.Types.Builder_T;
      C_If   : LLVM.Types.Value_T;
      C_Then : LLVM.Types.Value_T;
      C_Else : LLVM.Types.Value_T;
      Name   : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Select_C (arg1, C_If, C_Then, C_Else, Name_String);
   end Build_Select;

   function VA_Arg
     (arg1 : LLVM.Types.Builder_T;
      List : LLVM.Types.Value_T;
      Ty   : LLVM.Types.Type_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_VA_Arg_C (arg1, List, Ty, Name_String);
   end VA_Arg;

   function Extract_Element
     (arg1    : LLVM.Types.Builder_T;
      Vec_Val : LLVM.Types.Value_T;
      Index   : LLVM.Types.Value_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Extract_Element_C (arg1, Vec_Val, Index, Name_String);
   end Extract_Element;

   function Insert_Element
     (arg1    : LLVM.Types.Builder_T;
      Vec_Val : LLVM.Types.Value_T;
      Elt_Val : LLVM.Types.Value_T;
      Index   : LLVM.Types.Value_T;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Insert_Element_C (arg1, Vec_Val, Elt_Val, Index, Name_String);
   end Insert_Element;

   function Shuffle_Vector
     (arg1 : LLVM.Types.Builder_T;
      V1   : LLVM.Types.Value_T;
      V2   : LLVM.Types.Value_T;
      Mask : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Shuffle_Vector_C (arg1, V1, V2, Mask, Name_String);
   end Shuffle_Vector;

   function Extract_Value
     (arg1    : LLVM.Types.Builder_T;
      Agg_Val : LLVM.Types.Value_T;
      Index   : unsigned;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Extract_Value_C (arg1, Agg_Val, Index, Name_String);
   end Extract_Value;

   function Insert_Value
     (arg1    : LLVM.Types.Builder_T;
      Agg_Val : LLVM.Types.Value_T;
      Elt_Val : LLVM.Types.Value_T;
      Index   : unsigned;
      Name    : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Insert_Value_C (arg1, Agg_Val, Elt_Val, Index, Name_String);
   end Insert_Value;

   function Is_Null
     (arg1 : LLVM.Types.Builder_T;
      Val  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Is_Null_C (arg1, Val, Name_String);
   end Is_Null;

   function Is_Not_Null
     (arg1 : LLVM.Types.Builder_T;
      Val  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Is_Not_Null_C (arg1, Val, Name_String);
   end Is_Not_Null;

   function Ptr_Diff
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Ptr_Diff_C (arg1, LHS, RHS, Name_String);
   end Ptr_Diff;

   function Fence
     (B             : LLVM.Types.Builder_T;
      ordering      : Atomic_Ordering_T;
      single_Thread : Boolean;
      Name          : String)
      return LLVM.Types.Value_T
   is
      single_Thread_Bool : constant Bool_T := Boolean'Pos (single_Thread);
      Name_Array         : aliased char_array := To_C (Name);
      Name_String        : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Build_Fence_C (B, ordering, single_Thread_Bool, Name_String);
   end Fence;

   function Atomic_RMW
     (B             : LLVM.Types.Builder_T;
      op            : Atomic_RMW_Bin_Op_T;
      PTR           : LLVM.Types.Value_T;
      Val           : LLVM.Types.Value_T;
      ordering      : Atomic_Ordering_T;
      single_Thread : Boolean)
      return LLVM.Types.Value_T
   is
      single_Thread_Bool : constant Bool_T := Boolean'Pos (single_Thread);
   begin
      return Build_Atomic_RMW_C (B, op, PTR, Val, ordering, single_Thread_Bool);
   end Atomic_RMW;

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
      Single_Thread_Bool : constant Bool_T := Boolean'Pos (Single_Thread);
   begin
      return Build_Atomic_Cmp_Xchg_C (B, Ptr, Cmp, C_New, Success_Ordering, Failure_Ordering, Single_Thread_Bool);
   end Atomic_Cmp_Xchg;

   function Is_Atomic_Single_Thread
     (Atomic_Inst : LLVM.Types.Value_T)
      return Boolean
   is
   begin
      return Is_Atomic_Single_Thread_C (Atomic_Inst) /= 0;
   end Is_Atomic_Single_Thread;

   function Create_Memory_Buffer_With_Contents_Of_File
     (Path        : String;
      Out_Mem_Buf : System.Address;
      Out_Message : System.Address)
      return Boolean
   is
      Path_Array  : aliased char_array := To_C (Path);
      Path_String : constant chars_ptr := To_Chars_Ptr (Path_Array'Unchecked_Access);
   begin
      return Create_Memory_Buffer_With_Contents_Of_File_C (Path_String, Out_Mem_Buf, Out_Message) /= 0;
   end Create_Memory_Buffer_With_Contents_Of_File;

   function Create_Memory_Buffer_With_STDIN
     (Out_Mem_Buf : System.Address;
      Out_Message : System.Address)
      return Boolean
   is
   begin
      return Create_Memory_Buffer_With_STDIN_C (Out_Mem_Buf, Out_Message) /= 0;
   end Create_Memory_Buffer_With_STDIN;

   function Create_Memory_Buffer_With_Memory_Range
     (Input_Data               : String;
      Input_Data_Length        : stddef_h.size_t;
      Buffer_Name              : String;
      Requires_Null_Terminator : Boolean)
      return LLVM.Types.Memory_Buffer_T
   is
      Input_Data_Array              : aliased char_array := To_C (Input_Data);
      Input_Data_String             : constant chars_ptr := To_Chars_Ptr (Input_Data_Array'Unchecked_Access);
      Buffer_Name_Array             : aliased char_array := To_C (Buffer_Name);
      Buffer_Name_String            : constant chars_ptr := To_Chars_Ptr (Buffer_Name_Array'Unchecked_Access);
      Requires_Null_Terminator_Bool : constant Bool_T := Boolean'Pos (Requires_Null_Terminator);
   begin
      return Create_Memory_Buffer_With_Memory_Range_C (Input_Data_String, Input_Data_Length, Buffer_Name_String, Requires_Null_Terminator_Bool);
   end Create_Memory_Buffer_With_Memory_Range;

   function Create_Memory_Buffer_With_Memory_Range_Copy
     (Input_Data        : String;
      Input_Data_Length : stddef_h.size_t;
      Buffer_Name       : String)
      return LLVM.Types.Memory_Buffer_T
   is
      Input_Data_Array   : aliased char_array := To_C (Input_Data);
      Input_Data_String  : constant chars_ptr := To_Chars_Ptr (Input_Data_Array'Unchecked_Access);
      Buffer_Name_Array  : aliased char_array := To_C (Buffer_Name);
      Buffer_Name_String : constant chars_ptr := To_Chars_Ptr (Buffer_Name_Array'Unchecked_Access);
   begin
      return Create_Memory_Buffer_With_Memory_Range_Copy_C (Input_Data_String, Input_Data_Length, Buffer_Name_String);
   end Create_Memory_Buffer_With_Memory_Range_Copy;

   function Get_Buffer_Start
     (Mem_Buf : LLVM.Types.Memory_Buffer_T)
      return String
   is
   begin
      return Value (Get_Buffer_Start_C (Mem_Buf));
   end Get_Buffer_Start;

   function Run_Pass_Manager
     (PM : LLVM.Types.Pass_Manager_T;
      M  : LLVM.Types.Module_T)
      return Boolean
   is
   begin
      return Run_Pass_Manager_C (PM, M) /= 0;
   end Run_Pass_Manager;

   function Initialize_Function_Pass_Manager
     (FPM : LLVM.Types.Pass_Manager_T)
      return Boolean
   is
   begin
      return Initialize_Function_Pass_Manager_C (FPM) /= 0;
   end Initialize_Function_Pass_Manager;

   function Run_Function_Pass_Manager
     (FPM : LLVM.Types.Pass_Manager_T;
      F   : LLVM.Types.Value_T)
      return Boolean
   is
   begin
      return Run_Function_Pass_Manager_C (FPM, F) /= 0;
   end Run_Function_Pass_Manager;

   function Finalize_Function_Pass_Manager
     (FPM : LLVM.Types.Pass_Manager_T)
      return Boolean
   is
   begin
      return Finalize_Function_Pass_Manager_C (FPM) /= 0;
   end Finalize_Function_Pass_Manager;

   function Start_Multithreaded
      return Boolean
   is
   begin
      return Start_Multithreaded_C /= 0;
   end Start_Multithreaded;

   function Is_Multithreaded
      return Boolean
   is
   begin
      return Is_Multithreaded_C /= 0;
   end Is_Multithreaded;

   procedure Dispose_Message
     (Message : String)
   is
      Message_Array  : aliased char_array := To_C (Message);
      Message_String : constant chars_ptr := To_Chars_Ptr (Message_Array'Unchecked_Access);
   begin
      Dispose_Message_C (Message_String);
   end Dispose_Message;

   procedure Set_Module_Identifier
     (M     : LLVM.Types.Module_T;
      Ident : String;
      Len   : stddef_h.size_t)
   is
      Ident_Array  : aliased char_array := To_C (Ident);
      Ident_String : constant chars_ptr := To_Chars_Ptr (Ident_Array'Unchecked_Access);
   begin
      Set_Module_Identifier_C (M, Ident_String, Len);
   end Set_Module_Identifier;

   procedure Set_Data_Layout
     (M               : LLVM.Types.Module_T;
      Data_Layout_Str : String)
   is
      Data_Layout_Str_Array  : aliased char_array := To_C (Data_Layout_Str);
      Data_Layout_Str_String : constant chars_ptr := To_Chars_Ptr (Data_Layout_Str_Array'Unchecked_Access);
   begin
      Set_Data_Layout_C (M, Data_Layout_Str_String);
   end Set_Data_Layout;

   procedure Set_Target
     (M      : LLVM.Types.Module_T;
      Triple : String)
   is
      Triple_Array  : aliased char_array := To_C (Triple);
      Triple_String : constant chars_ptr := To_Chars_Ptr (Triple_Array'Unchecked_Access);
   begin
      Set_Target_C (M, Triple_String);
   end Set_Target;

   procedure Set_Module_Inline_Asm
     (M   : LLVM.Types.Module_T;
      Asm : String)
   is
      Asm_Array  : aliased char_array := To_C (Asm);
      Asm_String : constant chars_ptr := To_Chars_Ptr (Asm_Array'Unchecked_Access);
   begin
      Set_Module_Inline_Asm_C (M, Asm_String);
   end Set_Module_Inline_Asm;

   procedure Get_Named_Metadata_Operands
     (M    : LLVM.Types.Module_T;
      Name : String;
      Dest : System.Address)
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Get_Named_Metadata_Operands_C (M, Name_String, Dest);
   end Get_Named_Metadata_Operands;

   procedure Add_Named_Metadata_Operand
     (M    : LLVM.Types.Module_T;
      Name : String;
      Val  : LLVM.Types.Value_T)
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Add_Named_Metadata_Operand_C (M, Name_String, Val);
   end Add_Named_Metadata_Operand;

   procedure Struct_Set_Body
     (Struct_Ty     : LLVM.Types.Type_T;
      Element_Types : System.Address;
      Element_Count : unsigned;
      Packed        : Boolean)
   is
      Packed_Bool : constant Bool_T := Boolean'Pos (Packed);
   begin
      Struct_Set_Body_C (Struct_Ty, Element_Types, Element_Count, Packed_Bool);
   end Struct_Set_Body;

   procedure Set_Value_Name
     (Val  : LLVM.Types.Value_T;
      Name : String)
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Set_Value_Name_C (Val, Name_String);
   end Set_Value_Name;

   procedure Set_Section
     (Global  : LLVM.Types.Value_T;
      Section : String)
   is
      Section_Array  : aliased char_array := To_C (Section);
      Section_String : constant chars_ptr := To_Chars_Ptr (Section_Array'Unchecked_Access);
   begin
      Set_Section_C (Global, Section_String);
   end Set_Section;

   procedure Set_Unnamed_Addr
     (Global           : LLVM.Types.Value_T;
      Has_Unnamed_Addr : Boolean)
   is
      Has_Unnamed_Addr_Bool : constant Bool_T := Boolean'Pos (Has_Unnamed_Addr);
   begin
      Set_Unnamed_Addr_C (Global, Has_Unnamed_Addr_Bool);
   end Set_Unnamed_Addr;

   procedure Set_Thread_Local
     (Global_Var      : LLVM.Types.Value_T;
      Is_Thread_Local : Boolean)
   is
      Is_Thread_Local_Bool : constant Bool_T := Boolean'Pos (Is_Thread_Local);
   begin
      Set_Thread_Local_C (Global_Var, Is_Thread_Local_Bool);
   end Set_Thread_Local;

   procedure Set_Global_Constant
     (Global_Var  : LLVM.Types.Value_T;
      Is_Constant : Boolean)
   is
      Is_Constant_Bool : constant Bool_T := Boolean'Pos (Is_Constant);
   begin
      Set_Global_Constant_C (Global_Var, Is_Constant_Bool);
   end Set_Global_Constant;

   procedure Set_Externally_Initialized
     (Global_Var  : LLVM.Types.Value_T;
      Is_Ext_Init : Boolean)
   is
      Is_Ext_Init_Bool : constant Bool_T := Boolean'Pos (Is_Ext_Init);
   begin
      Set_Externally_Initialized_C (Global_Var, Is_Ext_Init_Bool);
   end Set_Externally_Initialized;

   procedure Set_GC
     (Fn   : LLVM.Types.Value_T;
      Name : String)
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Set_GC_C (Fn, Name_String);
   end Set_GC;

   procedure Remove_String_Attribute_At_Index
     (F     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : String;
      K_Len : unsigned)
   is
      K_Array  : aliased char_array := To_C (K);
      K_String : constant chars_ptr := To_Chars_Ptr (K_Array'Unchecked_Access);
   begin
      Remove_String_Attribute_At_Index_C (F, Idx, K_String, K_Len);
   end Remove_String_Attribute_At_Index;

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
      Add_Target_Dependent_Function_Attr_C (Fn, A_String, V_String);
   end Add_Target_Dependent_Function_Attr;

   procedure Remove_Call_Site_String_Attribute
     (C     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : String;
      K_Len : unsigned)
   is
      K_Array  : aliased char_array := To_C (K);
      K_String : constant chars_ptr := To_Chars_Ptr (K_Array'Unchecked_Access);
   begin
      Remove_Call_Site_String_Attribute_C (C, Idx, K_String, K_Len);
   end Remove_Call_Site_String_Attribute;

   procedure Set_Tail_Call
     (Call_Inst    : LLVM.Types.Value_T;
      Is_Tail_Call : Boolean)
   is
      Is_Tail_Call_Bool : constant Bool_T := Boolean'Pos (Is_Tail_Call);
   begin
      Set_Tail_Call_C (Call_Inst, Is_Tail_Call_Bool);
   end Set_Tail_Call;

   procedure Set_Is_In_Bounds
     (GEP       : LLVM.Types.Value_T;
      In_Bounds : Boolean)
   is
      In_Bounds_Bool : constant Bool_T := Boolean'Pos (In_Bounds);
   begin
      Set_Is_In_Bounds_C (GEP, In_Bounds_Bool);
   end Set_Is_In_Bounds;

   procedure Insert_Into_With_Name
     (Builder : LLVM.Types.Builder_T;
      Instr   : LLVM.Types.Value_T;
      Name    : String)
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Insert_Into_Builder_With_Name_C (Builder, Instr, Name_String);
   end Insert_Into_With_Name;

   procedure Set_Cleanup
     (Landing_Pad : LLVM.Types.Value_T;
      Val         : Boolean)
   is
      Val_Bool : constant Bool_T := Boolean'Pos (Val);
   begin
      Set_Cleanup_C (Landing_Pad, Val_Bool);
   end Set_Cleanup;

   procedure Set_Volatile
     (Memory_Access_Inst : LLVM.Types.Value_T;
      Is_Volatile        : Boolean)
   is
      Is_Volatile_Bool : constant Bool_T := Boolean'Pos (Is_Volatile);
   begin
      Set_Volatile_C (Memory_Access_Inst, Is_Volatile_Bool);
   end Set_Volatile;

   procedure Set_Atomic_Single_Thread
     (Atomic_Inst   : LLVM.Types.Value_T;
      Single_Thread : Boolean)
   is
      Single_Thread_Bool : constant Bool_T := Boolean'Pos (Single_Thread);
   begin
      Set_Atomic_Single_Thread_C (Atomic_Inst, Single_Thread_Bool);
   end Set_Atomic_Single_Thread;

end LLVM.Core;
