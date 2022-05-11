pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Orc is

   function Orc_Execution_Session_Intern
     (ES   : Orc_Execution_Session_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return Orc_Symbol_String_Pool_Entry_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcExecutionSessionIntern";
   function Orc_Execution_Session_Intern
     (ES   : Orc_Execution_Session_T;
      Name : String)
      return Orc_Symbol_String_Pool_Entry_T
   is
      Return_Value : Orc_Symbol_String_Pool_Entry_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Orc_Execution_Session_Intern (ES, Name_String);
      return Return_Value;
   end Orc_Execution_Session_Intern;

   function Orc_Symbol_String_Pool_Entry_Str
     (S : Orc_Symbol_String_Pool_Entry_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcSymbolStringPoolEntryStr";
   function Orc_Symbol_String_Pool_Entry_Str
     (S : Orc_Symbol_String_Pool_Entry_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Orc_Symbol_String_Pool_Entry_Str (S);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Orc_Symbol_String_Pool_Entry_Str;

   function Orc_Create_Custom_Materialization_Unit
     (Name        : Interfaces.C.Strings.chars_ptr;
      Ctx         : System.Address;
      Syms        : Orc_C_Symbol_Flags_Map_Pairs_T;
      Num_Syms    : stddef_h.size_t;
      Init_Sym    : Orc_Symbol_String_Pool_Entry_T;
      Materialize : Orc_Materialization_Unit_Materialize_Function_T;
      Discard     : Orc_Materialization_Unit_Discard_Function_T;
      Destroy     : Orc_Materialization_Unit_Destroy_Function_T)
      return Orc_Materialization_Unit_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcCreateCustomMaterializationUnit";
   function Orc_Create_Custom_Materialization_Unit
     (Name        : String;
      Ctx         : System.Address;
      Syms        : Orc_C_Symbol_Flags_Map_Pairs_T;
      Num_Syms    : stddef_h.size_t;
      Init_Sym    : Orc_Symbol_String_Pool_Entry_T;
      Materialize : Orc_Materialization_Unit_Materialize_Function_T;
      Discard     : Orc_Materialization_Unit_Discard_Function_T;
      Destroy     : Orc_Materialization_Unit_Destroy_Function_T)
      return Orc_Materialization_Unit_T
   is
      Return_Value : Orc_Materialization_Unit_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Orc_Create_Custom_Materialization_Unit (Name_String, Ctx, Syms, Num_Syms, Init_Sym, Materialize, Discard, Destroy);
      return Return_Value;
   end Orc_Create_Custom_Materialization_Unit;

   function Orc_Execution_Session_Create_Bare_JIT_Dylib
     (ES   : Orc_Execution_Session_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return Orc_JIT_Dylib_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcExecutionSessionCreateBareJITDylib";
   function Orc_Execution_Session_Create_Bare_JIT_Dylib
     (ES   : Orc_Execution_Session_T;
      Name : String)
      return Orc_JIT_Dylib_T
   is
      Return_Value : Orc_JIT_Dylib_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Orc_Execution_Session_Create_Bare_JIT_Dylib (ES, Name_String);
      return Return_Value;
   end Orc_Execution_Session_Create_Bare_JIT_Dylib;

   function Orc_Execution_Session_Create_JIT_Dylib
     (ES     : Orc_Execution_Session_T;
      Result : System.Address;
      Name   : Interfaces.C.Strings.chars_ptr)
      return LLVM.Error.Error_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcExecutionSessionCreateJITDylib";
   function Orc_Execution_Session_Create_JIT_Dylib
     (ES     : Orc_Execution_Session_T;
      Result : System.Address;
      Name   : String)
      return LLVM.Error.Error_T
   is
      Return_Value : LLVM.Error.Error_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Orc_Execution_Session_Create_JIT_Dylib (ES, Result, Name_String);
      return Return_Value;
   end Orc_Execution_Session_Create_JIT_Dylib;

   function Orc_Execution_Session_Get_JIT_Dylib_By_Name
     (ES   : Orc_Execution_Session_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return Orc_JIT_Dylib_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcExecutionSessionGetJITDylibByName";
   function Orc_Execution_Session_Get_JIT_Dylib_By_Name
     (ES   : Orc_Execution_Session_T;
      Name : String)
      return Orc_JIT_Dylib_T
   is
      Return_Value : Orc_JIT_Dylib_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Orc_Execution_Session_Get_JIT_Dylib_By_Name (ES, Name_String);
      return Return_Value;
   end Orc_Execution_Session_Get_JIT_Dylib_By_Name;

   function Orc_Create_Dynamic_Library_Search_Generator_For_Path
     (Result        : System.Address;
      File_Name     : Interfaces.C.Strings.chars_ptr;
      Global_Prefix : char;
      Filter        : Orc_Symbol_Predicate_T;
      Filter_Ctx    : System.Address)
      return LLVM.Error.Error_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcCreateDynamicLibrarySearchGeneratorForPath";
   function Orc_Create_Dynamic_Library_Search_Generator_For_Path
     (Result        : System.Address;
      File_Name     : String;
      Global_Prefix : char;
      Filter        : Orc_Symbol_Predicate_T;
      Filter_Ctx    : System.Address)
      return LLVM.Error.Error_T
   is
      Return_Value     : LLVM.Error.Error_T;
      File_Name_Array  : aliased char_array := To_C (File_Name);
      File_Name_String : constant chars_ptr := To_Chars_Ptr (File_Name_Array'Unchecked_Access);
   begin
      Return_Value := Orc_Create_Dynamic_Library_Search_Generator_For_Path (Result, File_Name_String, Global_Prefix, Filter, Filter_Ctx);
      return Return_Value;
   end Orc_Create_Dynamic_Library_Search_Generator_For_Path;

   function Orc_Create_Static_Library_Search_Generator_For_Path
     (Result        : System.Address;
      Obj_Layer     : Orc_Object_Layer_T;
      File_Name     : Interfaces.C.Strings.chars_ptr;
      Target_Triple : Interfaces.C.Strings.chars_ptr)
      return LLVM.Error.Error_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcCreateStaticLibrarySearchGeneratorForPath";
   function Orc_Create_Static_Library_Search_Generator_For_Path
     (Result        : System.Address;
      Obj_Layer     : Orc_Object_Layer_T;
      File_Name     : String;
      Target_Triple : String)
      return LLVM.Error.Error_T
   is
      Return_Value         : LLVM.Error.Error_T;
      File_Name_Array      : aliased char_array := To_C (File_Name);
      File_Name_String     : constant chars_ptr := To_Chars_Ptr (File_Name_Array'Unchecked_Access);
      Target_Triple_Array  : aliased char_array := To_C (Target_Triple);
      Target_Triple_String : constant chars_ptr := To_Chars_Ptr (Target_Triple_Array'Unchecked_Access);
   begin
      Return_Value := Orc_Create_Static_Library_Search_Generator_For_Path (Result, Obj_Layer, File_Name_String, Target_Triple_String);
      return Return_Value;
   end Orc_Create_Static_Library_Search_Generator_For_Path;

   function Orc_JIT_Target_Machine_Builder_Get_Target_Triple
     (JTMB : Orc_JIT_Target_Machine_Builder_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcJITTargetMachineBuilderGetTargetTriple";
   function Orc_JIT_Target_Machine_Get_Target_Triple
     (JTMB : Orc_JIT_Target_Machine_Builder_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Orc_JIT_Target_Machine_Builder_Get_Target_Triple (JTMB);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Orc_JIT_Target_Machine_Get_Target_Triple;

   procedure Orc_JIT_Target_Machine_Builder_Set_Target_Triple
     (JTMB          : Orc_JIT_Target_Machine_Builder_T;
      Target_Triple : Interfaces.C.Strings.chars_ptr)
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcJITTargetMachineBuilderSetTargetTriple";
   procedure Orc_JIT_Target_Machine_Set_Target_Triple
     (JTMB          : Orc_JIT_Target_Machine_Builder_T;
      Target_Triple : String)
   is
      Target_Triple_Array  : aliased char_array := To_C (Target_Triple);
      Target_Triple_String : constant chars_ptr := To_Chars_Ptr (Target_Triple_Array'Unchecked_Access);
   begin
      Orc_JIT_Target_Machine_Builder_Set_Target_Triple (JTMB, Target_Triple_String);
   end Orc_JIT_Target_Machine_Set_Target_Triple;

   function Orc_Create_Local_Indirect_Stubs_Manager
     (Target_Triple : Interfaces.C.Strings.chars_ptr)
      return Orc_Indirect_Stubs_Manager_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcCreateLocalIndirectStubsManager";
   function Orc_Create_Local_Indirect_Stubs_Manager
     (Target_Triple : String)
      return Orc_Indirect_Stubs_Manager_T
   is
      Return_Value         : Orc_Indirect_Stubs_Manager_T;
      Target_Triple_Array  : aliased char_array := To_C (Target_Triple);
      Target_Triple_String : constant chars_ptr := To_Chars_Ptr (Target_Triple_Array'Unchecked_Access);
   begin
      Return_Value := Orc_Create_Local_Indirect_Stubs_Manager (Target_Triple_String);
      return Return_Value;
   end Orc_Create_Local_Indirect_Stubs_Manager;

   function Orc_Create_Local_Lazy_Call_Through_Manager
     (Target_Triple      : Interfaces.C.Strings.chars_ptr;
      ES                 : Orc_Execution_Session_T;
      Error_Handler_Addr : Orc_JIT_Target_Address_T;
      LCTM               : System.Address)
      return LLVM.Error.Error_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcCreateLocalLazyCallThroughManager";
   function Orc_Create_Local_Lazy_Call_Through_Manager
     (Target_Triple      : String;
      ES                 : Orc_Execution_Session_T;
      Error_Handler_Addr : Orc_JIT_Target_Address_T;
      LCTM               : System.Address)
      return LLVM.Error.Error_T
   is
      Return_Value         : LLVM.Error.Error_T;
      Target_Triple_Array  : aliased char_array := To_C (Target_Triple);
      Target_Triple_String : constant chars_ptr := To_Chars_Ptr (Target_Triple_Array'Unchecked_Access);
   begin
      Return_Value := Orc_Create_Local_Lazy_Call_Through_Manager (Target_Triple_String, ES, Error_Handler_Addr, LCTM);
      return Return_Value;
   end Orc_Create_Local_Lazy_Call_Through_Manager;

   function Orc_Create_Dump_Objects
     (Dump_Dir            : Interfaces.C.Strings.chars_ptr;
      Identifier_Override : Interfaces.C.Strings.chars_ptr)
      return Orc_Dump_Objects_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcCreateDumpObjects";
   function Orc_Create_Dump_Objects
     (Dump_Dir            : String;
      Identifier_Override : String)
      return Orc_Dump_Objects_T
   is
      Return_Value               : Orc_Dump_Objects_T;
      Dump_Dir_Array             : aliased char_array := To_C (Dump_Dir);
      Dump_Dir_String            : constant chars_ptr := To_Chars_Ptr (Dump_Dir_Array'Unchecked_Access);
      Identifier_Override_Array  : aliased char_array := To_C (Identifier_Override);
      Identifier_Override_String : constant chars_ptr := To_Chars_Ptr (Identifier_Override_Array'Unchecked_Access);
   begin
      Return_Value := Orc_Create_Dump_Objects (Dump_Dir_String, Identifier_Override_String);
      return Return_Value;
   end Orc_Create_Dump_Objects;

end LLVM.Orc;
