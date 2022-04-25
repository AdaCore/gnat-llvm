pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Execution_Engine is

   function Create_Generic_Value_Of_Int
     (Ty        : LLVM.Types.Type_T;
      N         : Extensions.unsigned_long_long;
      Is_Signed : LLVM.Types.Bool_T)
      return Generic_Value_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMCreateGenericValueOfInt";
   function Create_Generic_Value_Of_Int
     (Ty        : LLVM.Types.Type_T;
      N         : Extensions.unsigned_long_long;
      Is_Signed : Boolean)
      return Generic_Value_T
   is
      Return_Value   : Generic_Value_T;
      Is_Signed_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Signed);
   begin
      Return_Value := Create_Generic_Value_Of_Int (Ty, N, Is_Signed_Bool);
      return Return_Value;
   end Create_Generic_Value_Of_Int;

   function Generic_Value_To_Int
     (Gen_Val   : Generic_Value_T;
      Is_Signed : LLVM.Types.Bool_T)
      return Extensions.unsigned_long_long
   with Import => True,
        Convention => C,
        External_Name => "LLVMGenericValueToInt";
   function Generic_Value_To_Int
     (Gen_Val   : Generic_Value_T;
      Is_Signed : Boolean)
      return Extensions.unsigned_long_long
   is
      Return_Value   : Extensions.unsigned_long_long;
      Is_Signed_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Signed);
   begin
      Return_Value := Generic_Value_To_Int (Gen_Val, Is_Signed_Bool);
      return Return_Value;
   end Generic_Value_To_Int;

   function Create_Execution_Engine_For_Module
     (Out_EE    : System.Address;
      M         : LLVM.Types.Module_T;
      Out_Error : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMCreateExecutionEngineForModule";
   function Create_Execution_Engine_For_Module
     (Out_EE    : System.Address;
      M         : LLVM.Types.Module_T;
      Out_Error : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Create_Execution_Engine_For_Module (Out_EE, M, Out_Error);
      return Return_Value /= 0;
   end Create_Execution_Engine_For_Module;

   function Create_Interpreter_For_Module
     (Out_Interp : System.Address;
      M          : LLVM.Types.Module_T;
      Out_Error  : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMCreateInterpreterForModule";
   function Create_Interpreter_For_Module
     (Out_Interp : System.Address;
      M          : LLVM.Types.Module_T;
      Out_Error  : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Create_Interpreter_For_Module (Out_Interp, M, Out_Error);
      return Return_Value /= 0;
   end Create_Interpreter_For_Module;

   function Create_JIT_Compiler_For_Module
     (Out_JIT   : System.Address;
      M         : LLVM.Types.Module_T;
      Opt_Level : unsigned;
      Out_Error : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMCreateJITCompilerForModule";
   function Create_JIT_Compiler_For_Module
     (Out_JIT   : System.Address;
      M         : LLVM.Types.Module_T;
      Opt_Level : unsigned;
      Out_Error : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Create_JIT_Compiler_For_Module (Out_JIT, M, Opt_Level, Out_Error);
      return Return_Value /= 0;
   end Create_JIT_Compiler_For_Module;

   function Create_MCJIT_Compiler_For_Module
     (Out_JIT         : System.Address;
      M               : LLVM.Types.Module_T;
      Options         : access MCJIT_Compiler_Options_T;
      Size_Of_Options : stddef_h.size_t;
      Out_Error       : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMCreateMCJITCompilerForModule";
   function Create_MCJIT_Compiler_For_Module
     (Out_JIT         : System.Address;
      M               : LLVM.Types.Module_T;
      Options         : access MCJIT_Compiler_Options_T;
      Size_Of_Options : stddef_h.size_t;
      Out_Error       : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Create_MCJIT_Compiler_For_Module (Out_JIT, M, Options, Size_Of_Options, Out_Error);
      return Return_Value /= 0;
   end Create_MCJIT_Compiler_For_Module;

   function Remove_Module
     (EE        : Execution_Engine_T;
      M         : LLVM.Types.Module_T;
      Out_Mod   : System.Address;
      Out_Error : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMRemoveModule";
   function Remove_Module
     (EE        : Execution_Engine_T;
      M         : LLVM.Types.Module_T;
      Out_Mod   : System.Address;
      Out_Error : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Remove_Module (EE, M, Out_Mod, Out_Error);
      return Return_Value /= 0;
   end Remove_Module;

   function Find_Function
     (EE     : Execution_Engine_T;
      Name   : Interfaces.C.Strings.chars_ptr;
      Out_Fn : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMFindFunction";
   function Find_Function
     (EE     : Execution_Engine_T;
      Name   : String;
      Out_Fn : System.Address)
      return LLVM.Types.Bool_T
   is
      Return_Value : LLVM.Types.Bool_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Find_Function (EE, Name_String, Out_Fn);
      return Return_Value;
   end Find_Function;

   function Find_Function
     (EE     : Execution_Engine_T;
      Name   : String;
      Out_Fn : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Find_Function (EE, Name, Out_Fn);
      return Return_Value /= 0;
   end Find_Function;

   function Get_Global_Value_Address
     (EE   : Execution_Engine_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return stdint_h.uint64_t
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetGlobalValueAddress";
   function Get_Global_Value_Address
     (EE   : Execution_Engine_T;
      Name : String)
      return stdint_h.uint64_t
   is
      Return_Value : stdint_h.uint64_t;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Get_Global_Value_Address (EE, Name_String);
      return Return_Value;
   end Get_Global_Value_Address;

   function Get_Function_Address
     (EE   : Execution_Engine_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return stdint_h.uint64_t
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetFunctionAddress";
   function Get_Function_Address
     (EE   : Execution_Engine_T;
      Name : String)
      return stdint_h.uint64_t
   is
      Return_Value : stdint_h.uint64_t;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Get_Function_Address (EE, Name_String);
      return Return_Value;
   end Get_Function_Address;

   function Execution_Engine_Get_Err_Msg
     (EE        : Execution_Engine_T;
      Out_Error : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMExecutionEngineGetErrMsg";
   function Execution_Engine_Get_Err_Msg
     (EE        : Execution_Engine_T;
      Out_Error : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Execution_Engine_Get_Err_Msg (EE, Out_Error);
      return Return_Value /= 0;
   end Execution_Engine_Get_Err_Msg;

end LLVM.Execution_Engine;
