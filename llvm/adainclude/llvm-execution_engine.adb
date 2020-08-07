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
      Is_Signed : Boolean)
      return Generic_Value_T
   is
      Is_Signed_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Signed);
   begin
      return Create_Generic_Value_Of_Int_C (Ty, N, Is_Signed_Bool);
   end Create_Generic_Value_Of_Int;

   function Generic_Value_To_Int
     (Gen_Val   : Generic_Value_T;
      Is_Signed : Boolean)
      return Extensions.unsigned_long_long
   is
      Is_Signed_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Is_Signed);
   begin
      return Generic_Value_To_Int_C (Gen_Val, Is_Signed_Bool);
   end Generic_Value_To_Int;

   function Create_Execution_Engine_For_Module
     (Out_EE    : System.Address;
      M         : LLVM.Types.Module_T;
      Out_Error : System.Address)
      return Boolean
   is
   begin
      return Create_Execution_Engine_For_Module_C (Out_EE, M, Out_Error) /= 0;
   end Create_Execution_Engine_For_Module;

   function Create_Interpreter_For_Module
     (Out_Interp : System.Address;
      M          : LLVM.Types.Module_T;
      Out_Error  : System.Address)
      return Boolean
   is
   begin
      return Create_Interpreter_For_Module_C (Out_Interp, M, Out_Error) /= 0;
   end Create_Interpreter_For_Module;

   function Create_JIT_Compiler_For_Module
     (Out_JIT   : System.Address;
      M         : LLVM.Types.Module_T;
      Opt_Level : unsigned;
      Out_Error : System.Address)
      return Boolean
   is
   begin
      return Create_JIT_Compiler_For_Module_C (Out_JIT, M, Opt_Level, Out_Error) /= 0;
   end Create_JIT_Compiler_For_Module;

   function Create_MCJIT_Compiler_For_Module
     (Out_JIT         : System.Address;
      M               : LLVM.Types.Module_T;
      Options         : access MCJIT_Compiler_Options_T;
      Size_Of_Options : stddef_h.size_t;
      Out_Error       : System.Address)
      return Boolean
   is
   begin
      return Create_MCJIT_Compiler_For_Module_C (Out_JIT, M, Options, Size_Of_Options, Out_Error) /= 0;
   end Create_MCJIT_Compiler_For_Module;

   function Remove_Module
     (EE        : Execution_Engine_T;
      M         : LLVM.Types.Module_T;
      Out_Mod   : System.Address;
      Out_Error : System.Address)
      return Boolean
   is
   begin
      return Remove_Module_C (EE, M, Out_Mod, Out_Error) /= 0;
   end Remove_Module;

   function Find_Function
     (EE     : Execution_Engine_T;
      Name   : String;
      Out_Fn : System.Address)
      return Boolean
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Find_Function_C (EE, Name_String, Out_Fn) /= 0;
   end Find_Function;

   function Get_Global_Value_Address
     (EE   : Execution_Engine_T;
      Name : String)
      return stdint_h.uint64_t
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Get_Global_Value_Address_C (EE, Name_String);
   end Get_Global_Value_Address;

   function Get_Function_Address
     (EE   : Execution_Engine_T;
      Name : String)
      return stdint_h.uint64_t
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Get_Function_Address_C (EE, Name_String);
   end Get_Function_Address;

end LLVM.Execution_Engine;
