pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Target_Machine is

   function Get_Target_From_Name
     (Name : String)
      return Target_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Get_Target_From_Name_C (Name_String);
   end Get_Target_From_Name;

   function Get_Target_From_Triple
     (Triple        : String;
      T             : System.Address;
      Error_Message : System.Address)
      return Boolean
   is
      Triple_Array  : aliased char_array := To_C (Triple);
      Triple_String : constant chars_ptr := To_Chars_Ptr (Triple_Array'Unchecked_Access);
   begin
      return Get_Target_From_Triple_C (Triple_String, T, Error_Message) /= 0;
   end Get_Target_From_Triple;

   function Get_Target_Name
     (T : Target_T)
      return String
   is
   begin
      return Value (Get_Target_Name_C (T));
   end Get_Target_Name;

   function Get_Target_Description
     (T : Target_T)
      return String
   is
   begin
      return Value (Get_Target_Description_C (T));
   end Get_Target_Description;

   function Target_Has_JIT
     (T : Target_T)
      return Boolean
   is
   begin
      return Target_Has_JIT_C (T) /= 0;
   end Target_Has_JIT;

   function Target_Has_Target_Machine
     (T : Target_T)
      return Boolean
   is
   begin
      return Target_Has_Target_Machine_C (T) /= 0;
   end Target_Has_Target_Machine;

   function Target_Has_Asm_Backend
     (T : Target_T)
      return Boolean
   is
   begin
      return Target_Has_Asm_Backend_C (T) /= 0;
   end Target_Has_Asm_Backend;

   function Create_Target_Machine
     (T          : Target_T;
      Triple     : String;
      CPU        : String;
      Features   : String;
      Level      : Code_Gen_Opt_Level_T;
      Reloc      : Reloc_Mode_T;
      Code_Model : Code_Model_T)
      return Target_Machine_T
   is
      Triple_Array    : aliased char_array := To_C (Triple);
      Triple_String   : constant chars_ptr := To_Chars_Ptr (Triple_Array'Unchecked_Access);
      CPU_Array       : aliased char_array := To_C (CPU);
      CPU_String      : constant chars_ptr := To_Chars_Ptr (CPU_Array'Unchecked_Access);
      Features_Array  : aliased char_array := To_C (Features);
      Features_String : constant chars_ptr := To_Chars_Ptr (Features_Array'Unchecked_Access);
   begin
      return Create_Target_Machine_C (T, Triple_String, CPU_String, Features_String, Level, Reloc, Code_Model);
   end Create_Target_Machine;

   function Get_Target_Machine_Triple
     (T : Target_Machine_T)
      return String
   is
   begin
      return Value (Get_Target_Machine_Triple_C (T));
   end Get_Target_Machine_Triple;

   function Get_Target_Machine_CPU
     (T : Target_Machine_T)
      return String
   is
   begin
      return Value (Get_Target_Machine_CPU_C (T));
   end Get_Target_Machine_CPU;

   function Get_Target_Machine_Feature_String
     (T : Target_Machine_T)
      return String
   is
   begin
      return Value (Get_Target_Machine_Feature_String_C (T));
   end Get_Target_Machine_Feature_String;

   function Target_Machine_Emit_To_File
     (T             : Target_Machine_T;
      M             : LLVM.Types.Module_T;
      Filename      : String;
      codegen       : Code_Gen_File_Type_T;
      Error_Message : System.Address)
      return Boolean
   is
      Filename_Array  : aliased char_array := To_C (Filename);
      Filename_String : constant chars_ptr := To_Chars_Ptr (Filename_Array'Unchecked_Access);
   begin
      return Target_Machine_Emit_To_File_C (T, M, Filename_String, codegen, Error_Message) /= 0;
   end Target_Machine_Emit_To_File;

   function Target_Machine_Emit_To_Memory_Buffer
     (T             : Target_Machine_T;
      M             : LLVM.Types.Module_T;
      codegen       : Code_Gen_File_Type_T;
      Error_Message : System.Address;
      Out_Mem_Buf   : System.Address)
      return Boolean
   is
   begin
      return Target_Machine_Emit_To_Memory_Buffer_C (T, M, codegen, Error_Message, Out_Mem_Buf) /= 0;
   end Target_Machine_Emit_To_Memory_Buffer;

   function Get_Default_Target_Triple
      return String
   is
   begin
      return Value (Get_Default_Target_Triple_C);
   end Get_Default_Target_Triple;

   function Normalize_Target_Triple
     (triple : String)
      return String
   is
      triple_Array  : aliased char_array := To_C (triple);
      triple_String : constant chars_ptr := To_Chars_Ptr (triple_Array'Unchecked_Access);
   begin
      return Value (Normalize_Target_Triple_C (triple_String));
   end Normalize_Target_Triple;

   function Get_Host_CPU_Name
      return String
   is
   begin
      return Value (Get_Host_CPU_Name_C);
   end Get_Host_CPU_Name;

   function Get_Host_CPU_Features
      return String
   is
   begin
      return Value (Get_Host_CPU_Features_C);
   end Get_Host_CPU_Features;

   procedure Set_Target_Machine_Asm_Verbosity
     (T           : Target_Machine_T;
      Verbose_Asm : Boolean)
   is
      Verbose_Asm_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Verbose_Asm);
   begin
      Set_Target_Machine_Asm_Verbosity_C (T, Verbose_Asm_Bool);
   end Set_Target_Machine_Asm_Verbosity;

end LLVM.Target_Machine;
