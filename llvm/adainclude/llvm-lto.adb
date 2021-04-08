pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Lto is

   function Get_Version
      return String
   is
   begin
      return Value (Get_Version_C);
   end Get_Version;

   function Get_Error_Message
      return String
   is
   begin
      return Value (Get_Error_Message_C);
   end Get_Error_Message;

   function Module_Is_Object_File
     (Path : String)
      return Bool_T_T
   is
      Path_Array  : aliased char_array := To_C (Path);
      Path_String : constant chars_ptr := To_Chars_Ptr (Path_Array'Unchecked_Access);
   begin
      return Module_Is_Object_File_C (Path_String);
   end Module_Is_Object_File;

   function Module_Is_Object_File_For_Target
     (Path                 : String;
      Target_Triple_Prefix : String)
      return Bool_T_T
   is
      Path_Array                  : aliased char_array := To_C (Path);
      Path_String                 : constant chars_ptr := To_Chars_Ptr (Path_Array'Unchecked_Access);
      Target_Triple_Prefix_Array  : aliased char_array := To_C (Target_Triple_Prefix);
      Target_Triple_Prefix_String : constant chars_ptr := To_Chars_Ptr (Target_Triple_Prefix_Array'Unchecked_Access);
   begin
      return Module_Is_Object_File_For_Target_C (Path_String, Target_Triple_Prefix_String);
   end Module_Is_Object_File_For_Target;

   function Module_Is_Object_File_In_Memory_For_Target
     (Mem                  : System.Address;
      Length               : stddef_h.size_t;
      Target_Triple_Prefix : String)
      return Bool_T_T
   is
      Target_Triple_Prefix_Array  : aliased char_array := To_C (Target_Triple_Prefix);
      Target_Triple_Prefix_String : constant chars_ptr := To_Chars_Ptr (Target_Triple_Prefix_Array'Unchecked_Access);
   begin
      return Module_Is_Object_File_In_Memory_For_Target_C (Mem, Length, Target_Triple_Prefix_String);
   end Module_Is_Object_File_In_Memory_For_Target;

   function Module_Create
     (Path : String)
      return Module_T_T
   is
      Path_Array  : aliased char_array := To_C (Path);
      Path_String : constant chars_ptr := To_Chars_Ptr (Path_Array'Unchecked_Access);
   begin
      return Module_Create_C (Path_String);
   end Module_Create;

   function Module_Create_From_Memory_With_Path
     (Mem    : System.Address;
      Length : stddef_h.size_t;
      Path   : String)
      return Module_T_T
   is
      Path_Array  : aliased char_array := To_C (Path);
      Path_String : constant chars_ptr := To_Chars_Ptr (Path_Array'Unchecked_Access);
   begin
      return Module_Create_From_Memory_With_Path_C (Mem, Length, Path_String);
   end Module_Create_From_Memory_With_Path;

   function Module_Create_In_Local_Context
     (Mem    : System.Address;
      Length : stddef_h.size_t;
      Path   : String)
      return Module_T_T
   is
      Path_Array  : aliased char_array := To_C (Path);
      Path_String : constant chars_ptr := To_Chars_Ptr (Path_Array'Unchecked_Access);
   begin
      return Module_Create_In_Local_Context_C (Mem, Length, Path_String);
   end Module_Create_In_Local_Context;

   function Module_Create_In_Codegen_Context
     (Mem    : System.Address;
      Length : stddef_h.size_t;
      Path   : String;
      Cg     : Code_Gen_T_T)
      return Module_T_T
   is
      Path_Array  : aliased char_array := To_C (Path);
      Path_String : constant chars_ptr := To_Chars_Ptr (Path_Array'Unchecked_Access);
   begin
      return Module_Create_In_Codegen_Context_C (Mem, Length, Path_String, Cg);
   end Module_Create_In_Codegen_Context;

   function Module_Create_From_Fd
     (Fd        : int;
      Path      : String;
      File_Size : stddef_h.size_t)
      return Module_T_T
   is
      Path_Array  : aliased char_array := To_C (Path);
      Path_String : constant chars_ptr := To_Chars_Ptr (Path_Array'Unchecked_Access);
   begin
      return Module_Create_From_Fd_C (Fd, Path_String, File_Size);
   end Module_Create_From_Fd;

   function Module_Create_From_Fd_At_Offset
     (Fd        : int;
      Path      : String;
      File_Size : stddef_h.size_t;
      Map_Size  : stddef_h.size_t;
      Offset    : stddef_h.off_t)
      return Module_T_T
   is
      Path_Array  : aliased char_array := To_C (Path);
      Path_String : constant chars_ptr := To_Chars_Ptr (Path_Array'Unchecked_Access);
   begin
      return Module_Create_From_Fd_At_Offset_C (Fd, Path_String, File_Size, Map_Size, Offset);
   end Module_Create_From_Fd_At_Offset;

   function Module_Get_Target_Triple
     (C_Mod : Module_T_T)
      return String
   is
   begin
      return Value (Module_Get_Target_Triple_C (C_Mod));
   end Module_Get_Target_Triple;

   procedure Module_Set_Target_Triple
     (C_Mod  : Module_T_T;
      Triple : String)
   is
      Triple_Array  : aliased char_array := To_C (Triple);
      Triple_String : constant chars_ptr := To_Chars_Ptr (Triple_Array'Unchecked_Access);
   begin
      Module_Set_Target_Triple_C (C_Mod, Triple_String);
   end Module_Set_Target_Triple;

   function Module_Get_Symbol_Name
     (C_Mod : Module_T_T;
      Index : unsigned)
      return String
   is
   begin
      return Value (Module_Get_Symbol_Name_C (C_Mod, Index));
   end Module_Get_Symbol_Name;

   function Module_Get_Linkeropts
     (C_Mod : Module_T_T)
      return String
   is
   begin
      return Value (Module_Get_Linkeropts_C (C_Mod));
   end Module_Get_Linkeropts;

   procedure Codegen_Set_Cpu
     (Cg  : Code_Gen_T_T;
      Cpu : String)
   is
      Cpu_Array  : aliased char_array := To_C (Cpu);
      Cpu_String : constant chars_ptr := To_Chars_Ptr (Cpu_Array'Unchecked_Access);
   begin
      Codegen_Set_Cpu_C (Cg, Cpu_String);
   end Codegen_Set_Cpu;

   procedure Codegen_Set_Assembler_Path
     (Cg   : Code_Gen_T_T;
      Path : String)
   is
      Path_Array  : aliased char_array := To_C (Path);
      Path_String : constant chars_ptr := To_Chars_Ptr (Path_Array'Unchecked_Access);
   begin
      Codegen_Set_Assembler_Path_C (Cg, Path_String);
   end Codegen_Set_Assembler_Path;

   procedure Codegen_Add_Must_Preserve_Symbol
     (Cg     : Code_Gen_T_T;
      Symbol : String)
   is
      Symbol_Array  : aliased char_array := To_C (Symbol);
      Symbol_String : constant chars_ptr := To_Chars_Ptr (Symbol_Array'Unchecked_Access);
   begin
      Codegen_Add_Must_Preserve_Symbol_C (Cg, Symbol_String);
   end Codegen_Add_Must_Preserve_Symbol;

   function Codegen_Write_Merged_Modules
     (Cg   : Code_Gen_T_T;
      Path : String)
      return Bool_T_T
   is
      Path_Array  : aliased char_array := To_C (Path);
      Path_String : constant chars_ptr := To_Chars_Ptr (Path_Array'Unchecked_Access);
   begin
      return Codegen_Write_Merged_Modules_C (Cg, Path_String);
   end Codegen_Write_Merged_Modules;

   procedure Codegen_Debug_Options
     (Cg    : Code_Gen_T_T;
      Arg_2 : String)
   is
      Arg_2_Array  : aliased char_array := To_C (Arg_2);
      Arg_2_String : constant chars_ptr := To_Chars_Ptr (Arg_2_Array'Unchecked_Access);
   begin
      Codegen_Debug_Options_C (Cg, Arg_2_String);
   end Codegen_Debug_Options;

   function Input_Create
     (Buffer      : System.Address;
      Buffer_Size : stddef_h.size_t;
      Path        : String)
      return Input_T_T
   is
      Path_Array  : aliased char_array := To_C (Path);
      Path_String : constant chars_ptr := To_Chars_Ptr (Path_Array'Unchecked_Access);
   begin
      return Input_Create_C (Buffer, Buffer_Size, Path_String);
   end Input_Create;

   function Input_Get_Dependent_Library
     (Input : Input_T_T;
      Index : stddef_h.size_t;
      Size  : access stddef_h.size_t)
      return String
   is
   begin
      return Value (Input_Get_Dependent_Library_C (Input, Index, Size));
   end Input_Get_Dependent_Library;

   procedure thinlto_codegen_add_module
     (Cg         : thinlto_code_gen_t;
      Identifier : String;
      Data       : String;
      Length     : int)
   is
      Identifier_Array  : aliased char_array := To_C (Identifier);
      Identifier_String : constant chars_ptr := To_Chars_Ptr (Identifier_Array'Unchecked_Access);
      Data_Array        : aliased char_array := To_C (Data);
      Data_String       : constant chars_ptr := To_Chars_Ptr (Data_Array'Unchecked_Access);
   begin
      thinlto_codegen_add_module_C (Cg, Identifier_String, Data_String, Length);
   end thinlto_codegen_add_module;

   function thinlto_module_get_object_file
     (Cg    : thinlto_code_gen_t;
      Index : unsigned)
      return String
   is
   begin
      return Value (thinlto_module_get_object_file_C (Cg, Index));
   end thinlto_module_get_object_file;

   procedure thinlto_codegen_set_savetemps_dir
     (Cg             : thinlto_code_gen_t;
      Save_Temps_Dir : String)
   is
      Save_Temps_Dir_Array  : aliased char_array := To_C (Save_Temps_Dir);
      Save_Temps_Dir_String : constant chars_ptr := To_Chars_Ptr (Save_Temps_Dir_Array'Unchecked_Access);
   begin
      thinlto_codegen_set_savetemps_dir_C (Cg, Save_Temps_Dir_String);
   end thinlto_codegen_set_savetemps_dir;

   procedure thinlto_set_generated_objects_dir
     (Cg             : thinlto_code_gen_t;
      Save_Temps_Dir : String)
   is
      Save_Temps_Dir_Array  : aliased char_array := To_C (Save_Temps_Dir);
      Save_Temps_Dir_String : constant chars_ptr := To_Chars_Ptr (Save_Temps_Dir_Array'Unchecked_Access);
   begin
      thinlto_set_generated_objects_dir_C (Cg, Save_Temps_Dir_String);
   end thinlto_set_generated_objects_dir;

   procedure thinlto_codegen_set_cpu
     (Cg  : thinlto_code_gen_t;
      Cpu : String)
   is
      Cpu_Array  : aliased char_array := To_C (Cpu);
      Cpu_String : constant chars_ptr := To_Chars_Ptr (Cpu_Array'Unchecked_Access);
   begin
      thinlto_codegen_set_cpu_C (Cg, Cpu_String);
   end thinlto_codegen_set_cpu;

   procedure thinlto_codegen_add_must_preserve_symbol
     (Cg     : thinlto_code_gen_t;
      Name   : String;
      Length : int)
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      thinlto_codegen_add_must_preserve_symbol_C (Cg, Name_String, Length);
   end thinlto_codegen_add_must_preserve_symbol;

   procedure thinlto_codegen_add_cross_referenced_symbol
     (Cg     : thinlto_code_gen_t;
      Name   : String;
      Length : int)
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      thinlto_codegen_add_cross_referenced_symbol_C (Cg, Name_String, Length);
   end thinlto_codegen_add_cross_referenced_symbol;

   procedure thinlto_codegen_set_cache_dir
     (Cg        : thinlto_code_gen_t;
      Cache_Dir : String)
   is
      Cache_Dir_Array  : aliased char_array := To_C (Cache_Dir);
      Cache_Dir_String : constant chars_ptr := To_Chars_Ptr (Cache_Dir_Array'Unchecked_Access);
   begin
      thinlto_codegen_set_cache_dir_C (Cg, Cache_Dir_String);
   end thinlto_codegen_set_cache_dir;

end LLVM.Lto;
