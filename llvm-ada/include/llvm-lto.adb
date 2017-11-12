pragma Ada_2005;
pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.lto is

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
     (path : String)
      return Bool_T_T
   is
      path_Array  : aliased char_array := To_C (path);
      path_String : constant chars_ptr := To_Chars_Ptr (path_Array'Unchecked_Access);
   begin
      return Module_Is_Object_File_C (path_String);
   end Module_Is_Object_File;

   function Module_Is_Object_File_For_Target
     (path                 : String;
      Target_Triple_Prefix : String)
      return Bool_T_T
   is
      path_Array                  : aliased char_array := To_C (path);
      path_String                 : constant chars_ptr := To_Chars_Ptr (path_Array'Unchecked_Access);
      Target_Triple_Prefix_Array  : aliased char_array := To_C (Target_Triple_Prefix);
      Target_Triple_Prefix_String : constant chars_ptr := To_Chars_Ptr (Target_Triple_Prefix_Array'Unchecked_Access);
   begin
      return Module_Is_Object_File_For_Target_C (path_String, Target_Triple_Prefix_String);
   end Module_Is_Object_File_For_Target;

   function Module_Is_Object_File_In_Memory_For_Target
     (mem                  : System.Address;
      length               : stddef_h.size_t;
      Target_Triple_Prefix : String)
      return Bool_T_T
   is
      Target_Triple_Prefix_Array  : aliased char_array := To_C (Target_Triple_Prefix);
      Target_Triple_Prefix_String : constant chars_ptr := To_Chars_Ptr (Target_Triple_Prefix_Array'Unchecked_Access);
   begin
      return Module_Is_Object_File_In_Memory_For_Target_C (mem, length, Target_Triple_Prefix_String);
   end Module_Is_Object_File_In_Memory_For_Target;

   function Module_Create
     (path : String)
      return Module_T_T
   is
      path_Array  : aliased char_array := To_C (path);
      path_String : constant chars_ptr := To_Chars_Ptr (path_Array'Unchecked_Access);
   begin
      return Module_Create_C (path_String);
   end Module_Create;

   function Module_Create_From_Memory_With_Path
     (mem    : System.Address;
      length : stddef_h.size_t;
      path   : String)
      return Module_T_T
   is
      path_Array  : aliased char_array := To_C (path);
      path_String : constant chars_ptr := To_Chars_Ptr (path_Array'Unchecked_Access);
   begin
      return Module_Create_From_Memory_With_Path_C (mem, length, path_String);
   end Module_Create_From_Memory_With_Path;

   function Module_Create_In_Local_Context
     (mem    : System.Address;
      length : stddef_h.size_t;
      path   : String)
      return Module_T_T
   is
      path_Array  : aliased char_array := To_C (path);
      path_String : constant chars_ptr := To_Chars_Ptr (path_Array'Unchecked_Access);
   begin
      return Module_Create_In_Local_Context_C (mem, length, path_String);
   end Module_Create_In_Local_Context;

   function Module_Create_In_Codegen_Context
     (mem    : System.Address;
      length : stddef_h.size_t;
      path   : String;
      cg     : Code_Gen_T_T)
      return Module_T_T
   is
      path_Array  : aliased char_array := To_C (path);
      path_String : constant chars_ptr := To_Chars_Ptr (path_Array'Unchecked_Access);
   begin
      return Module_Create_In_Codegen_Context_C (mem, length, path_String, cg);
   end Module_Create_In_Codegen_Context;

   function Module_Create_From_Fd
     (fd        : int;
      path      : String;
      File_Size : stddef_h.size_t)
      return Module_T_T
   is
      path_Array  : aliased char_array := To_C (path);
      path_String : constant chars_ptr := To_Chars_Ptr (path_Array'Unchecked_Access);
   begin
      return Module_Create_From_Fd_C (fd, path_String, File_Size);
   end Module_Create_From_Fd;

   function Module_Create_From_Fd_At_Offset
     (fd        : int;
      path      : String;
      File_Size : stddef_h.size_t;
      Map_Size  : stddef_h.size_t;
      offset    : stddef_h.off_t)
      return Module_T_T
   is
      path_Array  : aliased char_array := To_C (path);
      path_String : constant chars_ptr := To_Chars_Ptr (path_Array'Unchecked_Access);
   begin
      return Module_Create_From_Fd_At_Offset_C (fd, path_String, File_Size, Map_Size, offset);
   end Module_Create_From_Fd_At_Offset;

   function Module_Get_Target_Triple
     (C_Mod : Module_T_T)
      return String
   is
   begin
      return Value (Module_Get_Target_Triple_C (C_Mod));
   end Module_Get_Target_Triple;

   function Module_Get_Symbol_Name
     (C_Mod : Module_T_T;
      index : unsigned)
      return String
   is
   begin
      return Value (Module_Get_Symbol_Name_C (C_Mod, index));
   end Module_Get_Symbol_Name;

   function Module_Get_Linkeropts
     (C_Mod : Module_T_T)
      return String
   is
   begin
      return Value (Module_Get_Linkeropts_C (C_Mod));
   end Module_Get_Linkeropts;

   function Codegen_Write_Merged_Modules
     (cg   : Code_Gen_T_T;
      path : String)
      return Bool_T_T
   is
      path_Array  : aliased char_array := To_C (path);
      path_String : constant chars_ptr := To_Chars_Ptr (path_Array'Unchecked_Access);
   begin
      return Codegen_Write_Merged_Modules_C (cg, path_String);
   end Codegen_Write_Merged_Modules;

   function thinlto_module_get_object_file
     (cg    : thinlto_code_gen_t;
      index : unsigned)
      return String
   is
   begin
      return Value (thinlto_module_get_object_file_C (cg, index));
   end thinlto_module_get_object_file;

   procedure Module_Set_Target_Triple
     (C_Mod  : Module_T_T;
      triple : String)
   is
      triple_Array  : aliased char_array := To_C (triple);
      triple_String : constant chars_ptr := To_Chars_Ptr (triple_Array'Unchecked_Access);
   begin
      Module_Set_Target_Triple_C (C_Mod, triple_String);
   end Module_Set_Target_Triple;

   procedure Codegen_Set_Cpu
     (cg  : Code_Gen_T_T;
      cpu : String)
   is
      cpu_Array  : aliased char_array := To_C (cpu);
      cpu_String : constant chars_ptr := To_Chars_Ptr (cpu_Array'Unchecked_Access);
   begin
      Codegen_Set_Cpu_C (cg, cpu_String);
   end Codegen_Set_Cpu;

   procedure Codegen_Set_Assembler_Path
     (cg   : Code_Gen_T_T;
      path : String)
   is
      path_Array  : aliased char_array := To_C (path);
      path_String : constant chars_ptr := To_Chars_Ptr (path_Array'Unchecked_Access);
   begin
      Codegen_Set_Assembler_Path_C (cg, path_String);
   end Codegen_Set_Assembler_Path;

   procedure Codegen_Add_Must_Preserve_Symbol
     (cg     : Code_Gen_T_T;
      symbol : String)
   is
      symbol_Array  : aliased char_array := To_C (symbol);
      symbol_String : constant chars_ptr := To_Chars_Ptr (symbol_Array'Unchecked_Access);
   begin
      Codegen_Add_Must_Preserve_Symbol_C (cg, symbol_String);
   end Codegen_Add_Must_Preserve_Symbol;

   procedure Codegen_Debug_Options
     (cg   : Code_Gen_T_T;
      arg2 : String)
   is
      arg2_Array  : aliased char_array := To_C (arg2);
      arg2_String : constant chars_ptr := To_Chars_Ptr (arg2_Array'Unchecked_Access);
   begin
      Codegen_Debug_Options_C (cg, arg2_String);
   end Codegen_Debug_Options;

   procedure thinlto_codegen_add_module
     (cg         : thinlto_code_gen_t;
      identifier : String;
      data       : String;
      length     : int)
   is
      identifier_Array  : aliased char_array := To_C (identifier);
      identifier_String : constant chars_ptr := To_Chars_Ptr (identifier_Array'Unchecked_Access);
      data_Array        : aliased char_array := To_C (data);
      data_String       : constant chars_ptr := To_Chars_Ptr (data_Array'Unchecked_Access);
   begin
      thinlto_codegen_add_module_C (cg, identifier_String, data_String, length);
   end thinlto_codegen_add_module;

   procedure thinlto_codegen_set_savetemps_dir
     (cg             : thinlto_code_gen_t;
      Save_Temps_Dir : String)
   is
      Save_Temps_Dir_Array  : aliased char_array := To_C (Save_Temps_Dir);
      Save_Temps_Dir_String : constant chars_ptr := To_Chars_Ptr (Save_Temps_Dir_Array'Unchecked_Access);
   begin
      thinlto_codegen_set_savetemps_dir_C (cg, Save_Temps_Dir_String);
   end thinlto_codegen_set_savetemps_dir;

   procedure thinlto_set_generated_objects_dir
     (cg             : thinlto_code_gen_t;
      Save_Temps_Dir : String)
   is
      Save_Temps_Dir_Array  : aliased char_array := To_C (Save_Temps_Dir);
      Save_Temps_Dir_String : constant chars_ptr := To_Chars_Ptr (Save_Temps_Dir_Array'Unchecked_Access);
   begin
      thinlto_set_generated_objects_dir_C (cg, Save_Temps_Dir_String);
   end thinlto_set_generated_objects_dir;

   procedure thinlto_codegen_set_cpu
     (cg  : thinlto_code_gen_t;
      cpu : String)
   is
      cpu_Array  : aliased char_array := To_C (cpu);
      cpu_String : constant chars_ptr := To_Chars_Ptr (cpu_Array'Unchecked_Access);
   begin
      thinlto_codegen_set_cpu_C (cg, cpu_String);
   end thinlto_codegen_set_cpu;

   procedure thinlto_codegen_add_must_preserve_symbol
     (cg     : thinlto_code_gen_t;
      name   : String;
      length : int)
   is
      name_Array  : aliased char_array := To_C (name);
      name_String : constant chars_ptr := To_Chars_Ptr (name_Array'Unchecked_Access);
   begin
      thinlto_codegen_add_must_preserve_symbol_C (cg, name_String, length);
   end thinlto_codegen_add_must_preserve_symbol;

   procedure thinlto_codegen_add_cross_referenced_symbol
     (cg     : thinlto_code_gen_t;
      name   : String;
      length : int)
   is
      name_Array  : aliased char_array := To_C (name);
      name_String : constant chars_ptr := To_Chars_Ptr (name_Array'Unchecked_Access);
   begin
      thinlto_codegen_add_cross_referenced_symbol_C (cg, name_String, length);
   end thinlto_codegen_add_cross_referenced_symbol;

   procedure thinlto_codegen_set_cache_dir
     (cg        : thinlto_code_gen_t;
      Cache_Dir : String)
   is
      Cache_Dir_Array  : aliased char_array := To_C (Cache_Dir);
      Cache_Dir_String : constant chars_ptr := To_Chars_Ptr (Cache_Dir_Array'Unchecked_Access);
   begin
      thinlto_codegen_set_cache_dir_C (cg, Cache_Dir_String);
   end thinlto_codegen_set_cache_dir;

end LLVM.lto;
