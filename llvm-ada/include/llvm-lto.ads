pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with Interfaces.C.Extensions;
with System;
with Interfaces.C.Strings;
with stddef_h;

package LLVM.lto is

   LTO_API_VERSION : constant := 21;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:47

   subtype Bool_T_T is Extensions.bool;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:29

   subtype Symbol_Attributes_T is unsigned;
   SYMBOL_ALIGNMENT_MASK : constant Symbol_Attributes_T := 31;
   SYMBOL_PERMISSIONS_MASK : constant Symbol_Attributes_T := 224;
   SYMBOL_PERMISSIONS_CODE : constant Symbol_Attributes_T := 160;
   SYMBOL_PERMISSIONS_DATA : constant Symbol_Attributes_T := 192;
   SYMBOL_PERMISSIONS_RODATA : constant Symbol_Attributes_T := 128;
   SYMBOL_DEFINITION_MASK : constant Symbol_Attributes_T := 1792;
   SYMBOL_DEFINITION_REGULAR : constant Symbol_Attributes_T := 256;
   SYMBOL_DEFINITION_TENTATIVE : constant Symbol_Attributes_T := 512;
   SYMBOL_DEFINITION_WEAK : constant Symbol_Attributes_T := 768;
   SYMBOL_DEFINITION_UNDEFINED : constant Symbol_Attributes_T := 1024;
   SYMBOL_DEFINITION_WEAKUNDEF : constant Symbol_Attributes_T := 1280;
   SYMBOL_SCOPE_MASK : constant Symbol_Attributes_T := 14336;
   SYMBOL_SCOPE_INTERNAL : constant Symbol_Attributes_T := 2048;
   SYMBOL_SCOPE_HIDDEN : constant Symbol_Attributes_T := 4096;
   SYMBOL_SCOPE_PROTECTED : constant Symbol_Attributes_T := 8192;
   SYMBOL_SCOPE_DEFAULT : constant Symbol_Attributes_T := 6144;
   SYMBOL_SCOPE_DEFAULT_CAN_BE_HIDDEN : constant Symbol_Attributes_T := 10240;
   SYMBOL_COMDAT : constant Symbol_Attributes_T := 16384;
   SYMBOL_ALIAS : constant Symbol_Attributes_T := 32768;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:72

   type Debug_Model_T is 
     (DEBUG_MODEL_NONE,
      DEBUG_MODEL_DWARF);
   pragma Convention (C, Debug_Model_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:80

   type Codegen_Model_T is 
     (CODEGEN_PIC_MODEL_STATIC,
      CODEGEN_PIC_MODEL_DYNAMIC,
      CODEGEN_PIC_MODEL_DYNAMIC_NO_PIC,
      CODEGEN_PIC_MODEL_DEFAULT);
   pragma Convention (C, Codegen_Model_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:90

   --  skipped empty struct LLVMOpaqueLTOModule

   type Module_T_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:93

   --  skipped empty struct LLVMOpaqueLTOCodeGenerator

   type Code_Gen_T_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:96

   --  skipped empty struct LLVMOpaqueThinLTOCodeGenerator

   type thinlto_code_gen_t is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:99

   function Get_Version
      return String;
   function Get_Version_C
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:111
   pragma Import (C, Get_Version_C, "lto_get_version");

   function Get_Error_Message
      return String;
   function Get_Error_Message_C
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:119
   pragma Import (C, Get_Error_Message_C, "lto_get_error_message");

   function Module_Is_Object_File
     (path : String)
      return Bool_T_T;
   function Module_Is_Object_File_C
     (path : Interfaces.C.Strings.chars_ptr)
      return Bool_T_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:127
   pragma Import (C, Module_Is_Object_File_C, "lto_module_is_object_file");

   function Module_Is_Object_File_For_Target
     (path                 : String;
      Target_Triple_Prefix : String)
      return Bool_T_T;
   function Module_Is_Object_File_For_Target_C
     (path                 : Interfaces.C.Strings.chars_ptr;
      Target_Triple_Prefix : Interfaces.C.Strings.chars_ptr)
      return Bool_T_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:135
   pragma Import (C, Module_Is_Object_File_For_Target_C, "lto_module_is_object_file_for_target");

   function Module_Has_Objc_Category (mem : System.Address; length : stddef_h.size_t) return Bool_T_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:145
   pragma Import (C, Module_Has_Objc_Category, "lto_module_has_objc_category");

   function Module_Is_Object_File_In_Memory (mem : System.Address; length : stddef_h.size_t) return Bool_T_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:152
   pragma Import (C, Module_Is_Object_File_In_Memory, "lto_module_is_object_file_in_memory");

function Module_Is_Object_File_In_Memory_For_Target
     (mem                  : System.Address;
      length               : stddef_h.size_t;
      Target_Triple_Prefix : String)
      return Bool_T_T;
   function Module_Is_Object_File_In_Memory_For_Target_C
     (mem                  : System.Address;
      length               : stddef_h.size_t;
      Target_Triple_Prefix : Interfaces.C.Strings.chars_ptr)
      return Bool_T_T;
   pragma Import (C, Module_Is_Object_File_In_Memory_For_Target_C, "lto_module_is_object_file_in_memory_for_target");

   function Module_Create
     (path : String)
      return Module_T_T;
   function Module_Create_C
     (path : Interfaces.C.Strings.chars_ptr)
      return Module_T_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:171
   pragma Import (C, Module_Create_C, "lto_module_create");

   function Module_Create_From_Memory (mem : System.Address; length : stddef_h.size_t) return Module_T_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:180
   pragma Import (C, Module_Create_From_Memory, "lto_module_create_from_memory");

function Module_Create_From_Memory_With_Path
     (mem    : System.Address;
      length : stddef_h.size_t;
      path   : String)
      return Module_T_T;
   function Module_Create_From_Memory_With_Path_C
     (mem    : System.Address;
      length : stddef_h.size_t;
      path   : Interfaces.C.Strings.chars_ptr)
      return Module_T_T;
   pragma Import (C, Module_Create_From_Memory_With_Path_C, "lto_module_create_from_memory_with_path");

function Module_Create_In_Local_Context
     (mem    : System.Address;
      length : stddef_h.size_t;
      path   : String)
      return Module_T_T;
   function Module_Create_In_Local_Context_C
     (mem    : System.Address;
      length : stddef_h.size_t;
      path   : Interfaces.C.Strings.chars_ptr)
      return Module_T_T;
   pragma Import (C, Module_Create_In_Local_Context_C, "lto_module_create_in_local_context");

function Module_Create_In_Codegen_Context
     (mem    : System.Address;
      length : stddef_h.size_t;
      path   : String;
      cg     : Code_Gen_T_T)
      return Module_T_T;
   function Module_Create_In_Codegen_Context_C
     (mem    : System.Address;
      length : stddef_h.size_t;
      path   : Interfaces.C.Strings.chars_ptr;
      cg     : Code_Gen_T_T)
      return Module_T_T;
   pragma Import (C, Module_Create_In_Codegen_Context_C, "lto_module_create_in_codegen_context");

function Module_Create_From_Fd
     (fd        : int;
      path      : String;
      File_Size : stddef_h.size_t)
      return Module_T_T;
   function Module_Create_From_Fd_C
     (fd        : int;
      path      : Interfaces.C.Strings.chars_ptr;
      File_Size : stddef_h.size_t)
      return Module_T_T;
   pragma Import (C, Module_Create_From_Fd_C, "lto_module_create_from_fd");

function Module_Create_From_Fd_At_Offset
     (fd        : int;
      path      : String;
      File_Size : stddef_h.size_t;
      Map_Size  : stddef_h.size_t;
      offset    : stddef_h.off_t)
      return Module_T_T;
   function Module_Create_From_Fd_At_Offset_C
     (fd        : int;
      path      : Interfaces.C.Strings.chars_ptr;
      File_Size : stddef_h.size_t;
      Map_Size  : stddef_h.size_t;
      offset    : stddef_h.off_t)
      return Module_T_T;
   pragma Import (C, Module_Create_From_Fd_At_Offset_C, "lto_module_create_from_fd_at_offset");

   procedure Module_Dispose (C_Mod : Module_T_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:247
   pragma Import (C, Module_Dispose, "lto_module_dispose");

   function Module_Get_Target_Triple
     (C_Mod : Module_T_T)
      return String;
   function Module_Get_Target_Triple_C
     (C_Mod : Module_T_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:255
   pragma Import (C, Module_Get_Target_Triple_C, "lto_module_get_target_triple");

   procedure Module_Set_Target_Triple
     (C_Mod  : Module_T_T;
      triple : String);
   procedure Module_Set_Target_Triple_C
     (C_Mod  : Module_T_T;
      triple : Interfaces.C.Strings.chars_ptr);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:263
   pragma Import (C, Module_Set_Target_Triple_C, "lto_module_set_target_triple");

   function Module_Get_Num_Symbols (C_Mod : Module_T_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:271
   pragma Import (C, Module_Get_Num_Symbols, "lto_module_get_num_symbols");

   function Module_Get_Symbol_Name
     (C_Mod : Module_T_T;
      index : unsigned)
      return String;
   function Module_Get_Symbol_Name_C
     (C_Mod : Module_T_T;
      index : unsigned)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:279
   pragma Import (C, Module_Get_Symbol_Name_C, "lto_module_get_symbol_name");

   function Module_Get_Symbol_Attribute (C_Mod : Module_T_T; index : unsigned) return Symbol_Attributes_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:287
   pragma Import (C, Module_Get_Symbol_Attribute, "lto_module_get_symbol_attribute");

   function Module_Get_Linkeropts
     (C_Mod : Module_T_T)
      return String;
   function Module_Get_Linkeropts_C
     (C_Mod : Module_T_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:298
   pragma Import (C, Module_Get_Linkeropts_C, "lto_module_get_linkeropts");

   subtype Codegen_Diagnostic_Severity_T_T is unsigned;
   DS_ERROR : constant Codegen_Diagnostic_Severity_T_T := 0;
   DS_WARNING : constant Codegen_Diagnostic_Severity_T_T := 1;
   DS_REMARK : constant Codegen_Diagnostic_Severity_T_T := 3;
   DS_NOTE : constant Codegen_Diagnostic_Severity_T_T := 2;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:310

   type Diagnostic_Handler_T_T is access procedure 
        (arg1 : Codegen_Diagnostic_Severity_T_T;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : System.Address);
   pragma Convention (C, Diagnostic_Handler_T_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:321

   procedure Codegen_Set_Diagnostic_Handler
     (arg1 : Code_Gen_T_T;
      arg2 : Diagnostic_Handler_T_T;
      arg3 : System.Address);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:331
   pragma Import (C, Codegen_Set_Diagnostic_Handler, "lto_codegen_set_diagnostic_handler");

   function Codegen_Create return Code_Gen_T_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:345
   pragma Import (C, Codegen_Create, "lto_codegen_create");

   function Codegen_Create_In_Local_Context return Code_Gen_T_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:357
   pragma Import (C, Codegen_Create_In_Local_Context, "lto_codegen_create_in_local_context");

   procedure Codegen_Dispose (arg1 : Code_Gen_T_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:366
   pragma Import (C, Codegen_Dispose, "lto_codegen_dispose");

   function Codegen_Add_Module (cg : Code_Gen_T_T; C_Mod : Module_T_T) return Bool_T_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:379
   pragma Import (C, Codegen_Add_Module, "lto_codegen_add_module");

   procedure Codegen_Set_Module (cg : Code_Gen_T_T; C_Mod : Module_T_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:390
   pragma Import (C, Codegen_Set_Module, "lto_codegen_set_module");

   function Codegen_Set_Debug_Model (cg : Code_Gen_T_T; arg2 : Debug_Model_T) return Bool_T_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:399
   pragma Import (C, Codegen_Set_Debug_Model, "lto_codegen_set_debug_model");

   function Codegen_Set_Pic_Model (cg : Code_Gen_T_T; arg2 : Codegen_Model_T) return Bool_T_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:408
   pragma Import (C, Codegen_Set_Pic_Model, "lto_codegen_set_pic_model");

   procedure Codegen_Set_Cpu
     (cg  : Code_Gen_T_T;
      cpu : String);
   procedure Codegen_Set_Cpu_C
     (cg  : Code_Gen_T_T;
      cpu : Interfaces.C.Strings.chars_ptr);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:416
   pragma Import (C, Codegen_Set_Cpu_C, "lto_codegen_set_cpu");

   procedure Codegen_Set_Assembler_Path
     (cg   : Code_Gen_T_T;
      path : String);
   procedure Codegen_Set_Assembler_Path_C
     (cg   : Code_Gen_T_T;
      path : Interfaces.C.Strings.chars_ptr);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:425
   pragma Import (C, Codegen_Set_Assembler_Path_C, "lto_codegen_set_assembler_path");

   procedure Codegen_Set_Assembler_Args
     (cg : Code_Gen_T_T;
      args : System.Address;
      nargs : int);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:433
   pragma Import (C, Codegen_Set_Assembler_Args, "lto_codegen_set_assembler_args");

   procedure Codegen_Add_Must_Preserve_Symbol
     (cg     : Code_Gen_T_T;
      symbol : String);
   procedure Codegen_Add_Must_Preserve_Symbol_C
     (cg     : Code_Gen_T_T;
      symbol : Interfaces.C.Strings.chars_ptr);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:444
   pragma Import (C, Codegen_Add_Must_Preserve_Symbol_C, "lto_codegen_add_must_preserve_symbol");

   function Codegen_Write_Merged_Modules
     (cg   : Code_Gen_T_T;
      path : String)
      return Bool_T_T;
   function Codegen_Write_Merged_Modules_C
     (cg   : Code_Gen_T_T;
      path : Interfaces.C.Strings.chars_ptr)
      return Bool_T_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:454
   pragma Import (C, Codegen_Write_Merged_Modules_C, "lto_codegen_write_merged_modules");

   function Codegen_Compile (cg : Code_Gen_T_T; length : access stddef_h.size_t) return System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:469
   pragma Import (C, Codegen_Compile, "lto_codegen_compile");

   function Codegen_Compile_To_File (cg : Code_Gen_T_T; name : System.Address) return Bool_T_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:481
   pragma Import (C, Codegen_Compile_To_File, "lto_codegen_compile_to_file");

   function Codegen_Optimize (cg : Code_Gen_T_T) return Bool_T_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:489
   pragma Import (C, Codegen_Optimize, "lto_codegen_optimize");

   function Codegen_Compile_Optimized (cg : Code_Gen_T_T; length : access stddef_h.size_t) return System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:504
   pragma Import (C, Codegen_Compile_Optimized, "lto_codegen_compile_optimized");

   function Api_Version_Fun return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:512
   pragma Import (C, Api_Version_Fun, "lto_api_version_fun");

   procedure Codegen_Debug_Options
     (cg   : Code_Gen_T_T;
      arg2 : String);
   procedure Codegen_Debug_Options_C
     (cg   : Code_Gen_T_T;
      arg2 : Interfaces.C.Strings.chars_ptr);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:520
   pragma Import (C, Codegen_Debug_Options_C, "lto_codegen_debug_options");

   procedure Initialize_Disassembler;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:529
   pragma Import (C, Initialize_Disassembler, "lto_initialize_disassembler");

   procedure Codegen_Set_Should_Internalize (cg : Code_Gen_T_T; Should_Internalize : Bool_T_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:538
   pragma Import (C, Codegen_Set_Should_Internalize, "lto_codegen_set_should_internalize");

   procedure Codegen_Set_Should_Embed_Uselists (cg : Code_Gen_T_T; Should_Embed_Uselists : Bool_T_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:550
   pragma Import (C, Codegen_Set_Should_Embed_Uselists, "lto_codegen_set_should_embed_uselists");

   type bject_Buffer_T is record
      Buffer : Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:567
      Size : aliased stddef_h.size_t;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:568
   end record;
   pragma Convention (C_Pass_By_Copy, bject_Buffer_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:569

   --  skipped anonymous struct anon_18

   function thinlto_create_codegen return thinlto_code_gen_t;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:583
   pragma Import (C, thinlto_create_codegen, "thinlto_create_codegen");

   procedure thinlto_codegen_dispose (cg : thinlto_code_gen_t);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:591
   pragma Import (C, thinlto_codegen_dispose, "thinlto_codegen_dispose");

procedure thinlto_codegen_add_module
     (cg         : thinlto_code_gen_t;
      identifier : String;
      data       : String;
      length     : int);
   procedure thinlto_codegen_add_module_C
     (cg         : thinlto_code_gen_t;
      identifier : Interfaces.C.Strings.chars_ptr;
      data       : Interfaces.C.Strings.chars_ptr;
      length     : int);
   pragma Import (C, thinlto_codegen_add_module_C, "thinlto_codegen_add_module");

   procedure thinlto_codegen_process (cg : thinlto_code_gen_t);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:614
   pragma Import (C, thinlto_codegen_process, "thinlto_codegen_process");

   function thinlto_module_get_num_objects (cg : thinlto_code_gen_t) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:625
   pragma Import (C, thinlto_module_get_num_objects, "thinlto_module_get_num_objects");

   function thinlto_module_get_object (cg : thinlto_code_gen_t; index : unsigned) return bject_Buffer_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:636
   pragma Import (C, thinlto_module_get_object, "thinlto_module_get_object");

   function thinlto_module_get_num_object_files (cg : thinlto_code_gen_t) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:648
   pragma Import (C, thinlto_module_get_num_object_files, "thinlto_module_get_num_object_files");

   function thinlto_module_get_object_file
     (cg    : thinlto_code_gen_t;
      index : unsigned)
      return String;
   function thinlto_module_get_object_file_C
     (cg    : thinlto_code_gen_t;
      index : unsigned)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:659
   pragma Import (C, thinlto_module_get_object_file_C, "thinlto_module_get_object_file");

   function thinlto_codegen_set_pic_model (cg : thinlto_code_gen_t; arg2 : Codegen_Model_T) return Bool_T_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:668
   pragma Import (C, thinlto_codegen_set_pic_model, "thinlto_codegen_set_pic_model");

   procedure thinlto_codegen_set_savetemps_dir
     (cg             : thinlto_code_gen_t;
      Save_Temps_Dir : String);
   procedure thinlto_codegen_set_savetemps_dir_C
     (cg             : thinlto_code_gen_t;
      Save_Temps_Dir : Interfaces.C.Strings.chars_ptr);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:678
   pragma Import (C, thinlto_codegen_set_savetemps_dir_C, "thinlto_codegen_set_savetemps_dir");

   procedure thinlto_set_generated_objects_dir
     (cg             : thinlto_code_gen_t;
      Save_Temps_Dir : String);
   procedure thinlto_set_generated_objects_dir_C
     (cg             : thinlto_code_gen_t;
      Save_Temps_Dir : Interfaces.C.Strings.chars_ptr);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:689
   pragma Import (C, thinlto_set_generated_objects_dir_C, "thinlto_set_generated_objects_dir");

   procedure thinlto_codegen_set_cpu
     (cg  : thinlto_code_gen_t;
      cpu : String);
   procedure thinlto_codegen_set_cpu_C
     (cg  : thinlto_code_gen_t;
      cpu : Interfaces.C.Strings.chars_ptr);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:697
   pragma Import (C, thinlto_codegen_set_cpu_C, "thinlto_codegen_set_cpu");

   procedure thinlto_codegen_disable_codegen (cg : thinlto_code_gen_t; disable : Bool_T_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:705
   pragma Import (C, thinlto_codegen_disable_codegen, "thinlto_codegen_disable_codegen");

   procedure thinlto_codegen_set_codegen_only (cg : thinlto_code_gen_t; Codegen_Only : Bool_T_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:713
   pragma Import (C, thinlto_codegen_set_codegen_only, "thinlto_codegen_set_codegen_only");

   procedure thinlto_debug_options (options : System.Address; number : int);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:721
   pragma Import (C, thinlto_debug_options, "thinlto_debug_options");

   function Module_Is_Thinlto (C_Mod : Module_T_T) return Bool_T_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:728
   pragma Import (C, Module_Is_Thinlto, "lto_module_is_thinlto");

procedure thinlto_codegen_add_must_preserve_symbol
     (cg     : thinlto_code_gen_t;
      name   : String;
      length : int);
   procedure thinlto_codegen_add_must_preserve_symbol_C
     (cg     : thinlto_code_gen_t;
      name   : Interfaces.C.Strings.chars_ptr;
      length : int);
   pragma Import (C, thinlto_codegen_add_must_preserve_symbol_C, "thinlto_codegen_add_must_preserve_symbol");

procedure thinlto_codegen_add_cross_referenced_symbol
     (cg     : thinlto_code_gen_t;
      name   : String;
      length : int);
   procedure thinlto_codegen_add_cross_referenced_symbol_C
     (cg     : thinlto_code_gen_t;
      name   : Interfaces.C.Strings.chars_ptr;
      length : int);
   pragma Import (C, thinlto_codegen_add_cross_referenced_symbol_C, "thinlto_codegen_add_cross_referenced_symbol");

   procedure thinlto_codegen_set_cache_dir
     (cg        : thinlto_code_gen_t;
      Cache_Dir : String);
   procedure thinlto_codegen_set_cache_dir_C
     (cg        : thinlto_code_gen_t;
      Cache_Dir : Interfaces.C.Strings.chars_ptr);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:781
   pragma Import (C, thinlto_codegen_set_cache_dir_C, "thinlto_codegen_set_cache_dir");

   procedure thinlto_codegen_set_cache_pruning_interval (cg : thinlto_code_gen_t; interval : int);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:791
   pragma Import (C, thinlto_codegen_set_cache_pruning_interval, "thinlto_codegen_set_cache_pruning_interval");

   procedure thinlto_codegen_set_final_cache_size_relative_to_available_space (cg : thinlto_code_gen_t; percentage : unsigned);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:807
   pragma Import (C, thinlto_codegen_set_final_cache_size_relative_to_available_space, "thinlto_codegen_set_final_cache_size_relative_to_available_space");

   procedure thinlto_codegen_set_cache_entry_expiration (cg : thinlto_code_gen_t; expiration : unsigned);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/lto.h:816
   pragma Import (C, thinlto_codegen_set_cache_entry_expiration, "thinlto_codegen_set_cache_entry_expiration");

end LLVM.lto;

