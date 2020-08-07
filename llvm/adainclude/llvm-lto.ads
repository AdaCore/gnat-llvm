pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with Interfaces.C.Extensions;
with System;
with Interfaces.C.Strings;
with stddef_h;

package LLVM.lto is

   LTO_API_VERSION : constant := 26;  --  llvm-10.0.0.src/include/llvm-c/lto.h:49

  --===-- llvm-c/lto.h - LTO Public C Interface ---------------------*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header provides public interface to an abstract link time optimization*|
  --|* library.  LLVM provides an implementation of this interface for use with   *|
  --|* llvm bitcode files.                                                        *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

   subtype Bool_T_T is Extensions.bool;  -- llvm-10.0.0.src/include/llvm-c/lto.h:31

  -- MSVC in particular does not have anything like _Bool or bool in C, but we can
  --   at least make sure the type is the same size.  The implementation side will
  --   use C++ bool.  

  --*
  -- * @defgroup LLVMCLTO LTO
  -- * @ingroup LLVMC
  -- *
  -- * @{
  --  

  --*
  -- * \since prior to LTO_API_VERSION=3
  --  

  -- log2 of alignment  
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
   SYMBOL_ALIAS : constant Symbol_Attributes_T := 32768;  -- llvm-10.0.0.src/include/llvm-c/lto.h:74

  --*
  -- * \since prior to LTO_API_VERSION=3
  --  

   type Debug_Model_T is 
     (DEBUG_MODEL_NONE,
      DEBUG_MODEL_DWARF);
   pragma Convention (C, Debug_Model_T);  -- llvm-10.0.0.src/include/llvm-c/lto.h:82

  --*
  -- * \since prior to LTO_API_VERSION=3
  --  

   type Codegen_Model_T is 
     (CODEGEN_PIC_MODEL_STATIC,
      CODEGEN_PIC_MODEL_DYNAMIC,
      CODEGEN_PIC_MODEL_DYNAMIC_NO_PIC,
      CODEGEN_PIC_MODEL_DEFAULT);
   pragma Convention (C, Codegen_Model_T);  -- llvm-10.0.0.src/include/llvm-c/lto.h:92

  --* opaque reference to a loaded object module  
   --  skipped empty struct LLVMOpaqueLTOModule

   type Module_T_T is new System.Address;  -- llvm-10.0.0.src/include/llvm-c/lto.h:95

  --* opaque reference to a code generator  
   --  skipped empty struct LLVMOpaqueLTOCodeGenerator

   type Code_Gen_T_T is new System.Address;  -- llvm-10.0.0.src/include/llvm-c/lto.h:98

  --* opaque reference to a thin code generator  
   --  skipped empty struct LLVMOpaqueThinLTOCodeGenerator

   type thinlto_code_gen_t is new System.Address;  -- llvm-10.0.0.src/include/llvm-c/lto.h:101

  --*
  -- * Returns a printable string.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Get_Version
      return String;
   function Get_Version_C
      return Interfaces.C.Strings.chars_ptr;  -- llvm-10.0.0.src/include/llvm-c/lto.h:111
   pragma Import (C, Get_Version_C, "lto_get_version");

  --*
  -- * Returns the last error string or NULL if last operation was successful.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Get_Error_Message
      return String;
   function Get_Error_Message_C
      return Interfaces.C.Strings.chars_ptr;  -- llvm-10.0.0.src/include/llvm-c/lto.h:119
   pragma Import (C, Get_Error_Message_C, "lto_get_error_message");

  --*
  -- * Checks if a file is a loadable object file.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Module_Is_Object_File
     (path : String)
      return Bool_T_T;
   function Module_Is_Object_File_C
     (path : Interfaces.C.Strings.chars_ptr)
      return Bool_T_T;  -- llvm-10.0.0.src/include/llvm-c/lto.h:127
   pragma Import (C, Module_Is_Object_File_C, "lto_module_is_object_file");

  --*
  -- * Checks if a file is a loadable object compiled for requested target.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Module_Is_Object_File_For_Target
     (path                 : String;
      Target_Triple_Prefix : String)
      return Bool_T_T;
   function Module_Is_Object_File_For_Target_C
     (path                 : Interfaces.C.Strings.chars_ptr;
      Target_Triple_Prefix : Interfaces.C.Strings.chars_ptr)
      return Bool_T_T;  -- llvm-10.0.0.src/include/llvm-c/lto.h:135
   pragma Import (C, Module_Is_Object_File_For_Target_C, "lto_module_is_object_file_for_target");

  --*
  -- * Return true if \p Buffer contains a bitcode file with ObjC code (category
  -- * or class) in it.
  -- *
  -- * \since LTO_API_VERSION=20
  --  

   function Module_Has_Objc_Category (mem : System.Address; length : stddef_h.size_t) return Bool_T_T;  -- llvm-10.0.0.src/include/llvm-c/lto.h:145
   pragma Import (C, Module_Has_Objc_Category, "lto_module_has_objc_category");

  --*
  -- * Checks if a buffer is a loadable object file.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Module_Is_Object_File_In_Memory (mem : System.Address; length : stddef_h.size_t) return Bool_T_T;  -- llvm-10.0.0.src/include/llvm-c/lto.h:152
   pragma Import (C, Module_Is_Object_File_In_Memory, "lto_module_is_object_file_in_memory");

  --*
  -- * Checks if a buffer is a loadable object compiled for requested target.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

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

  --*
  -- * Loads an object file from disk.
  -- * Returns NULL on error (check lto_get_error_message() for details).
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Module_Create
     (path : String)
      return Module_T_T;
   function Module_Create_C
     (path : Interfaces.C.Strings.chars_ptr)
      return Module_T_T;  -- llvm-10.0.0.src/include/llvm-c/lto.h:171
   pragma Import (C, Module_Create_C, "lto_module_create");

  --*
  -- * Loads an object file from memory.
  -- * Returns NULL on error (check lto_get_error_message() for details).
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Module_Create_From_Memory (mem : System.Address; length : stddef_h.size_t) return Module_T_T;  -- llvm-10.0.0.src/include/llvm-c/lto.h:180
   pragma Import (C, Module_Create_From_Memory, "lto_module_create_from_memory");

  --*
  -- * Loads an object file from memory with an extra path argument.
  -- * Returns NULL on error (check lto_get_error_message() for details).
  -- *
  -- * \since LTO_API_VERSION=9
  --  

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

  --*
  -- * Loads an object file in its own context.
  -- *
  -- * Loads an object file in its own LLVMContext.  This function call is
  -- * thread-safe.  However, modules created this way should not be merged into an
  -- * lto_code_gen_t using \a lto_codegen_add_module().
  -- *
  -- * Returns NULL on error (check lto_get_error_message() for details).
  -- *
  -- * \since LTO_API_VERSION=11
  --  

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

  --*
  -- * Loads an object file in the codegen context.
  -- *
  -- * Loads an object file into the same context as \c cg.  The module is safe to
  -- * add using \a lto_codegen_add_module().
  -- *
  -- * Returns NULL on error (check lto_get_error_message() for details).
  -- *
  -- * \since LTO_API_VERSION=11
  --  

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

  --*
  -- * Loads an object file from disk. The seek point of fd is not preserved.
  -- * Returns NULL on error (check lto_get_error_message() for details).
  -- *
  -- * \since LTO_API_VERSION=5
  --  

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

  --*
  -- * Loads an object file from disk. The seek point of fd is not preserved.
  -- * Returns NULL on error (check lto_get_error_message() for details).
  -- *
  -- * \since LTO_API_VERSION=5
  --  

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

  --*
  -- * Frees all memory internally allocated by the module.
  -- * Upon return the lto_module_t is no longer valid.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   procedure Module_Dispose (C_Mod : Module_T_T);  -- llvm-10.0.0.src/include/llvm-c/lto.h:247
   pragma Import (C, Module_Dispose, "lto_module_dispose");

  --*
  -- * Returns triple string which the object module was compiled under.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Module_Get_Target_Triple
     (C_Mod : Module_T_T)
      return String;
   function Module_Get_Target_Triple_C
     (C_Mod : Module_T_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-10.0.0.src/include/llvm-c/lto.h:255
   pragma Import (C, Module_Get_Target_Triple_C, "lto_module_get_target_triple");

  --*
  -- * Sets triple string with which the object will be codegened.
  -- *
  -- * \since LTO_API_VERSION=4
  --  

   procedure Module_Set_Target_Triple
     (C_Mod  : Module_T_T;
      triple : String);
   procedure Module_Set_Target_Triple_C
     (C_Mod  : Module_T_T;
      triple : Interfaces.C.Strings.chars_ptr);  -- llvm-10.0.0.src/include/llvm-c/lto.h:263
   pragma Import (C, Module_Set_Target_Triple_C, "lto_module_set_target_triple");

  --*
  -- * Returns the number of symbols in the object module.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Module_Get_Num_Symbols (C_Mod : Module_T_T) return unsigned;  -- llvm-10.0.0.src/include/llvm-c/lto.h:271
   pragma Import (C, Module_Get_Num_Symbols, "lto_module_get_num_symbols");

  --*
  -- * Returns the name of the ith symbol in the object module.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Module_Get_Symbol_Name
     (C_Mod : Module_T_T;
      index : unsigned)
      return String;
   function Module_Get_Symbol_Name_C
     (C_Mod : Module_T_T;
      index : unsigned)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-10.0.0.src/include/llvm-c/lto.h:279
   pragma Import (C, Module_Get_Symbol_Name_C, "lto_module_get_symbol_name");

  --*
  -- * Returns the attributes of the ith symbol in the object module.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Module_Get_Symbol_Attribute (C_Mod : Module_T_T; index : unsigned) return Symbol_Attributes_T;  -- llvm-10.0.0.src/include/llvm-c/lto.h:287
   pragma Import (C, Module_Get_Symbol_Attribute, "lto_module_get_symbol_attribute");

  --*
  -- * Returns the module's linker options.
  -- *
  -- * The linker options may consist of multiple flags. It is the linker's
  -- * responsibility to split the flags using a platform-specific mechanism.
  -- *
  -- * \since LTO_API_VERSION=16
  --  

   function Module_Get_Linkeropts
     (C_Mod : Module_T_T)
      return String;
   function Module_Get_Linkeropts_C
     (C_Mod : Module_T_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-10.0.0.src/include/llvm-c/lto.h:298
   pragma Import (C, Module_Get_Linkeropts_C, "lto_module_get_linkeropts");

  --*
  -- * Diagnostic severity.
  -- *
  -- * \since LTO_API_VERSION=7
  --  

  -- Added in LTO_API_VERSION=10.
   subtype Codegen_Diagnostic_Severity_T_T is unsigned;
   DS_ERROR : constant Codegen_Diagnostic_Severity_T_T := 0;
   DS_WARNING : constant Codegen_Diagnostic_Severity_T_T := 1;
   DS_REMARK : constant Codegen_Diagnostic_Severity_T_T := 3;
   DS_NOTE : constant Codegen_Diagnostic_Severity_T_T := 2;  -- llvm-10.0.0.src/include/llvm-c/lto.h:310

  --*
  -- * Diagnostic handler type.
  -- * \p severity defines the severity.
  -- * \p diag is the actual diagnostic.
  -- * The diagnostic is not prefixed by any of severity keyword, e.g., 'error: '.
  -- * \p ctxt is used to pass the context set with the diagnostic handler.
  -- *
  -- * \since LTO_API_VERSION=7
  --  

   type Diagnostic_Handler_T_T is access procedure 
        (arg1 : Codegen_Diagnostic_Severity_T_T;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : System.Address);
   pragma Convention (C, Diagnostic_Handler_T_T);  -- llvm-10.0.0.src/include/llvm-c/lto.h:321

  --*
  -- * Set a diagnostic handler and the related context (void *).
  -- * This is more general than lto_get_error_message, as the diagnostic handler
  -- * can be called at anytime within lto.
  -- *
  -- * \since LTO_API_VERSION=7
  --  

   procedure Codegen_Set_Diagnostic_Handler
     (arg1 : Code_Gen_T_T;
      arg2 : Diagnostic_Handler_T_T;
      arg3 : System.Address);  -- llvm-10.0.0.src/include/llvm-c/lto.h:331
   pragma Import (C, Codegen_Set_Diagnostic_Handler, "lto_codegen_set_diagnostic_handler");

  --*
  -- * Instantiates a code generator.
  -- * Returns NULL on error (check lto_get_error_message() for details).
  -- *
  -- * All modules added using \a lto_codegen_add_module() must have been created
  -- * in the same context as the codegen.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Codegen_Create return Code_Gen_T_T;  -- llvm-10.0.0.src/include/llvm-c/lto.h:345
   pragma Import (C, Codegen_Create, "lto_codegen_create");

  --*
  -- * Instantiate a code generator in its own context.
  -- *
  -- * Instantiates a code generator in its own context.  Modules added via \a
  -- * lto_codegen_add_module() must have all been created in the same context,
  -- * using \a lto_module_create_in_codegen_context().
  -- *
  -- * \since LTO_API_VERSION=11
  --  

   function Codegen_Create_In_Local_Context return Code_Gen_T_T;  -- llvm-10.0.0.src/include/llvm-c/lto.h:357
   pragma Import (C, Codegen_Create_In_Local_Context, "lto_codegen_create_in_local_context");

  --*
  -- * Frees all code generator and all memory it internally allocated.
  -- * Upon return the lto_code_gen_t is no longer valid.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   procedure Codegen_Dispose (arg1 : Code_Gen_T_T);  -- llvm-10.0.0.src/include/llvm-c/lto.h:366
   pragma Import (C, Codegen_Dispose, "lto_codegen_dispose");

  --*
  -- * Add an object module to the set of modules for which code will be generated.
  -- * Returns true on error (check lto_get_error_message() for details).
  -- *
  -- * \c cg and \c mod must both be in the same context.  See \a
  -- * lto_codegen_create_in_local_context() and \a
  -- * lto_module_create_in_codegen_context().
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Codegen_Add_Module (cg : Code_Gen_T_T; C_Mod : Module_T_T) return Bool_T_T;  -- llvm-10.0.0.src/include/llvm-c/lto.h:379
   pragma Import (C, Codegen_Add_Module, "lto_codegen_add_module");

  --*
  -- * Sets the object module for code generation. This will transfer the ownership
  -- * of the module to the code generator.
  -- *
  -- * \c cg and \c mod must both be in the same context.
  -- *
  -- * \since LTO_API_VERSION=13
  --  

   procedure Codegen_Set_Module (cg : Code_Gen_T_T; C_Mod : Module_T_T);  -- llvm-10.0.0.src/include/llvm-c/lto.h:390
   pragma Import (C, Codegen_Set_Module, "lto_codegen_set_module");

  --*
  -- * Sets if debug info should be generated.
  -- * Returns true on error (check lto_get_error_message() for details).
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Codegen_Set_Debug_Model (cg : Code_Gen_T_T; arg2 : Debug_Model_T) return Bool_T_T;  -- llvm-10.0.0.src/include/llvm-c/lto.h:399
   pragma Import (C, Codegen_Set_Debug_Model, "lto_codegen_set_debug_model");

  --*
  -- * Sets which PIC code model to generated.
  -- * Returns true on error (check lto_get_error_message() for details).
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Codegen_Set_Pic_Model (cg : Code_Gen_T_T; arg2 : Codegen_Model_T) return Bool_T_T;  -- llvm-10.0.0.src/include/llvm-c/lto.h:408
   pragma Import (C, Codegen_Set_Pic_Model, "lto_codegen_set_pic_model");

  --*
  -- * Sets the cpu to generate code for.
  -- *
  -- * \since LTO_API_VERSION=4
  --  

   procedure Codegen_Set_Cpu
     (cg  : Code_Gen_T_T;
      cpu : String);
   procedure Codegen_Set_Cpu_C
     (cg  : Code_Gen_T_T;
      cpu : Interfaces.C.Strings.chars_ptr);  -- llvm-10.0.0.src/include/llvm-c/lto.h:416
   pragma Import (C, Codegen_Set_Cpu_C, "lto_codegen_set_cpu");

  --*
  -- * Sets the location of the assembler tool to run. If not set, libLTO
  -- * will use gcc to invoke the assembler.
  -- *
  -- * \since LTO_API_VERSION=3
  --  

   procedure Codegen_Set_Assembler_Path
     (cg   : Code_Gen_T_T;
      path : String);
   procedure Codegen_Set_Assembler_Path_C
     (cg   : Code_Gen_T_T;
      path : Interfaces.C.Strings.chars_ptr);  -- llvm-10.0.0.src/include/llvm-c/lto.h:425
   pragma Import (C, Codegen_Set_Assembler_Path_C, "lto_codegen_set_assembler_path");

  --*
  -- * Sets extra arguments that libLTO should pass to the assembler.
  -- *
  -- * \since LTO_API_VERSION=4
  --  

   procedure Codegen_Set_Assembler_Args
     (cg : Code_Gen_T_T;
      args : System.Address;
      nargs : int);  -- llvm-10.0.0.src/include/llvm-c/lto.h:433
   pragma Import (C, Codegen_Set_Assembler_Args, "lto_codegen_set_assembler_args");

  --*
  -- * Adds to a list of all global symbols that must exist in the final generated
  -- * code. If a function is not listed there, it might be inlined into every usage
  -- * and optimized away.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   procedure Codegen_Add_Must_Preserve_Symbol
     (cg     : Code_Gen_T_T;
      symbol : String);
   procedure Codegen_Add_Must_Preserve_Symbol_C
     (cg     : Code_Gen_T_T;
      symbol : Interfaces.C.Strings.chars_ptr);  -- llvm-10.0.0.src/include/llvm-c/lto.h:444
   pragma Import (C, Codegen_Add_Must_Preserve_Symbol_C, "lto_codegen_add_must_preserve_symbol");

  --*
  -- * Writes a new object file at the specified path that contains the
  -- * merged contents of all modules added so far.
  -- * Returns true on error (check lto_get_error_message() for details).
  -- *
  -- * \since LTO_API_VERSION=5
  --  

   function Codegen_Write_Merged_Modules
     (cg   : Code_Gen_T_T;
      path : String)
      return Bool_T_T;
   function Codegen_Write_Merged_Modules_C
     (cg   : Code_Gen_T_T;
      path : Interfaces.C.Strings.chars_ptr)
      return Bool_T_T;  -- llvm-10.0.0.src/include/llvm-c/lto.h:454
   pragma Import (C, Codegen_Write_Merged_Modules_C, "lto_codegen_write_merged_modules");

  --*
  -- * Generates code for all added modules into one native object file.
  -- * This calls lto_codegen_optimize then lto_codegen_compile_optimized.
  -- *
  -- * On success returns a pointer to a generated mach-o/ELF buffer and
  -- * length set to the buffer size.  The buffer is owned by the
  -- * lto_code_gen_t and will be freed when lto_codegen_dispose()
  -- * is called, or lto_codegen_compile() is called again.
  -- * On failure, returns NULL (check lto_get_error_message() for details).
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Codegen_Compile (cg : Code_Gen_T_T; length : access stddef_h.size_t) return System.Address;  -- llvm-10.0.0.src/include/llvm-c/lto.h:469
   pragma Import (C, Codegen_Compile, "lto_codegen_compile");

  --*
  -- * Generates code for all added modules into one native object file.
  -- * This calls lto_codegen_optimize then lto_codegen_compile_optimized (instead
  -- * of returning a generated mach-o/ELF buffer, it writes to a file).
  -- *
  -- * The name of the file is written to name. Returns true on error.
  -- *
  -- * \since LTO_API_VERSION=5
  --  

   function Codegen_Compile_To_File (cg : Code_Gen_T_T; name : System.Address) return Bool_T_T;  -- llvm-10.0.0.src/include/llvm-c/lto.h:481
   pragma Import (C, Codegen_Compile_To_File, "lto_codegen_compile_to_file");

  --*
  -- * Runs optimization for the merged module. Returns true on error.
  -- *
  -- * \since LTO_API_VERSION=12
  --  

   function Codegen_Optimize (cg : Code_Gen_T_T) return Bool_T_T;  -- llvm-10.0.0.src/include/llvm-c/lto.h:489
   pragma Import (C, Codegen_Optimize, "lto_codegen_optimize");

  --*
  -- * Generates code for the optimized merged module into one native object file.
  -- * It will not run any IR optimizations on the merged module.
  -- *
  -- * On success returns a pointer to a generated mach-o/ELF buffer and length set
  -- * to the buffer size.  The buffer is owned by the lto_code_gen_t and will be
  -- * freed when lto_codegen_dispose() is called, or
  -- * lto_codegen_compile_optimized() is called again. On failure, returns NULL
  -- * (check lto_get_error_message() for details).
  -- *
  -- * \since LTO_API_VERSION=12
  --  

   function Codegen_Compile_Optimized (cg : Code_Gen_T_T; length : access stddef_h.size_t) return System.Address;  -- llvm-10.0.0.src/include/llvm-c/lto.h:504
   pragma Import (C, Codegen_Compile_Optimized, "lto_codegen_compile_optimized");

  --*
  -- * Returns the runtime API version.
  -- *
  -- * \since LTO_API_VERSION=12
  --  

   function Api_Version_Fun return unsigned;  -- llvm-10.0.0.src/include/llvm-c/lto.h:512
   pragma Import (C, Api_Version_Fun, "lto_api_version_fun");

  --*
  -- * Sets options to help debug codegen bugs.
  -- *
  -- * This function takes one or more options separated by spaces.
  -- * Warning: passing file paths through this function may confuse the argument
  -- * parser if the paths contain spaces.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   procedure Codegen_Debug_Options
     (cg   : Code_Gen_T_T;
      arg2 : String);
   procedure Codegen_Debug_Options_C
     (cg   : Code_Gen_T_T;
      arg2 : Interfaces.C.Strings.chars_ptr);  -- llvm-10.0.0.src/include/llvm-c/lto.h:524
   pragma Import (C, Codegen_Debug_Options_C, "lto_codegen_debug_options");

  --*
  -- * Same as the previous function, but takes every option separately through an
  -- * array.
  -- *
  -- * \since prior to LTO_API_VERSION=26
  --  

   procedure Codegen_Debug_Options_Array
     (cg : Code_Gen_T_T;
      arg2 : System.Address;
      number : int);  -- llvm-10.0.0.src/include/llvm-c/lto.h:532
   pragma Import (C, Codegen_Debug_Options_Array, "lto_codegen_debug_options_array");

  --*
  -- * Initializes LLVM disassemblers.
  -- * FIXME: This doesn't really belong here.
  -- *
  -- * \since LTO_API_VERSION=5
  --  

   procedure Initialize_Disassembler;  -- llvm-10.0.0.src/include/llvm-c/lto.h:542
   pragma Import (C, Initialize_Disassembler, "lto_initialize_disassembler");

  --*
  -- * Sets if we should run internalize pass during optimization and code
  -- * generation.
  -- *
  -- * \since LTO_API_VERSION=14
  --  

   procedure Codegen_Set_Should_Internalize (cg : Code_Gen_T_T; Should_Internalize : Bool_T_T);  -- llvm-10.0.0.src/include/llvm-c/lto.h:551
   pragma Import (C, Codegen_Set_Should_Internalize, "lto_codegen_set_should_internalize");

  --*
  -- * Set whether to embed uselists in bitcode.
  -- *
  -- * Sets whether \a lto_codegen_write_merged_modules() should embed uselists in
  -- * output bitcode.  This should be turned on for all -save-temps output.
  -- *
  -- * \since LTO_API_VERSION=15
  --  

   procedure Codegen_Set_Should_Embed_Uselists (cg : Code_Gen_T_T; Should_Embed_Uselists : Bool_T_T);  -- llvm-10.0.0.src/include/llvm-c/lto.h:563
   pragma Import (C, Codegen_Set_Should_Embed_Uselists, "lto_codegen_set_should_embed_uselists");

  --* Opaque reference to an LTO input file  
   --  skipped empty struct LLVMOpaqueLTOInput

   type Input_T_T is new System.Address;  -- llvm-10.0.0.src/include/llvm-c/lto.h:567

  --*
  --  * Creates an LTO input file from a buffer. The path
  --  * argument is used for diagnotics as this function
  --  * otherwise does not know which file the given buffer
  --  * is associated with.
  --  *
  --  * \since LTO_API_VERSION=24
  --   

function Input_Create
     (buffer      : System.Address;
      Buffer_Size : stddef_h.size_t;
      path        : String)
      return Input_T_T;
   function Input_Create_C
     (buffer      : System.Address;
      Buffer_Size : stddef_h.size_t;
      path        : Interfaces.C.Strings.chars_ptr)
      return Input_T_T;
   pragma Import (C, Input_Create_C, "lto_input_create");

  --*
  --  * Frees all memory internally allocated by the LTO input file.
  --  * Upon return the lto_module_t is no longer valid.
  --  *
  --  * \since LTO_API_VERSION=24
  --   

   procedure Input_Dispose (input : Input_T_T);  -- llvm-10.0.0.src/include/llvm-c/lto.h:587
   pragma Import (C, Input_Dispose, "lto_input_dispose");

  --*
  --  * Returns the number of dependent library specifiers
  --  * for the given LTO input file.
  --  *
  --  * \since LTO_API_VERSION=24
  --   

   function Input_Get_Num_Dependent_Libraries (input : Input_T_T) return unsigned;  -- llvm-10.0.0.src/include/llvm-c/lto.h:595
   pragma Import (C, Input_Get_Num_Dependent_Libraries, "lto_input_get_num_dependent_libraries");

  --*
  --  * Returns the ith dependent library specifier
  --  * for the given LTO input file. The returned
  --  * string is not null-terminated.
  --  *
  --  * \since LTO_API_VERSION=24
  --   

function Input_Get_Dependent_Library
     (input : Input_T_T;
      index : stddef_h.size_t;
      size  : stddef_h.size_t)
      return String;
   function Input_Get_Dependent_Library_C
     (input : Input_T_T;
      index : stddef_h.size_t;
      size  : stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Input_Get_Dependent_Library_C, "lto_input_get_dependent_library");

  --*
  -- * Returns the list of libcall symbols that can be generated by LTO
  -- * that might not be visible from the symbol table of bitcode files.
  -- *
  -- * \since prior to LTO_API_VERSION=25
  --  

   function Runtime_Lib_Symbols_List (size : access stddef_h.size_t) return System.Address;  -- llvm-10.0.0.src/include/llvm-c/lto.h:614
   pragma Import (C, Runtime_Lib_Symbols_List, "lto_runtime_lib_symbols_list");

  --*
  -- * @} // endgoup LLVMCLTO
  -- * @defgroup LLVMCTLTO ThinLTO
  -- * @ingroup LLVMC
  -- *
  -- * @{
  --  

  --*
  -- * Type to wrap a single object returned by ThinLTO.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   type bject_Buffer_T is record
      Buffer : Interfaces.C.Strings.chars_ptr;  -- llvm-10.0.0.src/include/llvm-c/lto.h:630
      Size : aliased stddef_h.size_t;  -- llvm-10.0.0.src/include/llvm-c/lto.h:631
   end record;
   pragma Convention (C_Pass_By_Copy, bject_Buffer_T);  -- llvm-10.0.0.src/include/llvm-c/lto.h:632

   --  skipped anonymous struct anon_19

  --*
  -- * Instantiates a ThinLTO code generator.
  -- * Returns NULL on error (check lto_get_error_message() for details).
  -- *
  -- *
  -- * The ThinLTOCodeGenerator is not intended to be reuse for multiple
  -- * compilation: the model is that the client adds modules to the generator and
  -- * ask to perform the ThinLTO optimizations / codegen, and finally destroys the
  -- * codegenerator.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   function thinlto_create_codegen return thinlto_code_gen_t;  -- llvm-10.0.0.src/include/llvm-c/lto.h:646
   pragma Import (C, thinlto_create_codegen, "thinlto_create_codegen");

  --*
  -- * Frees the generator and all memory it internally allocated.
  -- * Upon return the thinlto_code_gen_t is no longer valid.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   procedure thinlto_codegen_dispose (cg : thinlto_code_gen_t);  -- llvm-10.0.0.src/include/llvm-c/lto.h:654
   pragma Import (C, thinlto_codegen_dispose, "thinlto_codegen_dispose");

  --*
  -- * Add a module to a ThinLTO code generator. Identifier has to be unique among
  -- * all the modules in a code generator. The data buffer stays owned by the
  -- * client, and is expected to be available for the entire lifetime of the
  -- * thinlto_code_gen_t it is added to.
  -- *
  -- * On failure, returns NULL (check lto_get_error_message() for details).
  -- *
  -- *
  -- * \since LTO_API_VERSION=18
  --  

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

  --*
  -- * Optimize and codegen all the modules added to the codegenerator using
  -- * ThinLTO. Resulting objects are accessible using thinlto_module_get_object().
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   procedure thinlto_codegen_process (cg : thinlto_code_gen_t);  -- llvm-10.0.0.src/include/llvm-c/lto.h:677
   pragma Import (C, thinlto_codegen_process, "thinlto_codegen_process");

  --*
  -- * Returns the number of object files produced by the ThinLTO CodeGenerator.
  -- *
  -- * It usually matches the number of input files, but this is not a guarantee of
  -- * the API and may change in future implementation, so the client should not
  -- * assume it.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   function thinlto_module_get_num_objects (cg : thinlto_code_gen_t) return unsigned;  -- llvm-10.0.0.src/include/llvm-c/lto.h:688
   pragma Import (C, thinlto_module_get_num_objects, "thinlto_module_get_num_objects");

  --*
  -- * Returns a reference to the ith object file produced by the ThinLTO
  -- * CodeGenerator.
  -- *
  -- * Client should use \p thinlto_module_get_num_objects() to get the number of
  -- * available objects.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   function thinlto_module_get_object (cg : thinlto_code_gen_t; index : unsigned) return bject_Buffer_T;  -- llvm-10.0.0.src/include/llvm-c/lto.h:699
   pragma Import (C, thinlto_module_get_object, "thinlto_module_get_object");

  --*
  -- * Returns the number of object files produced by the ThinLTO CodeGenerator.
  -- *
  -- * It usually matches the number of input files, but this is not a guarantee of
  -- * the API and may change in future implementation, so the client should not
  -- * assume it.
  -- *
  -- * \since LTO_API_VERSION=21
  --  

   function thinlto_module_get_num_object_files (cg : thinlto_code_gen_t) return unsigned;  -- llvm-10.0.0.src/include/llvm-c/lto.h:711
   pragma Import (C, thinlto_module_get_num_object_files, "thinlto_module_get_num_object_files");

  --*
  -- * Returns the path to the ith object file produced by the ThinLTO
  -- * CodeGenerator.
  -- *
  -- * Client should use \p thinlto_module_get_num_object_files() to get the number
  -- * of available objects.
  -- *
  -- * \since LTO_API_VERSION=21
  --  

   function thinlto_module_get_object_file
     (cg    : thinlto_code_gen_t;
      index : unsigned)
      return String;
   function thinlto_module_get_object_file_C
     (cg    : thinlto_code_gen_t;
      index : unsigned)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-10.0.0.src/include/llvm-c/lto.h:722
   pragma Import (C, thinlto_module_get_object_file_C, "thinlto_module_get_object_file");

  --*
  -- * Sets which PIC code model to generate.
  -- * Returns true on error (check lto_get_error_message() for details).
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   function thinlto_codegen_set_pic_model (cg : thinlto_code_gen_t; arg2 : Codegen_Model_T) return Bool_T_T;  -- llvm-10.0.0.src/include/llvm-c/lto.h:731
   pragma Import (C, thinlto_codegen_set_pic_model, "thinlto_codegen_set_pic_model");

  --*
  -- * Sets the path to a directory to use as a storage for temporary bitcode files.
  -- * The intention is to make the bitcode files available for debugging at various
  -- * stage of the pipeline.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   procedure thinlto_codegen_set_savetemps_dir
     (cg             : thinlto_code_gen_t;
      Save_Temps_Dir : String);
   procedure thinlto_codegen_set_savetemps_dir_C
     (cg             : thinlto_code_gen_t;
      Save_Temps_Dir : Interfaces.C.Strings.chars_ptr);  -- llvm-10.0.0.src/include/llvm-c/lto.h:741
   pragma Import (C, thinlto_codegen_set_savetemps_dir_C, "thinlto_codegen_set_savetemps_dir");

  --*
  -- * Set the path to a directory where to save generated object files. This
  -- * path can be used by a linker to request on-disk files instead of in-memory
  -- * buffers. When set, results are available through
  -- * thinlto_module_get_object_file() instead of thinlto_module_get_object().
  -- *
  -- * \since LTO_API_VERSION=21
  --  

   procedure thinlto_set_generated_objects_dir
     (cg             : thinlto_code_gen_t;
      Save_Temps_Dir : String);
   procedure thinlto_set_generated_objects_dir_C
     (cg             : thinlto_code_gen_t;
      Save_Temps_Dir : Interfaces.C.Strings.chars_ptr);  -- llvm-10.0.0.src/include/llvm-c/lto.h:752
   pragma Import (C, thinlto_set_generated_objects_dir_C, "thinlto_set_generated_objects_dir");

  --*
  -- * Sets the cpu to generate code for.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   procedure thinlto_codegen_set_cpu
     (cg  : thinlto_code_gen_t;
      cpu : String);
   procedure thinlto_codegen_set_cpu_C
     (cg  : thinlto_code_gen_t;
      cpu : Interfaces.C.Strings.chars_ptr);  -- llvm-10.0.0.src/include/llvm-c/lto.h:760
   pragma Import (C, thinlto_codegen_set_cpu_C, "thinlto_codegen_set_cpu");

  --*
  -- * Disable CodeGen, only run the stages till codegen and stop. The output will
  -- * be bitcode.
  -- *
  -- * \since LTO_API_VERSION=19
  --  

   procedure thinlto_codegen_disable_codegen (cg : thinlto_code_gen_t; disable : Bool_T_T);  -- llvm-10.0.0.src/include/llvm-c/lto.h:768
   pragma Import (C, thinlto_codegen_disable_codegen, "thinlto_codegen_disable_codegen");

  --*
  -- * Perform CodeGen only: disable all other stages.
  -- *
  -- * \since LTO_API_VERSION=19
  --  

   procedure thinlto_codegen_set_codegen_only (cg : thinlto_code_gen_t; Codegen_Only : Bool_T_T);  -- llvm-10.0.0.src/include/llvm-c/lto.h:776
   pragma Import (C, thinlto_codegen_set_codegen_only, "thinlto_codegen_set_codegen_only");

  --*
  -- * Parse -mllvm style debug options.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   procedure thinlto_debug_options (options : System.Address; number : int);  -- llvm-10.0.0.src/include/llvm-c/lto.h:784
   pragma Import (C, thinlto_debug_options, "thinlto_debug_options");

  --*
  -- * Test if a module has support for ThinLTO linking.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   function Module_Is_Thinlto (C_Mod : Module_T_T) return Bool_T_T;  -- llvm-10.0.0.src/include/llvm-c/lto.h:791
   pragma Import (C, Module_Is_Thinlto, "lto_module_is_thinlto");

  --*
  -- * Adds a symbol to the list of global symbols that must exist in the final
  -- * generated code. If a function is not listed there, it might be inlined into
  -- * every usage and optimized away. For every single module, the functions
  -- * referenced from code outside of the ThinLTO modules need to be added here.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

procedure thinlto_codegen_add_must_preserve_symbol
     (cg     : thinlto_code_gen_t;
      name   : String;
      length : int);
   procedure thinlto_codegen_add_must_preserve_symbol_C
     (cg     : thinlto_code_gen_t;
      name   : Interfaces.C.Strings.chars_ptr;
      length : int);
   pragma Import (C, thinlto_codegen_add_must_preserve_symbol_C, "thinlto_codegen_add_must_preserve_symbol");

  --*
  -- * Adds a symbol to the list of global symbols that are cross-referenced between
  -- * ThinLTO files. If the ThinLTO CodeGenerator can ensure that every
  -- * references from a ThinLTO module to this symbol is optimized away, then
  -- * the symbol can be discarded.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

procedure thinlto_codegen_add_cross_referenced_symbol
     (cg     : thinlto_code_gen_t;
      name   : String;
      length : int);
   procedure thinlto_codegen_add_cross_referenced_symbol_C
     (cg     : thinlto_code_gen_t;
      name   : Interfaces.C.Strings.chars_ptr;
      length : int);
   pragma Import (C, thinlto_codegen_add_cross_referenced_symbol_C, "thinlto_codegen_add_cross_referenced_symbol");

  --*
  -- * @} // endgoup LLVMCTLTO
  -- * @defgroup LLVMCTLTO_CACHING ThinLTO Cache Control
  -- * @ingroup LLVMCTLTO
  -- *
  -- * These entry points control the ThinLTO cache. The cache is intended to
  -- * support incremental builds, and thus needs to be persistent across builds.
  -- * The client enables the cache by supplying a path to an existing directory.
  -- * The code generator will use this to store objects files that may be reused
  -- * during a subsequent build.
  -- * To avoid filling the disk space, a few knobs are provided:
  -- *  - The pruning interval limits the frequency at which the garbage collector
  -- *    will try to scan the cache directory to prune expired entries.
  -- *    Setting to a negative number disables the pruning.
  -- *  - The pruning expiration time indicates to the garbage collector how old an
  -- *    entry needs to be to be removed.
  -- *  - Finally, the garbage collector can be instructed to prune the cache until
  -- *    the occupied space goes below a threshold.
  -- * @{
  --  

  --*
  -- * Sets the path to a directory to use as a cache storage for incremental build.
  -- * Setting this activates caching.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   procedure thinlto_codegen_set_cache_dir
     (cg        : thinlto_code_gen_t;
      Cache_Dir : String);
   procedure thinlto_codegen_set_cache_dir_C
     (cg        : thinlto_code_gen_t;
      Cache_Dir : Interfaces.C.Strings.chars_ptr);  -- llvm-10.0.0.src/include/llvm-c/lto.h:844
   pragma Import (C, thinlto_codegen_set_cache_dir_C, "thinlto_codegen_set_cache_dir");

  --*
  -- * Sets the cache pruning interval (in seconds). A negative value disables the
  -- * pruning. An unspecified default value will be applied, and a value of 0 will
  -- * force prunning to occur.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   procedure thinlto_codegen_set_cache_pruning_interval (cg : thinlto_code_gen_t; interval : int);  -- llvm-10.0.0.src/include/llvm-c/lto.h:854
   pragma Import (C, thinlto_codegen_set_cache_pruning_interval, "thinlto_codegen_set_cache_pruning_interval");

  --*
  -- * Sets the maximum cache size that can be persistent across build, in terms of
  -- * percentage of the available space on the disk. Set to 100 to indicate
  -- * no limit, 50 to indicate that the cache size will not be left over half the
  -- * available space. A value over 100 will be reduced to 100, a value of 0 will
  -- * be ignored. An unspecified default value will be applied.
  -- *
  -- * The formula looks like:
  -- *  AvailableSpace = FreeSpace + ExistingCacheSize
  -- *  NewCacheSize = AvailableSpace * P/100
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   procedure thinlto_codegen_set_final_cache_size_relative_to_available_space (cg : thinlto_code_gen_t; percentage : unsigned);  -- llvm-10.0.0.src/include/llvm-c/lto.h:870
   pragma Import (C, thinlto_codegen_set_final_cache_size_relative_to_available_space, "thinlto_codegen_set_final_cache_size_relative_to_available_space");

  --*
  -- * Sets the expiration (in seconds) for an entry in the cache. An unspecified
  -- * default value will be applied. A value of 0 will be ignored.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   procedure thinlto_codegen_set_cache_entry_expiration (cg : thinlto_code_gen_t; expiration : unsigned);  -- llvm-10.0.0.src/include/llvm-c/lto.h:879
   pragma Import (C, thinlto_codegen_set_cache_entry_expiration, "thinlto_codegen_set_cache_entry_expiration");

  --*
  -- * Sets the maximum size of the cache directory (in bytes). A value over the
  -- * amount of available space on the disk will be reduced to the amount of
  -- * available space. An unspecified default value will be applied. A value of 0
  -- * will be ignored.
  -- *
  -- * \since LTO_API_VERSION=22
  --  

   procedure thinlto_codegen_set_cache_size_bytes (cg : thinlto_code_gen_t; Max_Size_Bytes : unsigned);  -- llvm-10.0.0.src/include/llvm-c/lto.h:890
   pragma Import (C, thinlto_codegen_set_cache_size_bytes, "thinlto_codegen_set_cache_size_bytes");

  --*
  -- * Same as thinlto_codegen_set_cache_size_bytes, except the maximum size is in
  -- * megabytes (2^20 bytes).
  -- *
  -- * \since LTO_API_VERSION=23
  --  

   procedure thinlto_codegen_set_cache_size_megabytes (cg : thinlto_code_gen_t; Max_Size_Megabytes : unsigned);  -- llvm-10.0.0.src/include/llvm-c/lto.h:900
   pragma Import (C, thinlto_codegen_set_cache_size_megabytes, "thinlto_codegen_set_cache_size_megabytes");

  --*
  -- * Sets the maximum number of files in the cache directory. An unspecified
  -- * default value will be applied. A value of 0 will be ignored.
  -- *
  -- * \since LTO_API_VERSION=22
  --  

   procedure thinlto_codegen_set_cache_size_files (cg : thinlto_code_gen_t; Max_Size_Files : unsigned);  -- llvm-10.0.0.src/include/llvm-c/lto.h:909
   pragma Import (C, thinlto_codegen_set_cache_size_files, "thinlto_codegen_set_cache_size_files");

  --*
  -- * @} // endgroup LLVMCTLTO_CACHING
  --  

end LLVM.lto;

