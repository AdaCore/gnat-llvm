pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;
with stddef_h;

package LLVM.Lto is

   LTO_API_VERSION : constant := 29;  --  install/include/llvm-c/lto.h:49

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

   subtype Bool_T_T is Extensions.bool;  -- install/include/llvm-c/lto.h:31

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
   SYMBOL_ALIAS : constant Symbol_Attributes_T := 32768;  -- install/include/llvm-c/lto.h:74

  --*
  -- * \since prior to LTO_API_VERSION=3
  --  

   type Debug_Model_T is 
     (DEBUG_MODEL_NONE,
      DEBUG_MODEL_DWARF)
   with Convention => C;  -- install/include/llvm-c/lto.h:82

  --*
  -- * \since prior to LTO_API_VERSION=3
  --  

   type Codegen_Model_T is 
     (CODEGEN_PIC_MODEL_STATIC,
      CODEGEN_PIC_MODEL_DYNAMIC,
      CODEGEN_PIC_MODEL_DYNAMIC_NO_PIC,
      CODEGEN_PIC_MODEL_DEFAULT)
   with Convention => C;  -- install/include/llvm-c/lto.h:92

  --* opaque reference to a loaded object module  
   type LLVMOpaqueLTOModule is null record;   -- incomplete struct

   type Module_T_T is access all LLVMOpaqueLTOModule;  -- install/include/llvm-c/lto.h:95

  --* opaque reference to a code generator  
   type LLVMOpaqueLTOCodeGenerator is null record;   -- incomplete struct

   type Code_Gen_T_T is access all LLVMOpaqueLTOCodeGenerator;  -- install/include/llvm-c/lto.h:98

  --* opaque reference to a thin code generator  
   type LLVMOpaqueThinLTOCodeGenerator is null record;   -- incomplete struct

   type thinlto_code_gen_t is access all LLVMOpaqueThinLTOCodeGenerator;  -- install/include/llvm-c/lto.h:101

  --*
  -- * Returns a printable string.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

function Get_Version
      return String;

  --*
  -- * Returns the last error string or NULL if last operation was successful.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

function Get_Error_Message
      return String;

  --*
  -- * Checks if a file is a loadable object file.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

function Module_Is_Object_File
     (Path : String)
      return Bool_T_T;

  --*
  -- * Checks if a file is a loadable object compiled for requested target.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

function Module_Is_Object_File_For_Target
     (Path                 : String;
      Target_Triple_Prefix : String)
      return Bool_T_T;

  --*
  -- * Return true if \p Buffer contains a bitcode file with ObjC code (category
  -- * or class) in it.
  -- *
  -- * \since LTO_API_VERSION=20
  --  

   function Module_Has_Objc_Category (Mem : System.Address; Length : stddef_h.size_t) return Bool_T_T  -- install/include/llvm-c/lto.h:145
   with Import => True, 
        Convention => C, 
        External_Name => "lto_module_has_objc_category";

  --*
  -- * Checks if a buffer is a loadable object file.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Module_Is_Object_File_In_Memory (Mem : System.Address; Length : stddef_h.size_t) return Bool_T_T  -- install/include/llvm-c/lto.h:152
   with Import => True, 
        Convention => C, 
        External_Name => "lto_module_is_object_file_in_memory";

  --*
  -- * Checks if a buffer is a loadable object compiled for requested target.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

function Module_Is_Object_File_In_Memory_For_Target
     (Mem                  : System.Address;
      Length               : stddef_h.size_t;
      Target_Triple_Prefix : String)
      return Bool_T_T;

  --*
  -- * Loads an object file from disk.
  -- * Returns NULL on error (check lto_get_error_message() for details).
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

function Module_Create
     (Path : String)
      return Module_T_T;

  --*
  -- * Loads an object file from memory.
  -- * Returns NULL on error (check lto_get_error_message() for details).
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Module_Create_From_Memory (Mem : System.Address; Length : stddef_h.size_t) return Module_T_T  -- install/include/llvm-c/lto.h:180
   with Import => True, 
        Convention => C, 
        External_Name => "lto_module_create_from_memory";

  --*
  -- * Loads an object file from memory with an extra path argument.
  -- * Returns NULL on error (check lto_get_error_message() for details).
  -- *
  -- * \since LTO_API_VERSION=9
  --  

function Module_Create_From_Memory_With_Path
     (Mem    : System.Address;
      Length : stddef_h.size_t;
      Path   : String)
      return Module_T_T;

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
     (Mem    : System.Address;
      Length : stddef_h.size_t;
      Path   : String)
      return Module_T_T;

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
     (Mem    : System.Address;
      Length : stddef_h.size_t;
      Path   : String;
      Cg     : Code_Gen_T_T)
      return Module_T_T;

  --*
  -- * Loads an object file from disk. The seek point of fd is not preserved.
  -- * Returns NULL on error (check lto_get_error_message() for details).
  -- *
  -- * \since LTO_API_VERSION=5
  --  

function Module_Create_From_Fd
     (Fd        : int;
      Path      : String;
      File_Size : stddef_h.size_t)
      return Module_T_T;

  --*
  -- * Loads an object file from disk. The seek point of fd is not preserved.
  -- * Returns NULL on error (check lto_get_error_message() for details).
  -- *
  -- * \since LTO_API_VERSION=5
  --  

function Module_Create_From_Fd_At_Offset
     (Fd        : int;
      Path      : String;
      File_Size : stddef_h.size_t;
      Map_Size  : stddef_h.size_t;
      Offset    : stddef_h.off_t)
      return Module_T_T;

  --*
  -- * Frees all memory internally allocated by the module.
  -- * Upon return the lto_module_t is no longer valid.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   procedure Module_Dispose (C_Mod : Module_T_T)  -- install/include/llvm-c/lto.h:247
   with Import => True, 
        Convention => C, 
        External_Name => "lto_module_dispose";

  --*
  -- * Returns triple string which the object module was compiled under.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

function Module_Get_Target_Triple
     (C_Mod : Module_T_T)
      return String;

  --*
  -- * Sets triple string with which the object will be codegened.
  -- *
  -- * \since LTO_API_VERSION=4
  --  

procedure Module_Set_Target_Triple
     (C_Mod  : Module_T_T;
      Triple : String);

  --*
  -- * Returns the number of symbols in the object module.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Module_Get_Num_Symbols (C_Mod : Module_T_T) return unsigned  -- install/include/llvm-c/lto.h:271
   with Import => True, 
        Convention => C, 
        External_Name => "lto_module_get_num_symbols";

  --*
  -- * Returns the name of the ith symbol in the object module.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

function Module_Get_Symbol_Name
     (C_Mod : Module_T_T;
      Index : unsigned)
      return String;

  --*
  -- * Returns the attributes of the ith symbol in the object module.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Module_Get_Symbol_Attribute (C_Mod : Module_T_T; Index : unsigned) return Symbol_Attributes_T  -- install/include/llvm-c/lto.h:287
   with Import => True, 
        Convention => C, 
        External_Name => "lto_module_get_symbol_attribute";

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

  --*
  -- * If targeting mach-o on darwin, this function gets the CPU type and subtype
  -- * that will end up being encoded in the mach-o header. These are the values
  -- * that can be found in mach/machine.h.
  -- *
  -- * \p out_cputype and \p out_cpusubtype must be non-NULL.
  -- *
  -- * Returns true on error (check lto_get_error_message() for details).
  -- *
  -- * \since LTO_API_VERSION=27
  --  

   function Module_Get_Macho_Cputype
     (C_Mod : Module_T_T;
      Out_Cputype : access unsigned;
      Out_Cpusubtype : access unsigned) return Bool_T_T  -- install/include/llvm-c/lto.h:311
   with Import => True, 
        Convention => C, 
        External_Name => "lto_module_get_macho_cputype";

  --*
  -- * This function can be used by the linker to check if a given module has
  -- * any constructor or destructor functions.
  -- *
  -- * Returns true if the module has either the @llvm.global_ctors or the
  -- * @llvm.global_dtors symbol. Otherwise returns false.
  -- *
  -- * \since LTO_API_VERSION=29
  --  

   function Module_Has_Ctor_Dtor (C_Mod : Module_T_T) return Bool_T_T  -- install/include/llvm-c/lto.h:324
   with Import => True, 
        Convention => C, 
        External_Name => "lto_module_has_ctor_dtor";

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
   DS_NOTE : constant Codegen_Diagnostic_Severity_T_T := 2;  -- install/include/llvm-c/lto.h:335

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
        (Arg_1 : Codegen_Diagnostic_Severity_T_T;
         Arg_2 : Interfaces.C.Strings.chars_ptr;
         Arg_3 : System.Address)
   with Convention => C;  -- install/include/llvm-c/lto.h:346

  --*
  -- * Set a diagnostic handler and the related context (void *).
  -- * This is more general than lto_get_error_message, as the diagnostic handler
  -- * can be called at anytime within lto.
  -- *
  -- * \since LTO_API_VERSION=7
  --  

   procedure Codegen_Set_Diagnostic_Handler
     (Arg_1 : Code_Gen_T_T;
      Arg_2 : Diagnostic_Handler_T_T;
      Arg_3 : System.Address)  -- install/include/llvm-c/lto.h:356
   with Import => True, 
        Convention => C, 
        External_Name => "lto_codegen_set_diagnostic_handler";

  --*
  -- * Instantiates a code generator.
  -- * Returns NULL on error (check lto_get_error_message() for details).
  -- *
  -- * All modules added using \a lto_codegen_add_module() must have been created
  -- * in the same context as the codegen.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Codegen_Create return Code_Gen_T_T  -- install/include/llvm-c/lto.h:370
   with Import => True, 
        Convention => C, 
        External_Name => "lto_codegen_create";

  --*
  -- * Instantiate a code generator in its own context.
  -- *
  -- * Instantiates a code generator in its own context.  Modules added via \a
  -- * lto_codegen_add_module() must have all been created in the same context,
  -- * using \a lto_module_create_in_codegen_context().
  -- *
  -- * \since LTO_API_VERSION=11
  --  

   function Codegen_Create_In_Local_Context return Code_Gen_T_T  -- install/include/llvm-c/lto.h:382
   with Import => True, 
        Convention => C, 
        External_Name => "lto_codegen_create_in_local_context";

  --*
  -- * Frees all code generator and all memory it internally allocated.
  -- * Upon return the lto_code_gen_t is no longer valid.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   procedure Codegen_Dispose (Arg_1 : Code_Gen_T_T)  -- install/include/llvm-c/lto.h:391
   with Import => True, 
        Convention => C, 
        External_Name => "lto_codegen_dispose";

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

   function Codegen_Add_Module (Cg : Code_Gen_T_T; C_Mod : Module_T_T) return Bool_T_T  -- install/include/llvm-c/lto.h:404
   with Import => True, 
        Convention => C, 
        External_Name => "lto_codegen_add_module";

  --*
  -- * Sets the object module for code generation. This will transfer the ownership
  -- * of the module to the code generator.
  -- *
  -- * \c cg and \c mod must both be in the same context.
  -- *
  -- * \since LTO_API_VERSION=13
  --  

   procedure Codegen_Set_Module (Cg : Code_Gen_T_T; C_Mod : Module_T_T)  -- install/include/llvm-c/lto.h:415
   with Import => True, 
        Convention => C, 
        External_Name => "lto_codegen_set_module";

  --*
  -- * Sets if debug info should be generated.
  -- * Returns true on error (check lto_get_error_message() for details).
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Codegen_Set_Debug_Model (Cg : Code_Gen_T_T; Arg_2 : Debug_Model_T) return Bool_T_T  -- install/include/llvm-c/lto.h:424
   with Import => True, 
        Convention => C, 
        External_Name => "lto_codegen_set_debug_model";

  --*
  -- * Sets which PIC code model to generated.
  -- * Returns true on error (check lto_get_error_message() for details).
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

   function Codegen_Set_Pic_Model (Cg : Code_Gen_T_T; Arg_2 : Codegen_Model_T) return Bool_T_T  -- install/include/llvm-c/lto.h:433
   with Import => True, 
        Convention => C, 
        External_Name => "lto_codegen_set_pic_model";

  --*
  -- * Sets the cpu to generate code for.
  -- *
  -- * \since LTO_API_VERSION=4
  --  

procedure Codegen_Set_Cpu
     (Cg  : Code_Gen_T_T;
      Cpu : String);

  --*
  -- * Sets the location of the assembler tool to run. If not set, libLTO
  -- * will use gcc to invoke the assembler.
  -- *
  -- * \since LTO_API_VERSION=3
  --  

procedure Codegen_Set_Assembler_Path
     (Cg   : Code_Gen_T_T;
      Path : String);

  --*
  -- * Sets extra arguments that libLTO should pass to the assembler.
  -- *
  -- * \since LTO_API_VERSION=4
  --  

   procedure Codegen_Set_Assembler_Args
     (Cg : Code_Gen_T_T;
      Args : System.Address;
      Nargs : int)  -- install/include/llvm-c/lto.h:458
   with Import => True, 
        Convention => C, 
        External_Name => "lto_codegen_set_assembler_args";

  --*
  -- * Adds to a list of all global symbols that must exist in the final generated
  -- * code. If a function is not listed there, it might be inlined into every usage
  -- * and optimized away.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

procedure Codegen_Add_Must_Preserve_Symbol
     (Cg     : Code_Gen_T_T;
      Symbol : String);

  --*
  -- * Writes a new object file at the specified path that contains the
  -- * merged contents of all modules added so far.
  -- * Returns true on error (check lto_get_error_message() for details).
  -- *
  -- * \since LTO_API_VERSION=5
  --  

function Codegen_Write_Merged_Modules
     (Cg   : Code_Gen_T_T;
      Path : String)
      return Bool_T_T;

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

   function Codegen_Compile (Cg : Code_Gen_T_T; Length : access stddef_h.size_t) return System.Address  -- install/include/llvm-c/lto.h:494
   with Import => True, 
        Convention => C, 
        External_Name => "lto_codegen_compile";

  --*
  -- * Generates code for all added modules into one native object file.
  -- * This calls lto_codegen_optimize then lto_codegen_compile_optimized (instead
  -- * of returning a generated mach-o/ELF buffer, it writes to a file).
  -- *
  -- * The name of the file is written to name. Returns true on error.
  -- *
  -- * \since LTO_API_VERSION=5
  --  

   function Codegen_Compile_To_File (Cg : Code_Gen_T_T; Name : System.Address) return Bool_T_T  -- install/include/llvm-c/lto.h:506
   with Import => True, 
        Convention => C, 
        External_Name => "lto_codegen_compile_to_file";

  --*
  -- * Runs optimization for the merged module. Returns true on error.
  -- *
  -- * \since LTO_API_VERSION=12
  --  

   function Codegen_Optimize (Cg : Code_Gen_T_T) return Bool_T_T  -- install/include/llvm-c/lto.h:514
   with Import => True, 
        Convention => C, 
        External_Name => "lto_codegen_optimize";

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

   function Codegen_Compile_Optimized (Cg : Code_Gen_T_T; Length : access stddef_h.size_t) return System.Address  -- install/include/llvm-c/lto.h:529
   with Import => True, 
        Convention => C, 
        External_Name => "lto_codegen_compile_optimized";

  --*
  -- * Returns the runtime API version.
  -- *
  -- * \since LTO_API_VERSION=12
  --  

   function Api_Version_Fun return unsigned  -- install/include/llvm-c/lto.h:537
   with Import => True, 
        Convention => C, 
        External_Name => "lto_api_version_fun";

  --*
  -- * Parses options immediately, making them available as early as possible. For
  -- * example during executing codegen::InitTargetOptionsFromCodeGenFlags. Since
  -- * parsing shud only happen once, only one of lto_codegen_debug_options or
  -- * lto_set_debug_options should be called.
  -- *
  -- * This function takes one or more options separated by spaces.
  -- * Warning: passing file paths through this function may confuse the argument
  -- * parser if the paths contain spaces.
  -- *
  -- * \since LTO_API_VERSION=28
  --  

   procedure Set_Debug_Options (Options : System.Address; Number : int)  -- install/include/llvm-c/lto.h:551
   with Import => True, 
        Convention => C, 
        External_Name => "lto_set_debug_options";

  --*
  -- * Sets options to help debug codegen bugs. Since parsing shud only happen once,
  -- * only one of lto_codegen_debug_options or lto_set_debug_options
  -- * should be called.
  -- *
  -- * This function takes one or more options separated by spaces.
  -- * Warning: passing file paths through this function may confuse the argument
  -- * parser if the paths contain spaces.
  -- *
  -- * \since prior to LTO_API_VERSION=3
  --  

procedure Codegen_Debug_Options
     (Cg    : Code_Gen_T_T;
      Arg_2 : String);

  --*
  -- * Same as the previous function, but takes every option separately through an
  -- * array.
  -- *
  -- * \since prior to LTO_API_VERSION=26
  --  

   procedure Codegen_Debug_Options_Array
     (Cg : Code_Gen_T_T;
      Arg_2 : System.Address;
      Number : int)  -- install/include/llvm-c/lto.h:573
   with Import => True, 
        Convention => C, 
        External_Name => "lto_codegen_debug_options_array";

  --*
  -- * Initializes LLVM disassemblers.
  -- * FIXME: This doesn't really belong here.
  -- *
  -- * \since LTO_API_VERSION=5
  --  

   procedure Initialize_Disassembler  -- install/include/llvm-c/lto.h:583
   with Import => True, 
        Convention => C, 
        External_Name => "lto_initialize_disassembler";

  --*
  -- * Sets if we should run internalize pass during optimization and code
  -- * generation.
  -- *
  -- * \since LTO_API_VERSION=14
  --  

   procedure Codegen_Set_Should_Internalize (Cg : Code_Gen_T_T; Should_Internalize : Bool_T_T)  -- install/include/llvm-c/lto.h:592
   with Import => True, 
        Convention => C, 
        External_Name => "lto_codegen_set_should_internalize";

  --*
  -- * Set whether to embed uselists in bitcode.
  -- *
  -- * Sets whether \a lto_codegen_write_merged_modules() should embed uselists in
  -- * output bitcode.  This should be turned on for all -save-temps output.
  -- *
  -- * \since LTO_API_VERSION=15
  --  

   procedure Codegen_Set_Should_Embed_Uselists (Cg : Code_Gen_T_T; Should_Embed_Uselists : Bool_T_T)  -- install/include/llvm-c/lto.h:604
   with Import => True, 
        Convention => C, 
        External_Name => "lto_codegen_set_should_embed_uselists";

  --* Opaque reference to an LTO input file  
   type LLVMOpaqueLTOInput is null record;   -- incomplete struct

   type Input_T_T is access all LLVMOpaqueLTOInput;  -- install/include/llvm-c/lto.h:608

  --*
  --  * Creates an LTO input file from a buffer. The path
  --  * argument is used for diagnotics as this function
  --  * otherwise does not know which file the given buffer
  --  * is associated with.
  --  *
  --  * \since LTO_API_VERSION=24
  --   

function Input_Create
     (Buffer      : System.Address;
      Buffer_Size : stddef_h.size_t;
      Path        : String)
      return Input_T_T;

  --*
  --  * Frees all memory internally allocated by the LTO input file.
  --  * Upon return the lto_module_t is no longer valid.
  --  *
  --  * \since LTO_API_VERSION=24
  --   

   procedure Input_Dispose (Input : Input_T_T)  -- install/include/llvm-c/lto.h:628
   with Import => True, 
        Convention => C, 
        External_Name => "lto_input_dispose";

  --*
  --  * Returns the number of dependent library specifiers
  --  * for the given LTO input file.
  --  *
  --  * \since LTO_API_VERSION=24
  --   

   function Input_Get_Num_Dependent_Libraries (Input : Input_T_T) return unsigned  -- install/include/llvm-c/lto.h:636
   with Import => True, 
        Convention => C, 
        External_Name => "lto_input_get_num_dependent_libraries";

  --*
  --  * Returns the ith dependent library specifier
  --  * for the given LTO input file. The returned
  --  * string is not null-terminated.
  --  *
  --  * \since LTO_API_VERSION=24
  --   

function Input_Get_Dependent_Library
     (Input : Input_T_T;
      Index : stddef_h.size_t;
      Size  : access stddef_h.size_t)
      return String;

  --*
  -- * Returns the list of libcall symbols that can be generated by LTO
  -- * that might not be visible from the symbol table of bitcode files.
  -- *
  -- * \since prior to LTO_API_VERSION=25
  --  

   function Runtime_Lib_Symbols_List (Size : access stddef_h.size_t) return System.Address  -- install/include/llvm-c/lto.h:655
   with Import => True, 
        Convention => C, 
        External_Name => "lto_runtime_lib_symbols_list";

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

   type Object_Buffer_T is record
      Buffer : Interfaces.C.Strings.chars_ptr;  -- install/include/llvm-c/lto.h:671
      Size : aliased stddef_h.size_t;  -- install/include/llvm-c/lto.h:672
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/llvm-c/lto.h:673

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

   function thinlto_create_codegen return thinlto_code_gen_t  -- install/include/llvm-c/lto.h:687
   with Import => True, 
        Convention => C, 
        External_Name => "thinlto_create_codegen";

  --*
  -- * Frees the generator and all memory it internally allocated.
  -- * Upon return the thinlto_code_gen_t is no longer valid.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   procedure thinlto_codegen_dispose (Cg : thinlto_code_gen_t)  -- install/include/llvm-c/lto.h:695
   with Import => True, 
        Convention => C, 
        External_Name => "thinlto_codegen_dispose";

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
     (Cg         : thinlto_code_gen_t;
      Identifier : String;
      Data       : String;
      Length     : int);

  --*
  -- * Optimize and codegen all the modules added to the codegenerator using
  -- * ThinLTO. Resulting objects are accessible using thinlto_module_get_object().
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   procedure thinlto_codegen_process (Cg : thinlto_code_gen_t)  -- install/include/llvm-c/lto.h:718
   with Import => True, 
        Convention => C, 
        External_Name => "thinlto_codegen_process";

  --*
  -- * Returns the number of object files produced by the ThinLTO CodeGenerator.
  -- *
  -- * It usually matches the number of input files, but this is not a guarantee of
  -- * the API and may change in future implementation, so the client should not
  -- * assume it.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   function thinlto_module_get_num_objects (Cg : thinlto_code_gen_t) return unsigned  -- install/include/llvm-c/lto.h:729
   with Import => True, 
        Convention => C, 
        External_Name => "thinlto_module_get_num_objects";

  --*
  -- * Returns a reference to the ith object file produced by the ThinLTO
  -- * CodeGenerator.
  -- *
  -- * Client should use \p thinlto_module_get_num_objects() to get the number of
  -- * available objects.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   function thinlto_module_get_object (Cg : thinlto_code_gen_t; Index : unsigned) return Object_Buffer_T  -- install/include/llvm-c/lto.h:740
   with Import => True, 
        Convention => C, 
        External_Name => "thinlto_module_get_object";

  --*
  -- * Returns the number of object files produced by the ThinLTO CodeGenerator.
  -- *
  -- * It usually matches the number of input files, but this is not a guarantee of
  -- * the API and may change in future implementation, so the client should not
  -- * assume it.
  -- *
  -- * \since LTO_API_VERSION=21
  --  

   function thinlto_module_get_num_object_files (Cg : thinlto_code_gen_t) return unsigned  -- install/include/llvm-c/lto.h:752
   with Import => True, 
        Convention => C, 
        External_Name => "thinlto_module_get_num_object_files";

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
     (Cg    : thinlto_code_gen_t;
      Index : unsigned)
      return String;

  --*
  -- * Sets which PIC code model to generate.
  -- * Returns true on error (check lto_get_error_message() for details).
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   function thinlto_codegen_set_pic_model (Cg : thinlto_code_gen_t; Arg_2 : Codegen_Model_T) return Bool_T_T  -- install/include/llvm-c/lto.h:772
   with Import => True, 
        Convention => C, 
        External_Name => "thinlto_codegen_set_pic_model";

  --*
  -- * Sets the path to a directory to use as a storage for temporary bitcode files.
  -- * The intention is to make the bitcode files available for debugging at various
  -- * stage of the pipeline.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

procedure thinlto_codegen_set_savetemps_dir
     (Cg             : thinlto_code_gen_t;
      Save_Temps_Dir : String);

  --*
  -- * Set the path to a directory where to save generated object files. This
  -- * path can be used by a linker to request on-disk files instead of in-memory
  -- * buffers. When set, results are available through
  -- * thinlto_module_get_object_file() instead of thinlto_module_get_object().
  -- *
  -- * \since LTO_API_VERSION=21
  --  

procedure thinlto_set_generated_objects_dir
     (Cg             : thinlto_code_gen_t;
      Save_Temps_Dir : String);

  --*
  -- * Sets the cpu to generate code for.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

procedure thinlto_codegen_set_cpu
     (Cg  : thinlto_code_gen_t;
      Cpu : String);

  --*
  -- * Disable CodeGen, only run the stages till codegen and stop. The output will
  -- * be bitcode.
  -- *
  -- * \since LTO_API_VERSION=19
  --  

   procedure thinlto_codegen_disable_codegen (Cg : thinlto_code_gen_t; Disable : Bool_T_T)  -- install/include/llvm-c/lto.h:809
   with Import => True, 
        Convention => C, 
        External_Name => "thinlto_codegen_disable_codegen";

  --*
  -- * Perform CodeGen only: disable all other stages.
  -- *
  -- * \since LTO_API_VERSION=19
  --  

   procedure thinlto_codegen_set_codegen_only (Cg : thinlto_code_gen_t; Codegen_Only : Bool_T_T)  -- install/include/llvm-c/lto.h:817
   with Import => True, 
        Convention => C, 
        External_Name => "thinlto_codegen_set_codegen_only";

  --*
  -- * Parse -mllvm style debug options.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   procedure thinlto_debug_options (Options : System.Address; Number : int)  -- install/include/llvm-c/lto.h:825
   with Import => True, 
        Convention => C, 
        External_Name => "thinlto_debug_options";

  --*
  -- * Test if a module has support for ThinLTO linking.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   function Module_Is_Thinlto (C_Mod : Module_T_T) return Bool_T_T  -- install/include/llvm-c/lto.h:832
   with Import => True, 
        Convention => C, 
        External_Name => "lto_module_is_thinlto";

  --*
  -- * Adds a symbol to the list of global symbols that must exist in the final
  -- * generated code. If a function is not listed there, it might be inlined into
  -- * every usage and optimized away. For every single module, the functions
  -- * referenced from code outside of the ThinLTO modules need to be added here.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

procedure thinlto_codegen_add_must_preserve_symbol
     (Cg     : thinlto_code_gen_t;
      Name   : String;
      Length : int);

  --*
  -- * Adds a symbol to the list of global symbols that are cross-referenced between
  -- * ThinLTO files. If the ThinLTO CodeGenerator can ensure that every
  -- * references from a ThinLTO module to this symbol is optimized away, then
  -- * the symbol can be discarded.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

procedure thinlto_codegen_add_cross_referenced_symbol
     (Cg     : thinlto_code_gen_t;
      Name   : String;
      Length : int);

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
     (Cg        : thinlto_code_gen_t;
      Cache_Dir : String);

  --*
  -- * Sets the cache pruning interval (in seconds). A negative value disables the
  -- * pruning. An unspecified default value will be applied, and a value of 0 will
  -- * force prunning to occur.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   procedure thinlto_codegen_set_cache_pruning_interval (Cg : thinlto_code_gen_t; Interval : int)  -- install/include/llvm-c/lto.h:895
   with Import => True, 
        Convention => C, 
        External_Name => "thinlto_codegen_set_cache_pruning_interval";

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

   procedure thinlto_codegen_set_final_cache_size_relative_to_available_space (Cg : thinlto_code_gen_t; Percentage : unsigned)  -- install/include/llvm-c/lto.h:911
   with Import => True, 
        Convention => C, 
        External_Name => "thinlto_codegen_set_final_cache_size_relative_to_available_space";

  --*
  -- * Sets the expiration (in seconds) for an entry in the cache. An unspecified
  -- * default value will be applied. A value of 0 will be ignored.
  -- *
  -- * \since LTO_API_VERSION=18
  --  

   procedure thinlto_codegen_set_cache_entry_expiration (Cg : thinlto_code_gen_t; Expiration : unsigned)  -- install/include/llvm-c/lto.h:920
   with Import => True, 
        Convention => C, 
        External_Name => "thinlto_codegen_set_cache_entry_expiration";

  --*
  -- * Sets the maximum size of the cache directory (in bytes). A value over the
  -- * amount of available space on the disk will be reduced to the amount of
  -- * available space. An unspecified default value will be applied. A value of 0
  -- * will be ignored.
  -- *
  -- * \since LTO_API_VERSION=22
  --  

   procedure thinlto_codegen_set_cache_size_bytes (Cg : thinlto_code_gen_t; Max_Size_Bytes : unsigned)  -- install/include/llvm-c/lto.h:931
   with Import => True, 
        Convention => C, 
        External_Name => "thinlto_codegen_set_cache_size_bytes";

  --*
  -- * Same as thinlto_codegen_set_cache_size_bytes, except the maximum size is in
  -- * megabytes (2^20 bytes).
  -- *
  -- * \since LTO_API_VERSION=23
  --  

   procedure thinlto_codegen_set_cache_size_megabytes (Cg : thinlto_code_gen_t; Max_Size_Megabytes : unsigned)  -- install/include/llvm-c/lto.h:941
   with Import => True, 
        Convention => C, 
        External_Name => "thinlto_codegen_set_cache_size_megabytes";

  --*
  -- * Sets the maximum number of files in the cache directory. An unspecified
  -- * default value will be applied. A value of 0 will be ignored.
  -- *
  -- * \since LTO_API_VERSION=22
  --  

   procedure thinlto_codegen_set_cache_size_files (Cg : thinlto_code_gen_t; Max_Size_Files : unsigned)  -- install/include/llvm-c/lto.h:950
   with Import => True, 
        Convention => C, 
        External_Name => "thinlto_codegen_set_cache_size_files";

  --*
  -- * @} // endgroup LLVMCTLTO_CACHING
  --  

end LLVM.Lto;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
