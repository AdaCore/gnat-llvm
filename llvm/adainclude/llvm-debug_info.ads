pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;
with Interfaces.C.Strings;
with stddef_h;

package LLVM.Debug_Info is

  --===------------ DebugInfo.h - LLVM C API Debug Info API -----------------===//
  --                     The LLVM Compiler Infrastructure
  -- This file is distributed under the University of Illinois Open Source
  -- License. See LICENSE.TXT for details.
  --===----------------------------------------------------------------------===//
  --/
  --/ This file declares the C API endpoints for generating DWARF Debug Info
  --/
  --/ Note: This interface is experimental. It is *NOT* stable, and may be
  --/       changed without warning.
  --/
  --===----------------------------------------------------------------------===//
  --*
  -- * Debug info flags.
  --  

   subtype DI_Flags_T is unsigned;
   DI_Flag_Zero : constant DI_Flags_T := 0;
   DI_Flag_Private : constant DI_Flags_T := 1;
   DI_Flag_Protected : constant DI_Flags_T := 2;
   DI_Flag_Public : constant DI_Flags_T := 3;
   DI_Flag_Fwd_Decl : constant DI_Flags_T := 4;
   DI_Flag_Apple_Block : constant DI_Flags_T := 8;
   DI_Flag_Block_Byref_Struct : constant DI_Flags_T := 16;
   DI_Flag_Virtual : constant DI_Flags_T := 32;
   DI_Flag_Artificial : constant DI_Flags_T := 64;
   DI_Flag_Explicit : constant DI_Flags_T := 128;
   DI_Flag_Prototyped : constant DI_Flags_T := 256;
   DI_Flag_Objc_Class_Complete : constant DI_Flags_T := 512;
   DI_Flag_Object_Pointer : constant DI_Flags_T := 1024;
   DI_Flag_Vector : constant DI_Flags_T := 2048;
   DI_Flag_Static_Member : constant DI_Flags_T := 4096;
   DI_Flag_L_Value_Reference : constant DI_Flags_T := 8192;
   DI_Flag_R_Value_Reference : constant DI_Flags_T := 16384;
   DI_Flag_Reserved : constant DI_Flags_T := 32768;
   DI_Flag_Single_Inheritance : constant DI_Flags_T := 65536;
   DI_Flag_Multiple_Inheritance : constant DI_Flags_T := 131072;
   DI_Flag_Virtual_Inheritance : constant DI_Flags_T := 196608;
   DI_Flag_Introduced_Virtual : constant DI_Flags_T := 262144;
   DI_Flag_Bit_Field : constant DI_Flags_T := 524288;
   DI_Flag_No_Return : constant DI_Flags_T := 1048576;
   DI_Flag_Main_Subprogram : constant DI_Flags_T := 2097152;
   DI_Flag_Indirect_Virtual_Base : constant DI_Flags_T := 36;
   DI_Flag_Accessibility : constant DI_Flags_T := 3;
   DI_Flag_Ptr_To_Member_Rep : constant DI_Flags_T := 196608;  -- llvm-6.0.0.src/include/llvm-c/DebugInfo.h:61

  --*
  -- * Source languages known by DWARF.
  --  

  -- New in DWARF v3:
  -- New in DWARF v4:
  -- New in DWARF v5:
  -- Vendor extensions:
   type DWARF_Source_Language_T is 
     (DWARF_Source_Language_C89,
      DWARF_Source_Language_C,
      DWARF_Source_Language_Ada83,
      Dwarfsourcelanguagec_Plus_Plus,
      DWARF_Source_Language_Cobol74,
      DWARF_Source_Language_Cobol85,
      DWARF_Source_Language_Fortran77,
      DWARF_Source_Language_Fortran90,
      DWARF_Source_Language_Pascal83,
      DWARF_Source_Language_Modula2,
      DWARF_Source_Language_Java,
      DWARF_Source_Language_C99,
      DWARF_Source_Language_Ada95,
      DWARF_Source_Language_Fortran95,
      DWARF_Source_Language_PLI,
      DWARF_Source_Language_Obj_C,
      Dwarfsourcelanguageobjc_Plus_Plus,
      DWARF_Source_Language_UPC,
      DWARF_Source_Language_D,
      DWARF_Source_Language_Python,
      DWARF_Source_Language_Open_CL,
      DWARF_Source_Language_Go,
      DWARF_Source_Language_Modula3,
      DWARF_Source_Language_Haskell,
      Dwarfsourcelanguagec_Plus_Plus_03,
      Dwarfsourcelanguagec_Plus_Plus_11,
      DWARF_Source_Language_O_Caml,
      DWARF_Source_Language_Rust,
      DWARF_Source_Language_C11,
      DWARF_Source_Language_Swift,
      DWARF_Source_Language_Julia,
      DWARF_Source_Language_Dylan,
      Dwarfsourcelanguagec_Plus_Plus_14,
      DWARF_Source_Language_Fortran03,
      DWARF_Source_Language_Fortran08,
      DWARF_Source_Language_Render_Script,
      DWARF_Source_Language_BLISS,
      Dwarfsourcelanguagemips_Assembler,
      Dwarfsourcelanguagegoogle_Renderscript,
      Dwarfsourcelanguageborland_Delphi);
   pragma Convention (C, DWARF_Source_Language_T);  -- llvm-6.0.0.src/include/llvm-c/DebugInfo.h:111

  --*
  -- * The amount of debug information to emit.
  --  

   type DWARF_Emission_Kind_T is 
     (DWARF_Emission_None,
      DWARF_Emission_Full,
      DWARF_Emission_Line_Tables_Only);
   pragma Convention (C, DWARF_Emission_Kind_T);  -- llvm-6.0.0.src/include/llvm-c/DebugInfo.h:120

  --*
  -- * The current debug metadata version number.
  --  

   function Debug_Metadata_Version return unsigned;  -- llvm-6.0.0.src/include/llvm-c/DebugInfo.h:125
   pragma Import (C, Debug_Metadata_Version, "LLVMDebugMetadataVersion");

  --*
  -- * The version of debug metadata that's present in the provided \c Module.
  --  

   function Get_Module_Debug_Metadata_Version (Module : LLVM.Types.Module_T) return unsigned;  -- llvm-6.0.0.src/include/llvm-c/DebugInfo.h:130
   pragma Import (C, Get_Module_Debug_Metadata_Version, "LLVMGetModuleDebugMetadataVersion");

  --*
  -- * Strip debug info in the module if it exists.
  -- * To do this, we remove all calls to the debugger intrinsics and any named
  -- * metadata for debugging. We also remove debug locations for instructions.
  -- * Return true if module is modified.
  --  

   function Strip_Module_Debug_Info
     (Module : LLVM.Types.Module_T)
      return Boolean;
   function Strip_Module_Debug_Info_C
     (Module : LLVM.Types.Module_T)
      return LLVM.Types.Bool_T;  -- llvm-6.0.0.src/include/llvm-c/DebugInfo.h:138
   pragma Import (C, Strip_Module_Debug_Info_C, "LLVMStripModuleDebugInfo");

  --*
  -- * Construct a builder for a module, and do not allow for unresolved nodes
  -- * attached to the module.
  --  

   function Create_DI_Builder_Disallow_Unresolved (M : LLVM.Types.Module_T) return LLVM.Types.DI_Builder_T;  -- llvm-6.0.0.src/include/llvm-c/DebugInfo.h:144
   pragma Import (C, Create_DI_Builder_Disallow_Unresolved, "LLVMCreateDIBuilderDisallowUnresolved");

  --*
  -- * Construct a builder for a module and collect unresolved nodes attached
  -- * to the module in order to resolve cycles during a call to
  -- * \c LLVMDIBuilderFinalize.
  --  

   function Create_DI_Builder (M : LLVM.Types.Module_T) return LLVM.Types.DI_Builder_T;  -- llvm-6.0.0.src/include/llvm-c/DebugInfo.h:151
   pragma Import (C, Create_DI_Builder, "LLVMCreateDIBuilder");

  --*
  -- * Deallocates the \c DIBuilder and everything it owns.
  -- * @note You must call \c LLVMDIBuilderFinalize before this
  --  

   procedure Dispose_DI_Builder (Builder : LLVM.Types.DI_Builder_T);  -- llvm-6.0.0.src/include/llvm-c/DebugInfo.h:157
   pragma Import (C, Dispose_DI_Builder, "LLVMDisposeDIBuilder");

  --*
  -- * Construct any deferred debug info descriptors.
  --  

   procedure DI_Builder_Finalize (Builder : LLVM.Types.DI_Builder_T);  -- llvm-6.0.0.src/include/llvm-c/DebugInfo.h:162
   pragma Import (C, DI_Builder_Finalize, "LLVMDIBuilderFinalize");

  --*
  -- * A CompileUnit provides an anchor for all debugging
  -- * information generated during this instance of compilation.
  -- * \param Lang          Source programming language, eg.
  -- *                      \c LLVMDWARFSourceLanguageC99
  -- * \param FileRef       File info.
  -- * \param Producer      Identify the producer of debugging information
  -- *                      and code.  Usually this is a compiler
  -- *                      version string.
  -- * \param ProducerLen   The length of the C string passed to \c Producer.
  -- * \param isOptimized   A boolean flag which indicates whether optimization
  -- *                      is enabled or not.
  -- * \param Flags         This string lists command line options. This
  -- *                      string is directly embedded in debug info
  -- *                      output which may be used by a tool
  -- *                      analyzing generated debugging information.
  -- * \param FlagsLen      The length of the C string passed to \c Flags.
  -- * \param RuntimeVer    This indicates runtime version for languages like
  -- *                      Objective-C.
  -- * \param SplitName     The name of the file that we'll split debug info
  -- *                      out into.
  -- * \param SplitNameLen  The length of the C string passed to \c SplitName.
  -- * \param Kind          The kind of debug information to generate.
  -- * \param DWOId         The DWOId if this is a split skeleton compile unit.
  -- * \param SplitDebugInlining    Whether to emit inline debug info.
  -- * \param DebugInfoForProfiling Whether to emit extra debug info for
  -- *                              profile collection.
  --  

function DI_Create_Compile_Unit
     (Builder                  : LLVM.Types.DI_Builder_T;
      Lang                     : DWARF_Source_Language_T;
      File_Ref                 : LLVM.Types.Metadata_T;
      Producer                 : String;
      Producer_Len             : stddef_h.size_t;
      is_Optimized             : Boolean;
      Flags                    : String;
      Flags_Len                : stddef_h.size_t;
      Runtime_Ver              : unsigned;
      Split_Name               : String;
      Split_Name_Len           : stddef_h.size_t;
      Kind                     : DWARF_Emission_Kind_T;
      DWO_Id                   : unsigned;
      Split_Debug_Inlining     : Boolean;
      Debug_Info_For_Profiling : Boolean)
      return LLVM.Types.Metadata_T;
   function DI_Builder_Create_Compile_Unit_C
     (Builder                  : LLVM.Types.DI_Builder_T;
      Lang                     : DWARF_Source_Language_T;
      File_Ref                 : LLVM.Types.Metadata_T;
      Producer                 : Interfaces.C.Strings.chars_ptr;
      Producer_Len             : stddef_h.size_t;
      is_Optimized             : LLVM.Types.Bool_T;
      Flags                    : Interfaces.C.Strings.chars_ptr;
      Flags_Len                : stddef_h.size_t;
      Runtime_Ver              : unsigned;
      Split_Name               : Interfaces.C.Strings.chars_ptr;
      Split_Name_Len           : stddef_h.size_t;
      Kind                     : DWARF_Emission_Kind_T;
      DWO_Id                   : unsigned;
      Split_Debug_Inlining     : LLVM.Types.Bool_T;
      Debug_Info_For_Profiling : LLVM.Types.Bool_T)
      return LLVM.Types.Metadata_T;
   pragma Import (C, DI_Builder_Create_Compile_Unit_C, "LLVMDIBuilderCreateCompileUnit");

  --*
  -- * Create a file descriptor to hold debugging information for a file.
  -- * \param Builder      The \c DIBuilder.
  -- * \param Filename     File name.
  -- * \param FilenameLen  The length of the C string passed to \c Filename.
  -- * \param Directory    Directory.
  -- * \param DirectoryLen The length of the C string passed to \c Directory.
  --  

function DI_Create_File
     (Builder       : LLVM.Types.DI_Builder_T;
      Filename      : String;
      Filename_Len  : stddef_h.size_t;
      Directory     : String;
      Directory_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T;
   function DI_Builder_Create_File_C
     (Builder       : LLVM.Types.DI_Builder_T;
      Filename      : Interfaces.C.Strings.chars_ptr;
      Filename_Len  : stddef_h.size_t;
      Directory     : Interfaces.C.Strings.chars_ptr;
      Directory_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T;
   pragma Import (C, DI_Builder_Create_File_C, "LLVMDIBuilderCreateFile");

  --*
  -- * Creates a new DebugLocation that describes a source location.
  -- * \param Line The line in the source file.
  -- * \param Column The column in the source file.
  -- * \param Scope The scope in which the location resides.
  -- * \param InlinedAt The scope where this location was inlined, if at all.
  -- *                  (optional).
  -- * \note If the item to which this location is attached cannot be
  -- *       attributed to a source line, pass 0 for the line and column.
  --  

   function DI_Builder_Create_Debug_Location
     (Ctx : LLVM.Types.Context_T;
      Line : unsigned;
      Column : unsigned;
      Scope : LLVM.Types.Metadata_T;
      Inlined_At : LLVM.Types.Metadata_T) return LLVM.Types.Metadata_T;  -- llvm-6.0.0.src/include/llvm-c/DebugInfo.h:224
   pragma Import (C, DI_Builder_Create_Debug_Location, "LLVMDIBuilderCreateDebugLocation");

  -- end extern "C"  
end LLVM.Debug_Info;

