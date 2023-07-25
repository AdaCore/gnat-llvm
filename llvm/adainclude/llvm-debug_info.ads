pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;
with Interfaces.C.Strings;
with stddef_h;
with System;
with stdint_h;

package LLVM.Debug_Info is

  --===------------ DebugInfo.h - LLVM C API Debug Info API -----------------===//
  -- Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
  -- See https://llvm.org/LICENSE.txt for license information.
  -- SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
  --===----------------------------------------------------------------------===//
  --/
  --/ This file declares the C API endpoints for generating DWARF Debug Info
  --/
  --/ Note: This interface is experimental. It is *NOT* stable, and may be
  --/       changed without warning.
  --/
  --===----------------------------------------------------------------------===//
  --*
  -- * @defgroup LLVMCCoreDebugInfo Debug Information
  -- * @ingroup LLVMCCore
  -- *
  -- * @{
  --  

  --*
  -- * Debug info flags.
  --  

  -- Deprecated.
   subtype DI_Flags_T is unsigned;
   DI_Flag_Zero : constant DI_Flags_T := 0;
   DI_Flag_Private : constant DI_Flags_T := 1;
   DI_Flag_Protected : constant DI_Flags_T := 2;
   DI_Flag_Public : constant DI_Flags_T := 3;
   DI_Flag_Fwd_Decl : constant DI_Flags_T := 4;
   DI_Flag_Apple_Block : constant DI_Flags_T := 8;
   DI_Flag_Reserved_Bit_4 : constant DI_Flags_T := 16;
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
   DI_Flag_Type_Pass_By_Value : constant DI_Flags_T := 4194304;
   DI_Flag_Type_Pass_By_Reference : constant DI_Flags_T := 8388608;
   DI_Flag_Enum_Class : constant DI_Flags_T := 16777216;
   DI_Flag_Fixed_Enum : constant DI_Flags_T := 16777216;
   DI_Flag_Thunk : constant DI_Flags_T := 33554432;
   DI_Flag_Non_Trivial : constant DI_Flags_T := 67108864;
   DI_Flag_Big_Endian : constant DI_Flags_T := 134217728;
   DI_Flag_Little_Endian : constant DI_Flags_T := 268435456;
   DI_Flag_Indirect_Virtual_Base : constant DI_Flags_T := 36;
   DI_Flag_Accessibility : constant DI_Flags_T := 3;
   DI_Flag_Ptr_To_Member_Rep : constant DI_Flags_T := 196608;  -- install/include/llvm-c/DebugInfo.h:73

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
      DWARF_Source_Language_Ada_83,
      DWARF_Source_Language_C_Plus_Plus,
      DWARF_Source_Language_Cobol_74,
      DWARF_Source_Language_Cobol_85,
      DWARF_Source_Language_Fortran_77,
      DWARF_Source_Language_Fortran_90,
      DWARF_Source_Language_Pascal_83,
      DWARF_Source_Language_Modula_2,
      DWARF_Source_Language_Java,
      DWARF_Source_Language_C99,
      DWARF_Source_Language_Ada_95,
      DWARF_Source_Language_Fortran_95,
      DWARF_Source_Language_PLI,
      DWARF_Source_Language_Obj_C,
      DWARF_Source_Language_Obj_C_Plus_Plus,
      DWARF_Source_Language_UPC,
      DWARF_Source_Language_D,
      DWARF_Source_Language_Python,
      DWARF_Source_Language_Open_CL,
      DWARF_Source_Language_Go,
      DWARF_Source_Language_Modula_3,
      DWARF_Source_Language_Haskell,
      DWARF_Source_Language_C_Plus_Plus_03,
      DWARF_Source_Language_C_Plus_Plus_11,
      DWARF_Source_Language_O_Caml,
      DWARF_Source_Language_Rust,
      DWARF_Source_Language_C11,
      DWARF_Source_Language_Swift,
      DWARF_Source_Language_Julia,
      DWARF_Source_Language_Dylan,
      DWARF_Source_Language_C_Plus_Plus_14,
      DWARF_Source_Language_Fortran_03,
      DWARF_Source_Language_Fortran_08,
      DWARF_Source_Language_Render_Script,
      DWARF_Source_Language_BLISS,
      DWARF_Source_Language_Kotlin,
      DWARF_Source_Language_Zig,
      DWARF_Source_Language_Crystal,
      DWARF_Source_Language_C_Plus_Plus_17,
      DWARF_Source_Language_C_Plus_Plus_20,
      DWARF_Source_Language_C17,
      DWARF_Source_Language_Fortran_18,
      DWARF_Source_Language_Ada_2005,
      DWARF_Source_Language_Ada_2012,
      DWARF_Source_Language_Mips_Assembler,
      DWARF_Source_Language_GOOGLE_Render_Script,
      DWARF_Source_Language_BORLAND_Delphi)
   with Convention => C;  -- install/include/llvm-c/DebugInfo.h:132

  --*
  -- * The amount of debug information to emit.
  --  

   type DWARF_Emission_Kind_T is 
     (DWARF_Emission_None,
      DWARF_Emission_Full,
      DWARF_Emission_Line_Tables_Only)
   with Convention => C;  -- install/include/llvm-c/DebugInfo.h:141

  --*
  -- * The kind of metadata nodes.
  --  

   subtype Metadata_Kind_T is unsigned;  -- install/include/llvm-c/DebugInfo.h:184

  --*
  -- * An LLVM DWARF type encoding.
  --  

   subtype DWARF_Type_Encoding_T is unsigned;  -- install/include/llvm-c/DebugInfo.h:189

  --*
  -- * Describes the kind of macro declaration used for LLVMDIBuilderCreateMacro.
  -- * @see llvm::dwarf::MacinfoRecordType
  -- * @note Values are from DW_MACINFO_* constants in the DWARF specification.
  --  

   subtype DWARF_Macinfo_Record_Type_T is unsigned;
   DWARF_Macinfo_Record_Type_Define : constant DWARF_Macinfo_Record_Type_T := 1;
   DWARF_Macinfo_Record_Type_Macro : constant DWARF_Macinfo_Record_Type_T := 2;
   DWARF_Macinfo_Record_Type_Start_File : constant DWARF_Macinfo_Record_Type_T := 3;
   DWARF_Macinfo_Record_Type_End_File : constant DWARF_Macinfo_Record_Type_T := 4;
   DWARF_Macinfo_Record_Type_Vendor_Ext : constant DWARF_Macinfo_Record_Type_T := 255;  -- install/include/llvm-c/DebugInfo.h:202

  --*
  -- * The current debug metadata version number.
  --  

   function Debug_Metadata_Version return unsigned  -- install/include/llvm-c/DebugInfo.h:207
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDebugMetadataVersion";

  --*
  -- * The version of debug metadata that's present in the provided \c Module.
  --  

   function Get_Module_Debug_Metadata_Version (Module : LLVM.Types.Module_T) return unsigned  -- install/include/llvm-c/DebugInfo.h:212
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetModuleDebugMetadataVersion";

  --*
  -- * Strip debug info in the module if it exists.
  -- * To do this, we remove all calls to the debugger intrinsics and any named
  -- * metadata for debugging. We also remove debug locations for instructions.
  -- * Return true if module is modified.
  --  

function Strip_Module_Debug_Info
     (Module : LLVM.Types.Module_T)
      return Boolean;

  --*
  -- * Construct a builder for a module, and do not allow for unresolved nodes
  -- * attached to the module.
  --  

   function Create_DI_Builder_Disallow_Unresolved (M : LLVM.Types.Module_T) return LLVM.Types.DI_Builder_T  -- install/include/llvm-c/DebugInfo.h:226
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreateDIBuilderDisallowUnresolved";

  --*
  -- * Construct a builder for a module and collect unresolved nodes attached
  -- * to the module in order to resolve cycles during a call to
  -- * \c LLVMDIBuilderFinalize.
  --  

   function Create_DI_Builder (M : LLVM.Types.Module_T) return LLVM.Types.DI_Builder_T  -- install/include/llvm-c/DebugInfo.h:233
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreateDIBuilder";

  --*
  -- * Deallocates the \c DIBuilder and everything it owns.
  -- * @note You must call \c LLVMDIBuilderFinalize before this
  --  

   procedure Dispose_DI_Builder (Builder : LLVM.Types.DI_Builder_T)  -- install/include/llvm-c/DebugInfo.h:239
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDisposeDIBuilder";

  --*
  -- * Construct any deferred debug info descriptors.
  --  

   procedure DI_Builder_Finalize (Builder : LLVM.Types.DI_Builder_T)  -- install/include/llvm-c/DebugInfo.h:244
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderFinalize";

  --*
  -- * Finalize a specific subprogram.
  -- * No new variables may be added to this subprogram afterwards.
  --  

   procedure DI_Builder_Finalize_Subprogram (Builder : LLVM.Types.DI_Builder_T; Subprogram : LLVM.Types.Metadata_T)  -- install/include/llvm-c/DebugInfo.h:250
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderFinalizeSubprogram";

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
  -- * \param SysRoot         The Clang system root (value of -isysroot).
  -- * \param SysRootLen      The length of the C string passed to \c SysRoot.
  -- * \param SDK           The SDK. On Darwin, the last component of the sysroot.
  -- * \param SDKLen        The length of the C string passed to \c SDK.
  --  

function DI_Create_Compile_Unit
     (Builder                  : LLVM.Types.DI_Builder_T;
      Lang                     : DWARF_Source_Language_T;
      File_Ref                 : LLVM.Types.Metadata_T;
      Producer                 : String;
      Producer_Len             : stddef_h.size_t;
      Is_Optimized             : Boolean;
      Flags                    : String;
      Flags_Len                : stddef_h.size_t;
      Runtime_Ver              : unsigned;
      Split_Name               : String;
      Split_Name_Len           : stddef_h.size_t;
      Kind                     : DWARF_Emission_Kind_T;
      DWO_Id                   : unsigned;
      Split_Debug_Inlining     : Boolean;
      Debug_Info_For_Profiling : Boolean;
      Sys_Root                 : String;
      Sys_Root_Len             : stddef_h.size_t;
      SDK                      : String;
      SDK_Len                  : stddef_h.size_t)
      return LLVM.Types.Metadata_T;

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

  --*
  -- * Creates a new descriptor for a module with the specified parent scope.
  -- * \param Builder         The \c DIBuilder.
  -- * \param ParentScope     The parent scope containing this module declaration.
  -- * \param Name            Module name.
  -- * \param NameLen         The length of the C string passed to \c Name.
  -- * \param ConfigMacros    A space-separated shell-quoted list of -D macro
  --                          definitions as they would appear on a command line.
  -- * \param ConfigMacrosLen The length of the C string passed to \c ConfigMacros.
  -- * \param IncludePath     The path to the module map file.
  -- * \param IncludePathLen  The length of the C string passed to \c IncludePath.
  -- * \param APINotesFile    The path to an API notes file for the module.
  -- * \param APINotesFileLen The length of the C string passed to \c APINotestFile.
  --  

function DI_Create_Module
     (Builder            : LLVM.Types.DI_Builder_T;
      Parent_Scope       : LLVM.Types.Metadata_T;
      Name               : String;
      Name_Len           : stddef_h.size_t;
      Config_Macros      : String;
      Config_Macros_Len  : stddef_h.size_t;
      Include_Path       : String;
      Include_Path_Len   : stddef_h.size_t;
      API_Notes_File     : String;
      API_Notes_File_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T;

  --*
  -- * Creates a new descriptor for a namespace with the specified parent scope.
  -- * \param Builder          The \c DIBuilder.
  -- * \param ParentScope      The parent scope containing this module declaration.
  -- * \param Name             NameSpace name.
  -- * \param NameLen          The length of the C string passed to \c Name.
  -- * \param ExportSymbols    Whether or not the namespace exports symbols, e.g.
  -- *                         this is true of C++ inline namespaces.
  --  

function DI_Create_Name_Space
     (Builder        : LLVM.Types.DI_Builder_T;
      Parent_Scope   : LLVM.Types.Metadata_T;
      Name           : String;
      Name_Len       : stddef_h.size_t;
      Export_Symbols : Boolean)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create a new descriptor for the specified subprogram.
  -- * \param Builder         The \c DIBuilder.
  -- * \param Scope           Function scope.
  -- * \param Name            Function name.
  -- * \param NameLen         Length of enumeration name.
  -- * \param LinkageName     Mangled function name.
  -- * \param LinkageNameLen  Length of linkage name.
  -- * \param File            File where this variable is defined.
  -- * \param LineNo          Line number.
  -- * \param Ty              Function type.
  -- * \param IsLocalToUnit   True if this function is not externally visible.
  -- * \param IsDefinition    True if this is a function definition.
  -- * \param ScopeLine       Set to the beginning of the scope this starts
  -- * \param Flags           E.g.: \c LLVMDIFlagLValueReference. These flags are
  -- *                        used to emit dwarf attributes.
  -- * \param IsOptimized     True if optimization is ON.
  --  

function DI_Create_Function
     (Builder          : LLVM.Types.DI_Builder_T;
      Scope            : LLVM.Types.Metadata_T;
      Name             : String;
      Name_Len         : stddef_h.size_t;
      Linkage_Name     : String;
      Linkage_Name_Len : stddef_h.size_t;
      File             : LLVM.Types.Metadata_T;
      Line_No          : unsigned;
      Ty               : LLVM.Types.Metadata_T;
      Is_Local_To_Unit : Boolean;
      Is_Definition    : Boolean;
      Scope_Line       : unsigned;
      Flags            : DI_Flags_T;
      Is_Optimized     : Boolean)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create a descriptor for a lexical block with the specified parent context.
  -- * \param Builder      The \c DIBuilder.
  -- * \param Scope        Parent lexical block.
  -- * \param File         Source file.
  -- * \param Line         The line in the source file.
  -- * \param Column       The column in the source file.
  --  

   function DI_Builder_Create_Lexical_Block
     (Builder : LLVM.Types.DI_Builder_T;
      Scope : LLVM.Types.Metadata_T;
      File : LLVM.Types.Metadata_T;
      Line : unsigned;
      Column : unsigned) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:376
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderCreateLexicalBlock";

  --*
  -- * Create a descriptor for a lexical block with a new file attached.
  -- * \param Builder        The \c DIBuilder.
  -- * \param Scope          Lexical block.
  -- * \param File           Source file.
  -- * \param Discriminator  DWARF path discriminator value.
  --  

   function DI_Builder_Create_Lexical_Block_File
     (Builder : LLVM.Types.DI_Builder_T;
      Scope : LLVM.Types.Metadata_T;
      File : LLVM.Types.Metadata_T;
      Discriminator : unsigned) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:388
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderCreateLexicalBlockFile";

  --*
  -- * Create a descriptor for an imported namespace. Suitable for e.g. C++
  -- * using declarations.
  -- * \param Builder    The \c DIBuilder.
  -- * \param Scope      The scope this module is imported into
  -- * \param File       File where the declaration is located.
  -- * \param Line       Line number of the declaration.
  --  

   function DI_Builder_Create_Imported_Module_From_Namespace
     (Builder : LLVM.Types.DI_Builder_T;
      Scope : LLVM.Types.Metadata_T;
      NS : LLVM.Types.Metadata_T;
      File : LLVM.Types.Metadata_T;
      Line : unsigned) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:402
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderCreateImportedModuleFromNamespace";

  --*
  -- * Create a descriptor for an imported module that aliases another
  -- * imported entity descriptor.
  -- * \param Builder        The \c DIBuilder.
  -- * \param Scope          The scope this module is imported into
  -- * \param ImportedEntity Previous imported entity to alias.
  -- * \param File           File where the declaration is located.
  -- * \param Line           Line number of the declaration.
  -- * \param Elements       Renamed elements.
  -- * \param NumElements    Number of renamed elements.
  --  

   function DI_Builder_Create_Imported_Module_From_Alias
     (Builder : LLVM.Types.DI_Builder_T;
      Scope : LLVM.Types.Metadata_T;
      Imported_Entity : LLVM.Types.Metadata_T;
      File : LLVM.Types.Metadata_T;
      Line : unsigned;
      Elements : System.Address;
      Num_Elements : unsigned) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:419
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderCreateImportedModuleFromAlias";

  --*
  -- * Create a descriptor for an imported module.
  -- * \param Builder        The \c DIBuilder.
  -- * \param Scope          The scope this module is imported into
  -- * \param M              The module being imported here
  -- * \param File           File where the declaration is located.
  -- * \param Line           Line number of the declaration.
  -- * \param Elements       Renamed elements.
  -- * \param NumElements    Number of renamed elements.
  --  

   function DI_Builder_Create_Imported_Module_From_Module
     (Builder : LLVM.Types.DI_Builder_T;
      Scope : LLVM.Types.Metadata_T;
      M : LLVM.Types.Metadata_T;
      File : LLVM.Types.Metadata_T;
      Line : unsigned;
      Elements : System.Address;
      Num_Elements : unsigned) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:434
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderCreateImportedModuleFromModule";

  --*
  -- * Create a descriptor for an imported function, type, or variable.  Suitable
  -- * for e.g. FORTRAN-style USE declarations.
  -- * \param Builder        The DIBuilder.
  -- * \param Scope          The scope this module is imported into.
  -- * \param Decl           The declaration (or definition) of a function, type,
  --                         or variable.
  -- * \param File           File where the declaration is located.
  -- * \param Line           Line number of the declaration.
  -- * \param Name           A name that uniquely identifies this imported
  -- declaration.
  -- * \param NameLen        The length of the C string passed to \c Name.
  -- * \param Elements       Renamed elements.
  -- * \param NumElements    Number of renamed elements.
  --  

function DI_Create_Imported_Declaration
     (Builder      : LLVM.Types.DI_Builder_T;
      Scope        : LLVM.Types.Metadata_T;
      Decl         : LLVM.Types.Metadata_T;
      File         : LLVM.Types.Metadata_T;
      Line         : unsigned;
      Name         : String;
      Name_Len     : stddef_h.size_t;
      Elements     : System.Address;
      Num_Elements : unsigned)
      return LLVM.Types.Metadata_T;

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
      Inlined_At : LLVM.Types.Metadata_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:470
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderCreateDebugLocation";

  --*
  -- * Get the line number of this debug location.
  -- * \param Location     The debug location.
  -- *
  -- * @see DILocation::getLine()
  --  

   function DI_Location_Get_Line (Location : LLVM.Types.Metadata_T) return unsigned  -- install/include/llvm-c/DebugInfo.h:480
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDILocationGetLine";

  --*
  -- * Get the column number of this debug location.
  -- * \param Location     The debug location.
  -- *
  -- * @see DILocation::getColumn()
  --  

   function DI_Location_Get_Column (Location : LLVM.Types.Metadata_T) return unsigned  -- install/include/llvm-c/DebugInfo.h:488
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDILocationGetColumn";

  --*
  -- * Get the local scope associated with this debug location.
  -- * \param Location     The debug location.
  -- *
  -- * @see DILocation::getScope()
  --  

   function DI_Location_Get_Scope (Location : LLVM.Types.Metadata_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:496
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDILocationGetScope";

  --*
  -- * Get the "inline at" location associated with this debug location.
  -- * \param Location     The debug location.
  -- *
  -- * @see DILocation::getInlinedAt()
  --  

   function DI_Location_Get_Inlined_At (Location : LLVM.Types.Metadata_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:504
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDILocationGetInlinedAt";

  --*
  -- * Get the metadata of the file associated with a given scope.
  -- * \param Scope     The scope object.
  -- *
  -- * @see DIScope::getFile()
  --  

   function DI_Scope_Get_File (Scope : LLVM.Types.Metadata_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:512
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIScopeGetFile";

  --*
  -- * Get the directory of a given file.
  -- * \param File     The file object.
  -- * \param Len      The length of the returned string.
  -- *
  -- * @see DIFile::getDirectory()
  --  

function DI_File_Get_Directory
     (File : LLVM.Types.Metadata_T;
      Len  : access unsigned)
      return String;

  --*
  -- * Get the name of a given file.
  -- * \param File     The file object.
  -- * \param Len      The length of the returned string.
  -- *
  -- * @see DIFile::getFilename()
  --  

function DI_File_Get_Filename
     (File : LLVM.Types.Metadata_T;
      Len  : access unsigned)
      return String;

  --*
  -- * Get the source of a given file.
  -- * \param File     The file object.
  -- * \param Len      The length of the returned string.
  -- *
  -- * @see DIFile::getSource()
  --  

function DI_File_Get_Source
     (File : LLVM.Types.Metadata_T;
      Len  : access unsigned)
      return String;

  --*
  -- * Create a type array.
  -- * \param Builder        The DIBuilder.
  -- * \param Data           The type elements.
  -- * \param NumElements    Number of type elements.
  --  

   function DI_Builder_Get_Or_Create_Type_Array
     (Builder : LLVM.Types.DI_Builder_T;
      Data : System.Address;
      Num_Elements : stddef_h.size_t) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:547
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderGetOrCreateTypeArray";

  --*
  -- * Create subroutine type.
  -- * \param Builder        The DIBuilder.
  -- * \param File            The file in which the subroutine resides.
  -- * \param ParameterTypes  An array of subroutine parameter types. This
  -- *                        includes return type at 0th index.
  -- * \param NumParameterTypes The number of parameter types in \c ParameterTypes
  -- * \param Flags           E.g.: \c LLVMDIFlagLValueReference.
  -- *                        These flags are used to emit dwarf attributes.
  --  

   function DI_Builder_Create_Subroutine_Type
     (Builder : LLVM.Types.DI_Builder_T;
      File : LLVM.Types.Metadata_T;
      Parameter_Types : System.Address;
      Num_Parameter_Types : unsigned;
      Flags : DI_Flags_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:562
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderCreateSubroutineType";

  --*
  -- * Create debugging information entry for a macro.
  -- * @param Builder         The DIBuilder.
  -- * @param ParentMacroFile Macro parent (could be NULL).
  -- * @param Line            Source line number where the macro is defined.
  -- * @param RecordType      DW_MACINFO_define or DW_MACINFO_undef.
  -- * @param Name            Macro name.
  -- * @param NameLen         Macro name length.
  -- * @param Value           Macro value.
  -- * @param ValueLen        Macro value length.
  --  

function DI_Create_Macro
     (Builder           : LLVM.Types.DI_Builder_T;
      Parent_Macro_File : LLVM.Types.Metadata_T;
      Line              : unsigned;
      Record_Type       : DWARF_Macinfo_Record_Type_T;
      Name              : String;
      Name_Len          : stddef_h.size_t;
      Value             : String;
      Value_Len         : stddef_h.size_t)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create debugging information temporary entry for a macro file.
  -- * List of macro node direct children will be calculated by DIBuilder,
  -- * using the \p ParentMacroFile relationship.
  -- * @param Builder         The DIBuilder.
  -- * @param ParentMacroFile Macro parent (could be NULL).
  -- * @param Line            Source line number where the macro file is included.
  -- * @param File            File descriptor containing the name of the macro file.
  --  

   function DI_Builder_Create_Temp_Macro_File
     (Builder : LLVM.Types.DI_Builder_T;
      Parent_Macro_File : LLVM.Types.Metadata_T;
      Line : unsigned;
      File : LLVM.Types.Metadata_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:596
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderCreateTempMacroFile";

  --*
  -- * Create debugging information entry for an enumerator.
  -- * @param Builder        The DIBuilder.
  -- * @param Name           Enumerator name.
  -- * @param NameLen        Length of enumerator name.
  -- * @param Value          Enumerator value.
  -- * @param IsUnsigned     True if the value is unsigned.
  --  

function DI_Create_Enumerator
     (Builder     : LLVM.Types.DI_Builder_T;
      Name        : String;
      Name_Len    : stddef_h.size_t;
      Value       : stdint_h.int64_t;
      Is_Unsigned : Boolean)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create debugging information entry for an enumeration.
  -- * \param Builder        The DIBuilder.
  -- * \param Scope          Scope in which this enumeration is defined.
  -- * \param Name           Enumeration name.
  -- * \param NameLen        Length of enumeration name.
  -- * \param File           File where this member is defined.
  -- * \param LineNumber     Line number.
  -- * \param SizeInBits     Member size.
  -- * \param AlignInBits    Member alignment.
  -- * \param Elements       Enumeration elements.
  -- * \param NumElements    Number of enumeration elements.
  -- * \param ClassTy        Underlying type of a C++11/ObjC fixed enum.
  --  

function DI_Create_Enumeration_Type
     (Builder       : LLVM.Types.DI_Builder_T;
      Scope         : LLVM.Types.Metadata_T;
      Name          : String;
      Name_Len      : stddef_h.size_t;
      File          : LLVM.Types.Metadata_T;
      Line_Number   : unsigned;
      Size_In_Bits  : stdint_h.uint64_t;
      Align_In_Bits : stdint_h.uint32_t;
      Elements      : System.Address;
      Num_Elements  : unsigned;
      Class_Ty      : LLVM.Types.Metadata_T)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create debugging information entry for a union.
  -- * \param Builder      The DIBuilder.
  -- * \param Scope        Scope in which this union is defined.
  -- * \param Name         Union name.
  -- * \param NameLen      Length of union name.
  -- * \param File         File where this member is defined.
  -- * \param LineNumber   Line number.
  -- * \param SizeInBits   Member size.
  -- * \param AlignInBits  Member alignment.
  -- * \param Flags        Flags to encode member attribute, e.g. private
  -- * \param Elements     Union elements.
  -- * \param NumElements  Number of union elements.
  -- * \param RunTimeLang  Optional parameter, Objective-C runtime version.
  -- * \param UniqueId     A unique identifier for the union.
  -- * \param UniqueIdLen  Length of unique identifier.
  --  

function DI_Create_Union_Type
     (Builder       : LLVM.Types.DI_Builder_T;
      Scope         : LLVM.Types.Metadata_T;
      Name          : String;
      Name_Len      : stddef_h.size_t;
      File          : LLVM.Types.Metadata_T;
      Line_Number   : unsigned;
      Size_In_Bits  : stdint_h.uint64_t;
      Align_In_Bits : stdint_h.uint32_t;
      Flags         : DI_Flags_T;
      Elements      : System.Address;
      Num_Elements  : unsigned;
      Run_Time_Lang : unsigned;
      Unique_Id     : String;
      Unique_Id_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create debugging information entry for an array.
  -- * \param Builder      The DIBuilder.
  -- * \param Size         Array size.
  -- * \param AlignInBits  Alignment.
  -- * \param Ty           Element type.
  -- * \param Subscripts   Subscripts.
  -- * \param NumSubscripts Number of subscripts.
  --  

   function DI_Builder_Create_Array_Type
     (Builder : LLVM.Types.DI_Builder_T;
      Size : stdint_h.uint64_t;
      Align_In_Bits : stdint_h.uint32_t;
      Ty : LLVM.Types.Metadata_T;
      Subscripts : System.Address;
      Num_Subscripts : unsigned) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:668
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderCreateArrayType";

  --*
  -- * Create debugging information entry for a vector type.
  -- * \param Builder      The DIBuilder.
  -- * \param Size         Vector size.
  -- * \param AlignInBits  Alignment.
  -- * \param Ty           Element type.
  -- * \param Subscripts   Subscripts.
  -- * \param NumSubscripts Number of subscripts.
  --  

   function DI_Builder_Create_Vector_Type
     (Builder : LLVM.Types.DI_Builder_T;
      Size : stdint_h.uint64_t;
      Align_In_Bits : stdint_h.uint32_t;
      Ty : LLVM.Types.Metadata_T;
      Subscripts : System.Address;
      Num_Subscripts : unsigned) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:683
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderCreateVectorType";

  --*
  -- * Create a DWARF unspecified type.
  -- * \param Builder   The DIBuilder.
  -- * \param Name      The unspecified type's name.
  -- * \param NameLen   Length of type name.
  --  

function DI_Create_Unspecified_Type
     (Builder  : LLVM.Types.DI_Builder_T;
      Name     : String;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create debugging information entry for a basic
  -- * type.
  -- * \param Builder     The DIBuilder.
  -- * \param Name        Type name.
  -- * \param NameLen     Length of type name.
  -- * \param SizeInBits  Size of the type.
  -- * \param Encoding    DWARF encoding code, e.g. \c LLVMDWARFTypeEncoding_float.
  -- * \param Flags       Flags to encode optional attribute like endianity
  --  

function DI_Create_Basic_Type
     (Builder      : LLVM.Types.DI_Builder_T;
      Name         : String;
      Name_Len     : stddef_h.size_t;
      Size_In_Bits : stdint_h.uint64_t;
      Encoding     : DWARF_Type_Encoding_T;
      Flags        : DI_Flags_T)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create debugging information entry for a pointer.
  -- * \param Builder     The DIBuilder.
  -- * \param PointeeTy         Type pointed by this pointer.
  -- * \param SizeInBits        Size.
  -- * \param AlignInBits       Alignment. (optional, pass 0 to ignore)
  -- * \param AddressSpace      DWARF address space. (optional, pass 0 to ignore)
  -- * \param Name              Pointer type name. (optional)
  -- * \param NameLen           Length of pointer type name. (optional)
  --  

function DI_Create_Pointer_Type
     (Builder       : LLVM.Types.DI_Builder_T;
      Pointee_Ty    : LLVM.Types.Metadata_T;
      Size_In_Bits  : stdint_h.uint64_t;
      Align_In_Bits : stdint_h.uint32_t;
      Address_Space : unsigned;
      Name          : String;
      Name_Len      : stddef_h.size_t)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create debugging information entry for a struct.
  -- * \param Builder     The DIBuilder.
  -- * \param Scope        Scope in which this struct is defined.
  -- * \param Name         Struct name.
  -- * \param NameLen      Struct name length.
  -- * \param File         File where this member is defined.
  -- * \param LineNumber   Line number.
  -- * \param SizeInBits   Member size.
  -- * \param AlignInBits  Member alignment.
  -- * \param Flags        Flags to encode member attribute, e.g. private
  -- * \param Elements     Struct elements.
  -- * \param NumElements  Number of struct elements.
  -- * \param RunTimeLang  Optional parameter, Objective-C runtime version.
  -- * \param VTableHolder The object containing the vtable for the struct.
  -- * \param UniqueId     A unique identifier for the struct.
  -- * \param UniqueIdLen  Length of the unique identifier for the struct.
  --  

function DI_Create_Struct_Type
     (Builder        : LLVM.Types.DI_Builder_T;
      Scope          : LLVM.Types.Metadata_T;
      Name           : String;
      Name_Len       : stddef_h.size_t;
      File           : LLVM.Types.Metadata_T;
      Line_Number    : unsigned;
      Size_In_Bits   : stdint_h.uint64_t;
      Align_In_Bits  : stdint_h.uint32_t;
      Flags          : DI_Flags_T;
      Derived_From   : LLVM.Types.Metadata_T;
      Elements       : System.Address;
      Num_Elements   : unsigned;
      Run_Time_Lang  : unsigned;
      V_Table_Holder : LLVM.Types.Metadata_T;
      Unique_Id      : String;
      Unique_Id_Len  : stddef_h.size_t)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create debugging information entry for a member.
  -- * \param Builder      The DIBuilder.
  -- * \param Scope        Member scope.
  -- * \param Name         Member name.
  -- * \param NameLen      Length of member name.
  -- * \param File         File where this member is defined.
  -- * \param LineNo       Line number.
  -- * \param SizeInBits   Member size.
  -- * \param AlignInBits  Member alignment.
  -- * \param OffsetInBits Member offset.
  -- * \param Flags        Flags to encode member attribute, e.g. private
  -- * \param Ty           Parent type.
  --  

function DI_Create_Member_Type
     (Builder        : LLVM.Types.DI_Builder_T;
      Scope          : LLVM.Types.Metadata_T;
      Name           : String;
      Name_Len       : stddef_h.size_t;
      File           : LLVM.Types.Metadata_T;
      Line_No        : unsigned;
      Size_In_Bits   : stdint_h.uint64_t;
      Align_In_Bits  : stdint_h.uint32_t;
      Offset_In_Bits : stdint_h.uint64_t;
      Flags          : DI_Flags_T;
      Ty             : LLVM.Types.Metadata_T)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create debugging information entry for a
  -- * C++ static data member.
  -- * \param Builder      The DIBuilder.
  -- * \param Scope        Member scope.
  -- * \param Name         Member name.
  -- * \param NameLen      Length of member name.
  -- * \param File         File where this member is declared.
  -- * \param LineNumber   Line number.
  -- * \param Type         Type of the static member.
  -- * \param Flags        Flags to encode member attribute, e.g. private.
  -- * \param ConstantVal  Const initializer of the member.
  -- * \param AlignInBits  Member alignment.
  --  

function DI_Create_Static_Member_Type
     (Builder       : LLVM.Types.DI_Builder_T;
      Scope         : LLVM.Types.Metadata_T;
      Name          : String;
      Name_Len      : stddef_h.size_t;
      File          : LLVM.Types.Metadata_T;
      Line_Number   : unsigned;
      C_Type        : LLVM.Types.Metadata_T;
      Flags         : DI_Flags_T;
      Constant_Val  : LLVM.Types.Value_T;
      Align_In_Bits : stdint_h.uint32_t)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create debugging information entry for a pointer to member.
  -- * \param Builder      The DIBuilder.
  -- * \param PointeeType  Type pointed to by this pointer.
  -- * \param ClassType    Type for which this pointer points to members of.
  -- * \param SizeInBits   Size.
  -- * \param AlignInBits  Alignment.
  -- * \param Flags        Flags.
  --  

   function DI_Builder_Create_Member_Pointer_Type
     (Builder : LLVM.Types.DI_Builder_T;
      Pointee_Type : LLVM.Types.Metadata_T;
      Class_Type : LLVM.Types.Metadata_T;
      Size_In_Bits : stdint_h.uint64_t;
      Align_In_Bits : stdint_h.uint32_t;
      Flags : DI_Flags_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:806
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderCreateMemberPointerType";

  --*
  -- * Create debugging information entry for Objective-C instance variable.
  -- * \param Builder      The DIBuilder.
  -- * \param Name         Member name.
  -- * \param NameLen      The length of the C string passed to \c Name.
  -- * \param File         File where this member is defined.
  -- * \param LineNo       Line number.
  -- * \param SizeInBits   Member size.
  -- * \param AlignInBits  Member alignment.
  -- * \param OffsetInBits Member offset.
  -- * \param Flags        Flags to encode member attribute, e.g. private
  -- * \param Ty           Parent type.
  -- * \param PropertyNode Property associated with this ivar.
  --  

function DI_Create_Obj_CI_Var
     (Builder        : LLVM.Types.DI_Builder_T;
      Name           : String;
      Name_Len       : stddef_h.size_t;
      File           : LLVM.Types.Metadata_T;
      Line_No        : unsigned;
      Size_In_Bits   : stdint_h.uint64_t;
      Align_In_Bits  : stdint_h.uint32_t;
      Offset_In_Bits : stdint_h.uint64_t;
      Flags          : DI_Flags_T;
      Ty             : LLVM.Types.Metadata_T;
      Property_Node  : LLVM.Types.Metadata_T)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create debugging information entry for Objective-C property.
  -- * \param Builder            The DIBuilder.
  -- * \param Name               Property name.
  -- * \param NameLen            The length of the C string passed to \c Name.
  -- * \param File               File where this property is defined.
  -- * \param LineNo             Line number.
  -- * \param GetterName         Name of the Objective C property getter selector.
  -- * \param GetterNameLen      The length of the C string passed to \c GetterName.
  -- * \param SetterName         Name of the Objective C property setter selector.
  -- * \param SetterNameLen      The length of the C string passed to \c SetterName.
  -- * \param PropertyAttributes Objective C property attributes.
  -- * \param Ty                 Type.
  --  

function DI_Create_Obj_C_Property
     (Builder             : LLVM.Types.DI_Builder_T;
      Name                : String;
      Name_Len            : stddef_h.size_t;
      File                : LLVM.Types.Metadata_T;
      Line_No             : unsigned;
      Getter_Name         : String;
      Getter_Name_Len     : stddef_h.size_t;
      Setter_Name         : String;
      Setter_Name_Len     : stddef_h.size_t;
      Property_Attributes : unsigned;
      Ty                  : LLVM.Types.Metadata_T)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create a uniqued DIType* clone with FlagObjectPointer and FlagArtificial set.
  -- * \param Builder   The DIBuilder.
  -- * \param Type      The underlying type to which this pointer points.
  --  

   function DI_Builder_Create_Object_Pointer_Type (Builder : LLVM.Types.DI_Builder_T; C_Type : LLVM.Types.Metadata_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:863
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderCreateObjectPointerType";

  --*
  -- * Create debugging information entry for a qualified
  -- * type, e.g. 'const int'.
  -- * \param Builder     The DIBuilder.
  -- * \param Tag         Tag identifying type,
  -- *                    e.g. LLVMDWARFTypeQualifier_volatile_type
  -- * \param Type        Base Type.
  --  

   function DI_Builder_Create_Qualified_Type
     (Builder : LLVM.Types.DI_Builder_T;
      Tag : unsigned;
      C_Type : LLVM.Types.Metadata_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:875
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderCreateQualifiedType";

  --*
  -- * Create debugging information entry for a c++
  -- * style reference or rvalue reference type.
  -- * \param Builder   The DIBuilder.
  -- * \param Tag       Tag identifying type,
  -- * \param Type      Base Type.
  --  

   function DI_Builder_Create_Reference_Type
     (Builder : LLVM.Types.DI_Builder_T;
      Tag : unsigned;
      C_Type : LLVM.Types.Metadata_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:886
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderCreateReferenceType";

  --*
  -- * Create C++11 nullptr type.
  -- * \param Builder   The DIBuilder.
  --  

   function DI_Builder_Create_Null_Ptr_Type (Builder : LLVM.Types.DI_Builder_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:894
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderCreateNullPtrType";

  --*
  -- * Create debugging information entry for a typedef.
  -- * \param Builder    The DIBuilder.
  -- * \param Type       Original type.
  -- * \param Name       Typedef name.
  -- * \param File       File where this type is defined.
  -- * \param LineNo     Line number.
  -- * \param Scope      The surrounding context for the typedef.
  --  

function DI_Create_Typedef
     (Builder       : LLVM.Types.DI_Builder_T;
      C_Type        : LLVM.Types.Metadata_T;
      Name          : String;
      Name_Len      : stddef_h.size_t;
      File          : LLVM.Types.Metadata_T;
      Line_No       : unsigned;
      Scope         : LLVM.Types.Metadata_T;
      Align_In_Bits : stdint_h.uint32_t)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create debugging information entry to establish inheritance relationship
  -- * between two types.
  -- * \param Builder       The DIBuilder.
  -- * \param Ty            Original type.
  -- * \param BaseTy        Base type. Ty is inherits from base.
  -- * \param BaseOffset    Base offset.
  -- * \param VBPtrOffset  Virtual base pointer offset.
  -- * \param Flags         Flags to describe inheritance attribute, e.g. private
  --  

   function DI_Builder_Create_Inheritance
     (Builder : LLVM.Types.DI_Builder_T;
      Ty : LLVM.Types.Metadata_T;
      Base_Ty : LLVM.Types.Metadata_T;
      Base_Offset : stdint_h.uint64_t;
      VB_Ptr_Offset : stdint_h.uint32_t;
      Flags : DI_Flags_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:922
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderCreateInheritance";

  --*
  -- * Create a permanent forward-declared type.
  -- * \param Builder             The DIBuilder.
  -- * \param Tag                 A unique tag for this type.
  -- * \param Name                Type name.
  -- * \param NameLen             Length of type name.
  -- * \param Scope               Type scope.
  -- * \param File                File where this type is defined.
  -- * \param Line                Line number where this type is defined.
  -- * \param RuntimeLang         Indicates runtime version for languages like
  -- *                            Objective-C.
  -- * \param SizeInBits          Member size.
  -- * \param AlignInBits         Member alignment.
  -- * \param UniqueIdentifier    A unique identifier for the type.
  -- * \param UniqueIdentifierLen Length of the unique identifier.
  --  

function DI_Create_Forward_Decl
     (Builder               : LLVM.Types.DI_Builder_T;
      Tag                   : unsigned;
      Name                  : String;
      Name_Len              : stddef_h.size_t;
      Scope                 : LLVM.Types.Metadata_T;
      File                  : LLVM.Types.Metadata_T;
      Line                  : unsigned;
      Runtime_Lang          : unsigned;
      Size_In_Bits          : stdint_h.uint64_t;
      Align_In_Bits         : stdint_h.uint32_t;
      Unique_Identifier     : String;
      Unique_Identifier_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create a temporary forward-declared type.
  -- * \param Builder             The DIBuilder.
  -- * \param Tag                 A unique tag for this type.
  -- * \param Name                Type name.
  -- * \param NameLen             Length of type name.
  -- * \param Scope               Type scope.
  -- * \param File                File where this type is defined.
  -- * \param Line                Line number where this type is defined.
  -- * \param RuntimeLang         Indicates runtime version for languages like
  -- *                            Objective-C.
  -- * \param SizeInBits          Member size.
  -- * \param AlignInBits         Member alignment.
  -- * \param Flags               Flags.
  -- * \param UniqueIdentifier    A unique identifier for the type.
  -- * \param UniqueIdentifierLen Length of the unique identifier.
  --  

function DI_Create_Replaceable_Composite_Type
     (Builder               : LLVM.Types.DI_Builder_T;
      Tag                   : unsigned;
      Name                  : String;
      Name_Len              : stddef_h.size_t;
      Scope                 : LLVM.Types.Metadata_T;
      File                  : LLVM.Types.Metadata_T;
      Line                  : unsigned;
      Runtime_Lang          : unsigned;
      Size_In_Bits          : stdint_h.uint64_t;
      Align_In_Bits         : stdint_h.uint32_t;
      Flags                 : DI_Flags_T;
      Unique_Identifier     : String;
      Unique_Identifier_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create debugging information entry for a bit field member.
  -- * \param Builder             The DIBuilder.
  -- * \param Scope               Member scope.
  -- * \param Name                Member name.
  -- * \param NameLen             Length of member name.
  -- * \param File                File where this member is defined.
  -- * \param LineNumber          Line number.
  -- * \param SizeInBits          Member size.
  -- * \param OffsetInBits        Member offset.
  -- * \param StorageOffsetInBits Member storage offset.
  -- * \param Flags               Flags to encode member attribute.
  -- * \param Type                Parent type.
  --  

function DI_Create_Bit_Field_Member_Type
     (Builder                : LLVM.Types.DI_Builder_T;
      Scope                  : LLVM.Types.Metadata_T;
      Name                   : String;
      Name_Len               : stddef_h.size_t;
      File                   : LLVM.Types.Metadata_T;
      Line_Number            : unsigned;
      Size_In_Bits           : stdint_h.uint64_t;
      Offset_In_Bits         : stdint_h.uint64_t;
      Storage_Offset_In_Bits : stdint_h.uint64_t;
      Flags                  : DI_Flags_T;
      C_Type                 : LLVM.Types.Metadata_T)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create debugging information entry for a class.
  -- * \param Scope               Scope in which this class is defined.
  -- * \param Name                Class name.
  -- * \param NameLen             The length of the C string passed to \c Name.
  -- * \param File                File where this member is defined.
  -- * \param LineNumber          Line number.
  -- * \param SizeInBits          Member size.
  -- * \param AlignInBits         Member alignment.
  -- * \param OffsetInBits        Member offset.
  -- * \param Flags               Flags to encode member attribute, e.g. private.
  -- * \param DerivedFrom         Debug info of the base class of this type.
  -- * \param Elements            Class members.
  -- * \param NumElements         Number of class elements.
  -- * \param VTableHolder        Debug info of the base class that contains vtable
  -- *                            for this type. This is used in
  -- *                            DW_AT_containing_type. See DWARF documentation
  -- *                            for more info.
  -- * \param TemplateParamsNode  Template type parameters.
  -- * \param UniqueIdentifier    A unique identifier for the type.
  -- * \param UniqueIdentifierLen Length of the unique identifier.
  --  

function DI_Create_Class_Type
     (Builder               : LLVM.Types.DI_Builder_T;
      Scope                 : LLVM.Types.Metadata_T;
      Name                  : String;
      Name_Len              : stddef_h.size_t;
      File                  : LLVM.Types.Metadata_T;
      Line_Number           : unsigned;
      Size_In_Bits          : stdint_h.uint64_t;
      Align_In_Bits         : stdint_h.uint32_t;
      Offset_In_Bits        : stdint_h.uint64_t;
      Flags                 : DI_Flags_T;
      Derived_From          : LLVM.Types.Metadata_T;
      Elements              : System.Address;
      Num_Elements          : unsigned;
      V_Table_Holder        : LLVM.Types.Metadata_T;
      Template_Params_Node  : LLVM.Types.Metadata_T;
      Unique_Identifier     : String;
      Unique_Identifier_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create a uniqued DIType* clone with FlagArtificial set.
  -- * \param Builder     The DIBuilder.
  -- * \param Type        The underlying type.
  --  

   function DI_Builder_Create_Artificial_Type (Builder : LLVM.Types.DI_Builder_T; C_Type : LLVM.Types.Metadata_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:1035
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderCreateArtificialType";

  --*
  -- * Get the name of this DIType.
  -- * \param DType     The DIType.
  -- * \param Length    The length of the returned string.
  -- *
  -- * @see DIType::getName()
  --  

function DI_Type_Get_Name
     (D_Type : LLVM.Types.Metadata_T;
      Length : access stddef_h.size_t)
      return String;

  --*
  -- * Get the size of this DIType in bits.
  -- * \param DType     The DIType.
  -- *
  -- * @see DIType::getSizeInBits()
  --  

   function DI_Type_Get_Size_In_Bits (D_Type : LLVM.Types.Metadata_T) return stdint_h.uint64_t  -- install/include/llvm-c/DebugInfo.h:1053
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDITypeGetSizeInBits";

  --*
  -- * Get the offset of this DIType in bits.
  -- * \param DType     The DIType.
  -- *
  -- * @see DIType::getOffsetInBits()
  --  

   function DI_Type_Get_Offset_In_Bits (D_Type : LLVM.Types.Metadata_T) return stdint_h.uint64_t  -- install/include/llvm-c/DebugInfo.h:1061
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDITypeGetOffsetInBits";

  --*
  -- * Get the alignment of this DIType in bits.
  -- * \param DType     The DIType.
  -- *
  -- * @see DIType::getAlignInBits()
  --  

   function DI_Type_Get_Align_In_Bits (D_Type : LLVM.Types.Metadata_T) return stdint_h.uint32_t  -- install/include/llvm-c/DebugInfo.h:1069
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDITypeGetAlignInBits";

  --*
  -- * Get the source line where this DIType is declared.
  -- * \param DType     The DIType.
  -- *
  -- * @see DIType::getLine()
  --  

   function DI_Type_Get_Line (D_Type : LLVM.Types.Metadata_T) return unsigned  -- install/include/llvm-c/DebugInfo.h:1077
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDITypeGetLine";

  --*
  -- * Get the flags associated with this DIType.
  -- * \param DType     The DIType.
  -- *
  -- * @see DIType::getFlags()
  --  

   function DI_Type_Get_Flags (D_Type : LLVM.Types.Metadata_T) return DI_Flags_T  -- install/include/llvm-c/DebugInfo.h:1085
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDITypeGetFlags";

  --*
  -- * Create a descriptor for a value range.
  -- * \param Builder    The DIBuilder.
  -- * \param LowerBound Lower bound of the subrange, e.g. 0 for C, 1 for Fortran.
  -- * \param Count      Count of elements in the subrange.
  --  

   function DI_Builder_Get_Or_Create_Subrange
     (Builder : LLVM.Types.DI_Builder_T;
      Lower_Bound : stdint_h.int64_t;
      Count : stdint_h.int64_t) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:1093
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderGetOrCreateSubrange";

  --*
  -- * Create an array of DI Nodes.
  -- * \param Builder        The DIBuilder.
  -- * \param Data           The DI Node elements.
  -- * \param NumElements    Number of DI Node elements.
  --  

   function DI_Builder_Get_Or_Create_Array
     (Builder : LLVM.Types.DI_Builder_T;
      Data : System.Address;
      Num_Elements : stddef_h.size_t) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:1103
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderGetOrCreateArray";

  --*
  -- * Create a new descriptor for the specified variable which has a complex
  -- * address expression for its address.
  -- * \param Builder     The DIBuilder.
  -- * \param Addr        An array of complex address operations.
  -- * \param Length      Length of the address operation array.
  --  

   function DI_Builder_Create_Expression
     (Builder : LLVM.Types.DI_Builder_T;
      Addr : access stdint_h.uint64_t;
      Length : stddef_h.size_t) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:1114
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderCreateExpression";

  --*
  -- * Create a new descriptor for the specified variable that does not have an
  -- * address, but does have a constant value.
  -- * \param Builder     The DIBuilder.
  -- * \param Value       The constant value.
  --  

   function DI_Builder_Create_Constant_Value_Expression (Builder : LLVM.Types.DI_Builder_T; Value : stdint_h.uint64_t) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:1124
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderCreateConstantValueExpression";

  --*
  -- * Create a new descriptor for the specified variable.
  -- * \param Scope       Variable scope.
  -- * \param Name        Name of the variable.
  -- * \param NameLen     The length of the C string passed to \c Name.
  -- * \param Linkage     Mangled  name of the variable.
  -- * \param LinkLen     The length of the C string passed to \c Linkage.
  -- * \param File        File where this variable is defined.
  -- * \param LineNo      Line number.
  -- * \param Ty          Variable Type.
  -- * \param LocalToUnit Boolean flag indicate whether this variable is
  -- *                    externally visible or not.
  -- * \param Expr        The location of the global relative to the attached
  -- *                    GlobalVariable.
  -- * \param Decl        Reference to the corresponding declaration.
  -- *                    variables.
  -- * \param AlignInBits Variable alignment(or 0 if no alignment attr was
  -- *                    specified)
  --  

function DI_Create_Global_Variable_Expression
     (Builder       : LLVM.Types.DI_Builder_T;
      Scope         : LLVM.Types.Metadata_T;
      Name          : String;
      Name_Len      : stddef_h.size_t;
      Linkage       : String;
      Link_Len      : stddef_h.size_t;
      File          : LLVM.Types.Metadata_T;
      Line_No       : unsigned;
      Ty            : LLVM.Types.Metadata_T;
      Local_To_Unit : Boolean;
      Expr          : LLVM.Types.Metadata_T;
      Decl          : LLVM.Types.Metadata_T;
      Align_In_Bits : stdint_h.uint32_t)
      return LLVM.Types.Metadata_T;

  --*
  -- * Retrieves the \c DIVariable associated with this global variable expression.
  -- * \param GVE    The global variable expression.
  -- *
  -- * @see llvm::DIGlobalVariableExpression::getVariable()
  --  

   function DI_Global_Variable_Expression_Get_Variable (GVE : LLVM.Types.Metadata_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:1158
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIGlobalVariableExpressionGetVariable";

  --*
  -- * Retrieves the \c DIExpression associated with this global variable expression.
  -- * \param GVE    The global variable expression.
  -- *
  -- * @see llvm::DIGlobalVariableExpression::getExpression()
  --  

   function DI_Global_Variable_Expression_Get_Expression (GVE : LLVM.Types.Metadata_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:1166
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIGlobalVariableExpressionGetExpression";

  --*
  -- * Get the metadata of the file associated with a given variable.
  -- * \param Var     The variable object.
  -- *
  -- * @see DIVariable::getFile()
  --  

   function DI_Variable_Get_File (Var : LLVM.Types.Metadata_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:1175
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIVariableGetFile";

  --*
  -- * Get the metadata of the scope associated with a given variable.
  -- * \param Var     The variable object.
  -- *
  -- * @see DIVariable::getScope()
  --  

   function DI_Variable_Get_Scope (Var : LLVM.Types.Metadata_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:1183
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIVariableGetScope";

  --*
  -- * Get the source line where this \c DIVariable is declared.
  -- * \param Var     The DIVariable.
  -- *
  -- * @see DIVariable::getLine()
  --  

   function DI_Variable_Get_Line (Var : LLVM.Types.Metadata_T) return unsigned  -- install/include/llvm-c/DebugInfo.h:1191
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIVariableGetLine";

  --*
  -- * Create a new temporary \c MDNode.  Suitable for use in constructing cyclic
  -- * \c MDNode structures. A temporary \c MDNode is not uniqued, may be RAUW'd,
  -- * and must be manually deleted with \c LLVMDisposeTemporaryMDNode.
  -- * \param Ctx            The context in which to construct the temporary node.
  -- * \param Data           The metadata elements.
  -- * \param NumElements    Number of metadata elements.
  --  

   function Temporary_MD_Node
     (Ctx : LLVM.Types.Context_T;
      Data : System.Address;
      Num_Elements : stddef_h.size_t) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:1201
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMTemporaryMDNode";

  --*
  -- * Deallocate a temporary node.
  -- *
  -- * Calls \c replaceAllUsesWith(nullptr) before deleting, so any remaining
  -- * references will be reset.
  -- * \param TempNode    The temporary metadata node.
  --  

   procedure Dispose_Temporary_MD_Node (Temp_Node : LLVM.Types.Metadata_T)  -- install/include/llvm-c/DebugInfo.h:1211
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDisposeTemporaryMDNode";

  --*
  -- * Replace all uses of temporary metadata.
  -- * \param TempTargetMetadata    The temporary metadata node.
  -- * \param Replacement           The replacement metadata node.
  --  

   procedure Metadata_Replace_All_Uses_With (Temp_Target_Metadata : LLVM.Types.Metadata_T; Replacement : LLVM.Types.Metadata_T)  -- install/include/llvm-c/DebugInfo.h:1218
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMMetadataReplaceAllUsesWith";

  --*
  -- * Create a new descriptor for the specified global variable that is temporary
  -- * and meant to be RAUWed.
  -- * \param Scope       Variable scope.
  -- * \param Name        Name of the variable.
  -- * \param NameLen     The length of the C string passed to \c Name.
  -- * \param Linkage     Mangled  name of the variable.
  -- * \param LnkLen      The length of the C string passed to \c Linkage.
  -- * \param File        File where this variable is defined.
  -- * \param LineNo      Line number.
  -- * \param Ty          Variable Type.
  -- * \param LocalToUnit Boolean flag indicate whether this variable is
  -- *                    externally visible or not.
  -- * \param Decl        Reference to the corresponding declaration.
  -- * \param AlignInBits Variable alignment(or 0 if no alignment attr was
  -- *                    specified)
  --  

function DI_Create_Temp_Global_Variable_Fwd_Decl
     (Builder       : LLVM.Types.DI_Builder_T;
      Scope         : LLVM.Types.Metadata_T;
      Name          : String;
      Name_Len      : stddef_h.size_t;
      Linkage       : String;
      Lnk_Len       : stddef_h.size_t;
      File          : LLVM.Types.Metadata_T;
      Line_No       : unsigned;
      Ty            : LLVM.Types.Metadata_T;
      Local_To_Unit : Boolean;
      Decl          : LLVM.Types.Metadata_T;
      Align_In_Bits : stdint_h.uint32_t)
      return LLVM.Types.Metadata_T;

  --*
  -- * Insert a new llvm.dbg.declare intrinsic call before the given instruction.
  -- * \param Builder     The DIBuilder.
  -- * \param Storage     The storage of the variable to declare.
  -- * \param VarInfo     The variable's debug info descriptor.
  -- * \param Expr        A complex location expression for the variable.
  -- * \param DebugLoc    Debug info location.
  -- * \param Instr       Instruction acting as a location for the new intrinsic.
  --  

   function DI_Builder_Insert_Declare_Before
     (Builder : LLVM.Types.DI_Builder_T;
      Storage : LLVM.Types.Value_T;
      Var_Info : LLVM.Types.Metadata_T;
      Expr : LLVM.Types.Metadata_T;
      Debug_Loc : LLVM.Types.Metadata_T;
      Instr : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/DebugInfo.h:1253
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderInsertDeclareBefore";

  --*
  -- * Insert a new llvm.dbg.declare intrinsic call at the end of the given basic
  -- * block. If the basic block has a terminator instruction, the intrinsic is
  -- * inserted before that terminator instruction.
  -- * \param Builder     The DIBuilder.
  -- * \param Storage     The storage of the variable to declare.
  -- * \param VarInfo     The variable's debug info descriptor.
  -- * \param Expr        A complex location expression for the variable.
  -- * \param DebugLoc    Debug info location.
  -- * \param Block       Basic block acting as a location for the new intrinsic.
  --  

   function DI_Builder_Insert_Declare_At_End
     (Builder : LLVM.Types.DI_Builder_T;
      Storage : LLVM.Types.Value_T;
      Var_Info : LLVM.Types.Metadata_T;
      Expr : LLVM.Types.Metadata_T;
      Debug_Loc : LLVM.Types.Metadata_T;
      Block : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T  -- install/include/llvm-c/DebugInfo.h:1268
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderInsertDeclareAtEnd";

  --*
  -- * Insert a new llvm.dbg.value intrinsic call before the given instruction.
  -- * \param Builder     The DIBuilder.
  -- * \param Val         The value of the variable.
  -- * \param VarInfo     The variable's debug info descriptor.
  -- * \param Expr        A complex location expression for the variable.
  -- * \param DebugLoc    Debug info location.
  -- * \param Instr       Instruction acting as a location for the new intrinsic.
  --  

   function DI_Builder_Insert_Dbg_Value_Before
     (Builder : LLVM.Types.DI_Builder_T;
      Val : LLVM.Types.Value_T;
      Var_Info : LLVM.Types.Metadata_T;
      Expr : LLVM.Types.Metadata_T;
      Debug_Loc : LLVM.Types.Metadata_T;
      Instr : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/DebugInfo.h:1281
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderInsertDbgValueBefore";

  --*
  -- * Insert a new llvm.dbg.value intrinsic call at the end of the given basic
  -- * block. If the basic block has a terminator instruction, the intrinsic is
  -- * inserted before that terminator instruction.
  -- * \param Builder     The DIBuilder.
  -- * \param Val         The value of the variable.
  -- * \param VarInfo     The variable's debug info descriptor.
  -- * \param Expr        A complex location expression for the variable.
  -- * \param DebugLoc    Debug info location.
  -- * \param Block       Basic block acting as a location for the new intrinsic.
  --  

   function DI_Builder_Insert_Dbg_Value_At_End
     (Builder : LLVM.Types.DI_Builder_T;
      Val : LLVM.Types.Value_T;
      Var_Info : LLVM.Types.Metadata_T;
      Expr : LLVM.Types.Metadata_T;
      Debug_Loc : LLVM.Types.Metadata_T;
      Block : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T  -- install/include/llvm-c/DebugInfo.h:1299
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDIBuilderInsertDbgValueAtEnd";

  --*
  -- * Create a new descriptor for a local auto variable.
  -- * \param Builder         The DIBuilder.
  -- * \param Scope           The local scope the variable is declared in.
  -- * \param Name            Variable name.
  -- * \param NameLen         Length of variable name.
  -- * \param File            File where this variable is defined.
  -- * \param LineNo          Line number.
  -- * \param Ty              Metadata describing the type of the variable.
  -- * \param AlwaysPreserve  If true, this descriptor will survive optimizations.
  -- * \param Flags           Flags.
  -- * \param AlignInBits     Variable alignment.
  --  

function DI_Create_Auto_Variable
     (Builder         : LLVM.Types.DI_Builder_T;
      Scope           : LLVM.Types.Metadata_T;
      Name            : String;
      Name_Len        : stddef_h.size_t;
      File            : LLVM.Types.Metadata_T;
      Line_No         : unsigned;
      Ty              : LLVM.Types.Metadata_T;
      Always_Preserve : Boolean;
      Flags           : DI_Flags_T;
      Align_In_Bits   : stdint_h.uint32_t)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create a new descriptor for a function parameter variable.
  -- * \param Builder         The DIBuilder.
  -- * \param Scope           The local scope the variable is declared in.
  -- * \param Name            Variable name.
  -- * \param NameLen         Length of variable name.
  -- * \param ArgNo           Unique argument number for this variable; starts at 1.
  -- * \param File            File where this variable is defined.
  -- * \param LineNo          Line number.
  -- * \param Ty              Metadata describing the type of the variable.
  -- * \param AlwaysPreserve  If true, this descriptor will survive optimizations.
  -- * \param Flags           Flags.
  --  

function DI_Create_Parameter_Variable
     (Builder         : LLVM.Types.DI_Builder_T;
      Scope           : LLVM.Types.Metadata_T;
      Name            : String;
      Name_Len        : stddef_h.size_t;
      Arg_No          : unsigned;
      File            : LLVM.Types.Metadata_T;
      Line_No         : unsigned;
      Ty              : LLVM.Types.Metadata_T;
      Always_Preserve : Boolean;
      Flags           : DI_Flags_T)
      return LLVM.Types.Metadata_T;

  --*
  -- * Get the metadata of the subprogram attached to a function.
  -- *
  -- * @see llvm::Function::getSubprogram()
  --  

   function Get_Subprogram (Func : LLVM.Types.Value_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:1347
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetSubprogram";

  --*
  -- * Set the subprogram attached to a function.
  -- *
  -- * @see llvm::Function::setSubprogram()
  --  

   procedure Set_Subprogram (Func : LLVM.Types.Value_T; SP : LLVM.Types.Metadata_T)  -- install/include/llvm-c/DebugInfo.h:1354
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetSubprogram";

  --*
  -- * Get the line associated with a given subprogram.
  -- * \param Subprogram     The subprogram object.
  -- *
  -- * @see DISubprogram::getLine()
  --  

   function DI_Subprogram_Get_Line (Subprogram : LLVM.Types.Metadata_T) return unsigned  -- install/include/llvm-c/DebugInfo.h:1362
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDISubprogramGetLine";

  --*
  -- * Get the debug location for the given instruction.
  -- *
  -- * @see llvm::Instruction::getDebugLoc()
  --  

   function Instruction_Get_Debug_Loc (Inst : LLVM.Types.Value_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/DebugInfo.h:1369
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInstructionGetDebugLoc";

  --*
  -- * Set the debug location for the given instruction.
  -- *
  -- * To clear the location metadata of the given instruction, pass NULL to \p Loc.
  -- *
  -- * @see llvm::Instruction::setDebugLoc()
  --  

   procedure Instruction_Set_Debug_Loc (Inst : LLVM.Types.Value_T; Loc : LLVM.Types.Metadata_T)  -- install/include/llvm-c/DebugInfo.h:1378
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInstructionSetDebugLoc";

  --*
  -- * Obtain the enumerated type of a Metadata instance.
  -- *
  -- * @see llvm::Metadata::getMetadataID()
  --  

   function Get_Metadata_Kind (Metadata : LLVM.Types.Metadata_T) return Metadata_Kind_T  -- install/include/llvm-c/DebugInfo.h:1385
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetMetadataKind";

  --*
  -- * @}
  --  

end LLVM.Debug_Info;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
