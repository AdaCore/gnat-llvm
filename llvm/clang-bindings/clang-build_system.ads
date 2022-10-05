pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with Clang.CX_Error_Code;
with System;

package Clang.Build_System is

  --==-- clang-c/BuildSystem.h - Utilities for use by build systems -*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header provides various utilities for use by build systems.           *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * \defgroup BUILD_SYSTEM Build system utilities
  -- * @{
  --  

  --*
  -- * Return the timestamp for use with Clang's
  -- * \c -fbuild-session-timestamp= option.
  --  

   function Get_Build_Session_Timestamp return Extensions.unsigned_long_long  -- install/include/clang-c/BuildSystem.h:33
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getBuildSessionTimestamp";

  --*
  -- * Object encapsulating information about overlaying virtual
  -- * file/directories over the real file system.
  --  

   type Virtual_File_Overlay_Impl_T is null record;   -- incomplete struct

   type Virtual_File_Overlay_T is access all Virtual_File_Overlay_Impl_T;  -- install/include/clang-c/BuildSystem.h:39

  --*
  -- * Create a \c CXVirtualFileOverlay object.
  -- * Must be disposed with \c clang_VirtualFileOverlay_dispose().
  -- *
  -- * \param options is reserved, always pass 0.
  --  

   function Virtual_File_Overlay_Create (Options : unsigned) return Virtual_File_Overlay_T  -- install/include/clang-c/BuildSystem.h:48
   with Import => True, 
        Convention => C, 
        External_Name => "clang_VirtualFileOverlay_create";

  --*
  -- * Map an absolute virtual file path to an absolute real one.
  -- * The virtual path must be canonicalized (not contain "."/"..").
  -- * \returns 0 for success, non-zero to indicate an error.
  --  

function Virtual_File_Overlay_Add_File_Mapping
     (Arg_1        : Virtual_File_Overlay_T;
      Virtual_Path : String;
      Real_Path    : String)
      return Clang.CX_Error_Code.Error_Code_T;

  --*
  -- * Set the case sensitivity for the \c CXVirtualFileOverlay object.
  -- * The \c CXVirtualFileOverlay object is case-sensitive by default, this
  -- * option can be used to override the default.
  -- * \returns 0 for success, non-zero to indicate an error.
  --  

   function Virtual_File_Overlay_Set_Case_Sensitivity (Arg_1 : Virtual_File_Overlay_T; Case_Sensitive : int) return Clang.CX_Error_Code.Error_Code_T  -- install/include/clang-c/BuildSystem.h:67
   with Import => True, 
        Convention => C, 
        External_Name => "clang_VirtualFileOverlay_setCaseSensitivity";

  --*
  -- * Write out the \c CXVirtualFileOverlay object to a char buffer.
  -- *
  -- * \param options is reserved, always pass 0.
  -- * \param out_buffer_ptr pointer to receive the buffer pointer, which should be
  -- * disposed using \c clang_free().
  -- * \param out_buffer_size pointer to receive the buffer size.
  -- * \returns 0 for success, non-zero to indicate an error.
  --  

   function Virtual_File_Overlay_Write_To_Buffer
     (Arg_1 : Virtual_File_Overlay_T;
      Options : unsigned;
      Out_Buffer_Ptr : System.Address;
      Out_Buffer_Size : access unsigned) return Clang.CX_Error_Code.Error_Code_T  -- install/include/clang-c/BuildSystem.h:80
   with Import => True, 
        Convention => C, 
        External_Name => "clang_VirtualFileOverlay_writeToBuffer";

  --*
  -- * free memory allocated by libclang, such as the buffer returned by
  -- * \c CXVirtualFileOverlay() or \c clang_ModuleMapDescriptor_writeToBuffer().
  -- *
  -- * \param buffer memory pointer to free.
  --  

   procedure Free (Buffer : System.Address)  -- install/include/clang-c/BuildSystem.h:90
   with Import => True, 
        Convention => C, 
        External_Name => "clang_free";

  --*
  -- * Dispose a \c CXVirtualFileOverlay object.
  --  

   procedure Virtual_File_Overlay_Dispose (Arg_1 : Virtual_File_Overlay_T)  -- install/include/clang-c/BuildSystem.h:95
   with Import => True, 
        Convention => C, 
        External_Name => "clang_VirtualFileOverlay_dispose";

  --*
  -- * Object encapsulating information about a module.map file.
  --  

   type Module_Map_Descriptor_Impl_T is null record;   -- incomplete struct

   type Module_Map_Descriptor_T is access all Module_Map_Descriptor_Impl_T;  -- install/include/clang-c/BuildSystem.h:100

  --*
  -- * Create a \c CXModuleMapDescriptor object.
  -- * Must be disposed with \c clang_ModuleMapDescriptor_dispose().
  -- *
  -- * \param options is reserved, always pass 0.
  --  

   function Module_Map_Descriptor_Create (Options : unsigned) return Module_Map_Descriptor_T  -- install/include/clang-c/BuildSystem.h:109
   with Import => True, 
        Convention => C, 
        External_Name => "clang_ModuleMapDescriptor_create";

  --*
  -- * Sets the framework module name that the module.map describes.
  -- * \returns 0 for success, non-zero to indicate an error.
  --  

function Module_Map_Descriptor_Set_Framework_Module_Name
     (Arg_1 : Module_Map_Descriptor_T;
      Name  : String)
      return Clang.CX_Error_Code.Error_Code_T;

  --*
  -- * Sets the umbrella header name that the module.map describes.
  -- * \returns 0 for success, non-zero to indicate an error.
  --  

function Module_Map_Descriptor_Set_Umbrella_Header
     (Arg_1 : Module_Map_Descriptor_T;
      Name  : String)
      return Clang.CX_Error_Code.Error_Code_T;

  --*
  -- * Write out the \c CXModuleMapDescriptor object to a char buffer.
  -- *
  -- * \param options is reserved, always pass 0.
  -- * \param out_buffer_ptr pointer to receive the buffer pointer, which should be
  -- * disposed using \c clang_free().
  -- * \param out_buffer_size pointer to receive the buffer size.
  -- * \returns 0 for success, non-zero to indicate an error.
  --  

   function Module_Map_Descriptor_Write_To_Buffer
     (Arg_1 : Module_Map_Descriptor_T;
      Options : unsigned;
      Out_Buffer_Ptr : System.Address;
      Out_Buffer_Size : access unsigned) return Clang.CX_Error_Code.Error_Code_T  -- install/include/clang-c/BuildSystem.h:137
   with Import => True, 
        Convention => C, 
        External_Name => "clang_ModuleMapDescriptor_writeToBuffer";

  --*
  -- * Dispose a \c CXModuleMapDescriptor object.
  --  

   procedure Module_Map_Descriptor_Dispose (Arg_1 : Module_Map_Descriptor_T)  -- install/include/clang-c/BuildSystem.h:144
   with Import => True, 
        Convention => C, 
        External_Name => "clang_ModuleMapDescriptor_dispose";

  --*
  -- * @}
  --  

end Clang.Build_System;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
