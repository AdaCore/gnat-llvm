------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2023, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with GNATLLVM.GLType;  use GNATLLVM.GLType;
with GNATLLVM.GLValue; use GNATLLVM.GLValue;

package GNATLLVM.Builtins is

   --  When we want to create an overloaded intrinsic, we need to specify
   --  what operand signature the intrinsic has. The following are those
   --  that we currently support.

   type Overloaded_Intrinsic_Kind is
     (Unary, Binary, Ternary, Boolean_And_Data);

   function Build_Intrinsic
     (Kind : Overloaded_Intrinsic_Kind;
      Name : String;
      GT   : GL_Type) return GL_Value
     with Pre  => Is_Primitive_GL_Type (GT) and then Known_RM_Size (GT),
          Post => Present (Build_Intrinsic'Result);
   --  Build an intrinsic function of the specified type, name, and kind

   function Emit_Intrinsic_Call
     (N : N_Subprogram_Call_Id; Subp : Subprogram_Kind_Id) return GL_Value;
   --  If Subp is an intrinsic that we know how to handle, emit the LLVM
   --  for it and return the result. Otherwise, No_GL_Value.

   function Get_Default_Alloc_Fn return GL_Value
     with Post => Present (Get_Default_Alloc_Fn'Result);
   --  Get default function to use for allocating memory

   function Get_Default_Free_Fn return GL_Value
     with Post => Present (Get_Default_Free_Fn'Result);
   --  Get default function to use for freeing memory

   function Get_Memory_Compare_Fn return GL_Value
     with Post => Present (Get_Memory_Compare_Fn'Result);
   --  Get function to use to compare memory

   function Get_Stack_Save_Fn return GL_Value
     with Post => Present (Get_Stack_Save_Fn'Result);
   --  Get function to save stack pointer

   function Get_Stack_Restore_Fn return GL_Value
     with Post => Present (Get_Stack_Restore_Fn'Result);
   --  Get function to restore stack pointer

   function Get_Expect_Fn return GL_Value
     with Post => Present (Get_Expect_Fn'Result);
   --  Get function corresponing to llvm.expect

   function Get_Tramp_Init_Fn   return GL_Value
     with Post => Present (Get_Tramp_Init_Fn'Result);
   function Get_Tramp_Adjust_Fn return GL_Value
     with Post => Present (Get_Tramp_Adjust_Fn'Result);
   --  Get functions to create and adjust trampolines

   function Get_Get_Address_Fn return GL_Value
     with Post => Present (Get_Get_Address_Fn'Result);
   --  Get function to obtain the address from a pointer

   function Get_Set_Address_Fn return GL_Value
     with Post => Present (Get_Set_Address_Fn'Result);
   --  Get function to set the address of a pointer

   procedure Initialize;
   --  Initialize module

end GNATLLVM.Builtins;
