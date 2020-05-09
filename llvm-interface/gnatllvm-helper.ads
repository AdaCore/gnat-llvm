------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2020, AdaCore                     --
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

with LLVM.Core;       use LLVM.Core;
with LLVM.Debug_Info; use LLVM.Debug_Info;

with GNATLLVM.GLValue; use GNATLLVM.GLValue;
with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

package GNATLLVM.Helper is

   --  This package contains definitions for "helper" subprograms that do
   --  some small amounts of maniplations and then call the lower-level
   --  LLVM function.  If the major purpose of the LLVM function is to
   --  generate code, the helper subprogram should be in
   --  GNATLLVM.Instructions and if it's to perform operations on a
   --  GL_Value that LLVM supports on a Value_T, it should be in
   --  GNATLLVM.GLValue.

   function Create_TBAA_Scalar_Type_Node
     (Name : String; Size : ULL; Parent : Metadata_T) return Metadata_T
   is
     (Create_TBAA_Scalar_Type_Node (Context, MD_Builder, Name, Size, Parent))
     with Pre  => Present (Parent),
          Post => Present (Create_TBAA_Scalar_Type_Node'Result);

   function Create_TBAA_Struct_Type_Node
     (Name    : String;
      Size    : ULL;
      Parent  : Metadata_T;
      Offsets : ULL_Array;
      Sizes   : ULL_Array;
      TBAAs   : Metadata_Array) return Metadata_T
   is
      (Create_TBAA_Struct_Type_Node
         (Context, MD_Builder, Name, Size, Offsets'Length,
          Parent, TBAAs'Address, Offsets'Address, Sizes'Address))
     with Pre  => Present (Parent) and then Sizes'First = Offsets'First
                  and then Sizes'Last  = Offsets'Last
                  and then TBAAs'First = Offsets'First
                  and then TBAAs'Last  = Offsets'Last
                  and then (for all T of TBAAs   => Present (T)),
          Post => Present (Create_TBAA_Struct_Type_Node'Result);

   function Create_TBAA_Struct_Node
     (TBAAs   : Metadata_Array;
      Offsets : ULL_Array;
      Sizes   : ULL_Array) return Metadata_T
   is
      (Create_TBAA_Struct_Node
         (Context, MD_Builder, TBAAs'Length, TBAAs'Address, Offsets'Address,
          Sizes'Address))
     with Pre  => TBAAs'First = Offsets'First
                  and then TBAAs'Last  = Offsets'Last
                  and then TBAAs'First = Sizes'First
                  and then TBAAs'Last  = Sizes'Last
                  and then (for all T of TBAAs   => Present (T)),
          Post => Present (Create_TBAA_Struct_Node'Result);

   function Get_Subprogram_Debug_Metadata (V : GL_Value) return Metadata_T is
     (Get_Subprogram (+V))
     with Pre => Is_A_Function (V), Inline;

   procedure Set_Subprogram_Debug_Metadata (V : GL_Value; M : Metadata_T)
     with Pre => Is_A_Function (V) and then Present (M), Inline;

   function Value_As_Metadata (V : GL_Value) return Metadata_T is
     (Value_As_Metadata (+V))
     with Pre => Present (V), Post => Present (Value_As_Metadata'Result);

   function Metadata_As_Value (M : Metadata_T) return Value_T is
     (Metadata_As_Value (Context, M))
     with Pre => Present (M), Post => Present (Metadata_As_Value'Result);

   function Const_32_As_Metadata (U : Uint) return Metadata_T is
     (Value_As_Metadata (+Const_Int_32 (U)))
     with Pre => Present (U), Post => Present (Const_32_As_Metadata'Result);

   function MD_String (S : String) return Metadata_T is
     (MD_String_In_Context2 (Context, S, S'Length))
     with Post => Present (MD_String'Result);

   function MD_Node (MDs : Metadata_Array) return Metadata_T is
     (MD_Node_In_Context2 (Context, MDs'Address, MDs'Length))
     with Pre  => (for all M of MDs => Present (M)),
          Post => Present (MD_Node'Result);

   procedure Add_Named_Metadata_Operand (Name : String; V : Value_T)
     with Pre => Present (V), Inline;

   procedure Add_Named_Metadata_Operand (Name : String; M : Metadata_T)
     with Pre => Present (M), Inline;

end GNATLLVM.Helper;
