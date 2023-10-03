------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2023, AdaCore                     --
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

with Interfaces.C; use Interfaces.C;

with LLVM.Core; use LLVM.Core;

with Set_Targ; use Set_Targ;

with CCG.Helper; use CCG.Helper;
with CCG.Strs;   use CCG.Strs;
with CCG.Utils;  use CCG.Utils;

package CCG.Aggregates is

   --  This package contains routines used to process aggregate data,
   --  which are arrays and structs.

   --  For reasons discussed in the spec of GNATLLVM.Records.Create, we
   --  create most LLVM struct types as packed. However, we would prefer to
   --  not have most records packed in the C output, both because this
   --  makes it harder to read and to avoid warnings if we take the address
   --  of a packed field. Likewise, we'd prefer to avoid outputting any
   --  padding fields since they clutter the output. So we need to compute
   --  whether or not the record actually needs to be packed. It does if
   --  one of the following is true:
   --
   --  - a field is at a position that is less than that determined by
   --    the default alignment of the previous field
   --
   --  - the alignment of the record is less than that of its default
   --    alignment
   --
   --  - the struct is part of an unconstrained record
   --
   --  In order to make these determinations, we need a function that
   --  computes the default alignment of a type, which recurses over all
   --  fields in the struct. Because this is exponential in the depth of
   --  nesting of structs, we could use a map to cache the intermediate
   --  alignment, but multiple deep structures are rare.
   --
   --  In addition to the default alignment that a record would have if it
   --  weren't packed, we have the actual alignment of the record, which is
   --  the default if we aren't going to pack it and byte-alignment if we
   --  are.
   --
   --  We also record whether, for the non-packed case, we need to include
   --  padding fields. We do if the padding fields produce additional
   --  space. So there are three cases:
   --
   --  (1) Each field is at its natural position, taking into account
   --      alignment. We don't pack the record or output padding fields.
   --  (2) At least one field is at a lower offset than its natural
   --      position. We must pack the record and output padding fields.
   --  (3) At least one field is at a higher offset than its natural position
   --      but none is at a lower offset. We don't pack the record, but
   --      do output padding fields.
   --
   --  In computing alignment, we use the ABI-returned alignment for
   --  elementary types, which can be specified by the user in the data
   --  layout string, assume that the default alignment of a struct is the
   --  maximum alignments of all the fields in the struct, and assume that
   --  the alignment of an array is that of its component.

   type Struct_Out_Style_T is (Normal, Padding, Packed);
   --  Says how we want to output the struct. Packed also means that we
   --  output padding fields.

   procedure Need_IXX_Struct (J : Nat)
     with Pre => J < Long_Long_Size, Inline;
   --  Show that we need a struct to support a load or store of an iXX type

   procedure Output_IXX_Structs;
   --  Output any needed such structs

   function Default_Alignment (T : Type_T) return Nat
     with Pre => Present (T);
   function Struct_Out_Style (T : Type_T) return Struct_Out_Style_T
     with Pre => Is_Struct_Type (T);
   function Actual_Alignment (T : Type_T) return Nat is
     ((if   Is_Struct_Type (T) and then Struct_Out_Style (T) = Packed
       then BPU else Default_Alignment (T)))
     with Pre => Present (T);

   procedure Output_Struct_Typedef (T : Type_T; Incomplete : Boolean := False)
     with Pre => Is_Struct_Type (T);
   --  Output a typedef for T, a struct type. If Incomplete, only output the
   --  initial struct definition, not the fields.

   procedure Error_If_Cannot_Pack (T : Type_T)
     with Pre => Present (T);
   --  We're using T in a context where it matters if its size isn't a
   --  multiple of its alignment, so give an error if that's that case
   --  and we don't support packing.

   procedure Output_Array_Typedef (T : Type_T)
     with Pre => Is_Array_Type (T);
   --  Output a typedef for T, an array type

   procedure Maybe_Output_Array_Return_Typedef (T : Type_T)
     with Pre => Is_Array_Type (T);
   --  If we haven't done so already, output the typedef for the struct that
   --  will be used as the actual return type if T were the return type of
   --  a function. This is known to be the name of T with a suffixed "_R".

   function Extract_Value_Instruction (V : Value_T; Op : Value_T) return Str
     with Pre  => Is_A_Extract_Value_Inst (V) and then Present (Op),
          Post => Present (Extract_Value_Instruction'Result);
   --  Return the result of an extractvalue instruction V

   procedure Insert_Value_Instruction (V, Aggr, Op : Value_T)
     with Pre => Is_A_Insert_Value_Inst (V) and then Present (Aggr)
                 and then Present (Op);
   --  Process an insertvalue instruction V with an initial value of Aggr
   --  and assigning Op to the component.

   procedure GEP_Instruction (V : Value_T; Ops : Value_Array)
     with Pre  => Get_Opcode (V) = Op_Get_Element_Ptr and then Ops'Length > 1;
   --  Process a GEP instruction or a GEP constant expression

end CCG.Aggregates;
