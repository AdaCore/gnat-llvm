------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020, AdaCore                          --
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

with CCG.Helper; use CCG.Helper;
with CCG.Tables; use CCG.Tables;

package CCG.Aggregates is

   --  This package contains routines used to process aggregate data,
   --  which are arrays and structs.

   --  When creating LLVM structs, we record what each field in the
   --  struct is for. We first say what each field is for and then
   --  say what struct it was for. We specify the type so tha we can
   --  link those two. Doing it in the opposite order would make things
   --  simpler for us, but complicate the record creation process.

   procedure Set_Field_Name_Info
     (TE          : Entity_Id;
      Idx         : Nat;
      Name        : Name_Id := No_Name;
      Is_Padding  : Boolean := False;
      Is_Bitfield : Boolean := False)
     with Pre => Is_Type (TE);
   --  Say what field Idx in the next struct is used for.  This is in
   --  the processing of TE.

   procedure Set_Struct (TE : Entity_Id; T : Type_T)
     with Pre => Is_Type (TE) and then Present (T);
   --  Indicate that the previous calls to Set_Field_Name_Info for TE
   --  were for LLVM struct type T.

   function Get_Field_Name (T : Type_T; Idx : Nat) return Str
     with Pre  => Get_Type_Kind (T) = Struct_Type_Kind,
          Post => Present (Get_Field_Name'Result);
   --  Return a name to use for field Idx of LLVM struct T

   procedure Write_Struct_Typedef (T : Type_T)
     with Pre => Get_Type_Kind (T) = Struct_Type_Kind;
   --  Write a typedef for T, a struct type

   procedure Write_Array_Typedef (T : Type_T)
     with Pre => Get_Type_Kind (T) = Array_Type_Kind;
   --  Write a typedef for T, an array type

   function Extract_Value_Instruction (V : Value_T; Op : Value_T) return Str
     with Pre  => Get_Opcode (V) = Op_Extract_Value and then Present (Op),
          Post => Present (Extract_Value_Instruction'Result);
   --  Return the result of an extractvalue instruction V

   procedure Insert_Value_Instruction (V, Aggr, Op : Value_T)
     with Pre => Get_Opcode (V) = Op_Insert_Value and then Present (Aggr)
                 and then Present (Op);
   --  Process an insertvalue instruction V with an initial value of Aggr
   --  and assigning Op to the component.

   procedure GEP_Instruction (V : Value_T; Ops : Value_Array)
     with Pre  => Get_Opcode (V) = Op_Get_Element_Ptr
                  and then Ops'Length > 1;
   --  Process a GEP instruction

end CCG.Aggregates;
