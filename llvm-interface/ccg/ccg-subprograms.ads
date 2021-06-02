------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

with LLVM.Core;  use LLVM.Core;

with CCG.Blocks; use CCG.Blocks;
with CCG.Helper; use CCG.Helper;
with CCG.Strs;   use CCG.Strs;

with Table;

package CCG.Subprograms is

   --  This package contains subprograms and data used in the handling of
   --  subprograms.

   Subprogram_Idx_Low_Bound  : constant := 300_000_000;
   Subprogram_Idx_High_Bound : constant := 399_999_999;
   type Subprogram_Idx is
     range Subprogram_Idx_Low_Bound .. Subprogram_Idx_High_Bound;
   Subprogram_Idx_Start      : constant Subprogram_Idx :=
     Subprogram_Idx_Low_Bound + 1;

   --  For each subprogram, we record the first and last decl and statement
   --  belonging to that subprogram.

   type Subprogram_Data is record
      Func       : Value_T;
      First_Decl : Local_Decl_Idx;
      Last_Decl  : Local_Decl_Idx;
      First_Stmt : Stmt_Idx;
      Last_Stmt  : Stmt_Idx;
   end record;

   package Subprogram_Table is new Table.Table
     (Table_Component_Type => Subprogram_Data,
      Table_Index_Type     => Subprogram_Idx,
      Table_Low_Bound      => Subprogram_Idx_Start,
      Table_Initial        => 50,
      Table_Increment      => 50,
      Table_Name           => "Subprogram_Table");

   procedure New_Subprogram (V : Value_T)
     with Pre => Present (Is_A_Function (V));
   --  Switch to a new subprogram V

   function Curr_Func return Value_T
     with Post => Present (Curr_Func'Result);
   --  Return the decl for the function being converted to C

   procedure Call_Instruction (V : Value_T; Ops : Value_Array)
     with Pre => Is_A_Call_Inst (V);
   --  Process a call instruction

   procedure Return_Instruction (V : Value_T; Op : Value_T)
     with Pre => Is_A_Return_Inst (V);
   --  Process a call instruction

   procedure Declare_Subprogram (V : Value_T)
     with Pre => Is_A_Function (V);
   --  Write a declaration for subprogram V

   procedure Generate_C_For_Subprogram (V : Value_T)
     with Pre => Is_A_Function (V);
   --  Generate the C statements and decls for V, a function

   procedure Write_Function_Type_Typedef (T : Type_T)
     with Pre => Get_Type_Kind (Get_Element_Type (T)) = Function_Type_Kind;
   --  Write a typedef for T, which is a pointer to a function type

   function Function_Proto (V : Value_T; Extern : Boolean := False) return Str
     with Pre  => Is_A_Function (V),
          Post => Present (Function_Proto'Result);
   --  Return the prototype for function V. Extern is True if we're writing
   --  a declaration of the function (meaning we don't have parameter names).

   function Function_Proto (T : Type_T; S : Str) return Str
     with Pre  => Get_Type_Kind (T) = Function_Type_Kind
                  and then Present (S),
          Post => Present (Function_Proto'Result);
   --  Return the prototype for function type T, using S for where the name
   --  of the function would be.

   procedure Write_Subprograms;
   --  Write all the decls and statements for all subprograms

end CCG.Subprograms;
