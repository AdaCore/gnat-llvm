------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

with Atree; use Atree;
with Einfo; use Einfo;
with Sinfo; use Sinfo;
with Table; use Table;
with Types; use Types;
with Uintp; use Uintp;

with LLVM.Core;  use LLVM.Core;
with LLVM.Types; use LLVM.Types;

with GNATLLVM.Environment; use GNATLLVM.Environment;

package GNATLLVM.Subprograms is

   package Nested_Functions_Table is new Table.Table
     (Table_Component_Type => Node_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "Nested_Function_Table");
   --  Table of nested functions to elaborate

   --  When we want to create an overloaded intrinsic, we need to specify
   --  what operand signature the intrinsic has.  The following are those
   --  that we currently support.

   type Overloaded_Intrinsic_Kind is
     (Unary, Binary, Overflow, Memcpy, Memset);

   function Count_Params (E : Entity_Id) return Nat
     with Pre => Present (E);
   --  Return a count of the number of parameters of E, which is either
   --  a subprogram or a subprogram type.

   function Create_Subprogram_Type_From_Spec (N : Node_Id) return Type_T
     with Pre  => Present (N),
          Post => (Get_Type_Kind (Create_Subprogram_Type_From_Spec'Result) =
                   Function_Type_Kind);

   function Create_Subprogram_Type_From_Entity
     (TE : Entity_Id; Takes_S_Link  : Boolean) return Type_T
     with Pre  => Ekind (TE) = E_Subprogram_Type,
          Post => (Get_Type_Kind (Create_Subprogram_Type_From_Entity'Result) =
                   Function_Type_Kind);

   function Create_Subprogram_Access_Type (T : Type_T) return Type_T
     with Pre  => Present (T),
          Post => Present (Create_Subprogram_Access_Type'Result);
   --  Return a structure type that embeds Subp_Type and a static link pointer

   function Build_Intrinsic
     (Kind : Overloaded_Intrinsic_Kind;
      Name : String;
      TE   : Entity_Id) return GL_Value
     with Pre => Is_Type (TE) and then RM_Size (TE) /= No_Uint,
          Post => Present (Build_Intrinsic'Result);
   --  Build an intrinsic function of the specified type, name, and kind

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

   function Get_LCH_Fn return GL_Value
     with Post => Present (Get_LCH_Fn'Result);
   --  Get function for our last-chance handler

   function Get_Static_Link (N : Entity_Id) return GL_Value
     with Pre  => Present (N),
          Post => Present (Get_Static_Link'Result);
   --  Build and return the static link to pass to a call to Node

   function Emit_Call (N : Node_Id) return GL_Value
     with Pre  => Nkind (N) in N_Subprogram_Call,
          Post => Present (Emit_Call'Result);
   --  Helper for Emit/Emit_Expression: compile a call statement/expression and
   --  return its result value.

   procedure Emit_LCH_Call_If (V : GL_Value; N : Node_Id)
     with Pre => Present (V) and then Present (N);
   --  Call the last change helper if V evaluates to True

   procedure Emit_LCH_Call (N : Node_Id)
     with Pre => Present (N);
   --  Generate a call to __gnat_last_chance_handler

   procedure Emit_Elab_Proc
     (N : Node_Id; Stmts : Node_Id; CU : Node_Id; Suffix : String)
     with Pre => Nkind_In (N, N_Package_Specification, N_Package_Body)
                 and then Suffix'Length = 1;
   --  Emit code for the elaboration procedure for N.  Suffix is either "s"
   --  or "b".  CU is the corresponding N_Compilation_Unit on which we set
   --  Has_No_Elaboration_Code if there isn't any.  Stmts, if Present, is
   --  an N_Handled_Sequence_Of_Statements that also have to be in the
   --  elaboration procedure.

   procedure Emit_One_Body (N : Node_Id)
     with Pre => Present (N);
   --  Generate code for one given subprogram body

   function Emit_Subprogram_Decl (N : Node_Id) return GL_Value
     with Post => Present (Emit_Subprogram_Decl'Result);
   --  Compile a subprogram declaration, save the corresponding LLVM value to
   --  the environment and return it.

   procedure Emit_Subprogram_Body (N : Node_Id)
     with Pre => Present (N);
   --  Compile a subprogram body and save it in the environment

   function Node_Enclosing_Subprogram (N : Node_Id) return Node_Id
     with Pre  => Present (N),
          Post => Present (Node_Enclosing_Subprogram'Result);
   --  Return the enclosing subprogram containing Node

   function Subp_Ptr (N : Node_Id) return GL_Value
     with Pre  => Present (N), Post => Present (Subp_Ptr'Result);
   --  Return the subprogram pointer associated with Node

end GNATLLVM.Subprograms;
