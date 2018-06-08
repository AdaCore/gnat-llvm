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

with Sinfo; use Sinfo;
with Uintp; use Uintp;

with GNATLLVM.GLValue;     use GNATLLVM.GLValue;

package GNATLLVM.Subprograms is

   --  When we want to create an overloaded intrinsic, we need to specify
   --  what operand signature the intrinsic has.  The following are those
   --  that we currently support.

   type Overloaded_Intrinsic_Kind is
     (Unary, Binary, Overflow, Memcpy, Memset);

   function Count_Params (E : Entity_Id) return Nat
     with Pre => Present (E);
   --  Return a count of the number of parameters of E, which is either
   --  a subprogram or a subprogram type.

   function Create_Subprogram_Type (Def_Ident  : Entity_Id) return Type_T
     with Pre  => Present (Def_Ident),
          Post => Present (Create_Subprogram_Type'Result);
   --  Create subprogram type.  Def_Ident can either be a subprogram,
   --  in which case a subprogram type will be created from it or a
   --  subprogram type directly.

   function Create_Subprogram_Access_Type return Type_T
     with Post => Present (Create_Subprogram_Access_Type'Result);
   --  Return a structure type that embeds Subp_Type and a static link pointer

   function Build_Intrinsic
     (Kind : Overloaded_Intrinsic_Kind;
      Name : String;
      TE   : Entity_Id) return GL_Value
     with Pre => Is_Type (TE) and then RM_Size (TE) /= No_Uint,
          Post => Present (Build_Intrinsic'Result);
   --  Build an intrinsic function of the specified type, name, and kind

   function Add_Global_Function
     (S          : String;
      Subp_Type  : Type_T;
      TE         : Entity_Id;
      Can_Throw  : Boolean := False;
      Can_Return : Boolean := True) return GL_Value
     with Pre => S'Length > 0 and then Present (Subp_Type)
                 and then Present (TE);
   --  Create a function with the give name and type, but handling the case
   --  where we're also compiling a function with that name.  By default,
   --  these functions can return, but will not throw an exception, but
   --  this can be changed.

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

   function Get_From_Activation_Record (E : Entity_Id) return GL_Value
     with Pre => not Is_Type (E);
   --  Checks whether E is present in the current activation record and
   --  returns an LValue pointing to the value of the object if so.

   function Get_Static_Link (N : Node_Id) return GL_Value
     with Pre  => Present (N),
          Post => Present (Get_Static_Link'Result);
   --  Build and return the static link to pass to a call to N

   function Emit_Call (N : Node_Id) return GL_Value
     with Pre  => Nkind (N) in N_Subprogram_Call;
   --  Compile a call statement/expression and return its result
   --  value.  If this is calling a procedure, there will be no return value.

   procedure Call_Alloc_Dealloc (Proc : Entity_Id; Args : GL_Value_Array)
     with Pre => Ekind (Proc) = E_Procedure;
   --  Proc is a Procedure_To_Call for an allocation or deallocation and Args
   --  are its arguments.  See if Proc needs a static link and pass one, if so.

   procedure Emit_LCH_Call_If (V : GL_Value; N : Node_Id)
     with Pre => Present (V) and then Present (N);
   --  Call the last change helper if V evaluates to True

   procedure Emit_LCH_Call (N : Node_Id)
     with Pre => Present (N);
   --  Generate a call to __gnat_last_chance_handler

   procedure Add_To_Elab_Proc (N : Node_Id; For_Type : Entity_Id := Empty)
     with Pre => Library_Level and then Present (N)
                 and then (No (For_Type) or else Is_Type (For_Type));
   --  Add N to the elaboration table if it's not already there.  We assume
   --  here that if it's already there, it was the last one added.  If
   --  For_Type is Present, elaborate N as an expression, convert to
   --  For_Type, and save it as the value for N.

   procedure Emit_Elab_Proc
     (N : Node_Id; Stmts : Node_Id; CU : Node_Id; Suffix : String)
     with Pre => Library_Level
                 and then Nkind_In (N, N_Package_Specification, N_Package_Body)
                 and then Suffix'Length = 1;
   --  Emit code for the elaboration procedure for N.  Suffix is either "s"
   --  or "b".  CU is the corresponding N_Compilation_Unit on which we set
   --  Has_No_Elaboration_Code if there is any.  Stmts, if Present, is
   --  an N_Handled_Sequence_Of_Statements that also have to be in the
   --  elaboration procedure.

   procedure Emit_One_Body (N : Node_Id)
     with Pre => Present (N);
   --  Generate code for one given subprogram body

   function Create_Subprogram (Def_Ident : Entity_Id) return GL_Value
     with Pre => Ekind (Def_Ident) in Subprogram_Kind;
   --  Create and save an LLVM object for Def_Ident, a subprogram

   function Emit_Subprogram_Decl (N : Node_Id) return GL_Value
     with Post => Present (Emit_Subprogram_Decl'Result);
   --  Compile a subprogram declaration, creating the subprogram if not
   --  already done.  Return the subprogram value.

   procedure Emit_Subprogram_Body (N : Node_Id)
     with Pre => Present (N);
   --  Compile a subprogram body and save it in the environment

   procedure Emit_Return_Statement (N : Node_Id)
     with Pre => Nkind (N) = N_Simple_Return_Statement;
   --  Emit code for a return statement

   function Subp_Ptr (N : Node_Id) return GL_Value
     with Pre  => Present (N), Post => Present (Subp_Ptr'Result);
   --  Return the subprogram pointer associated with Node

   procedure Enter_Subp (Func : GL_Value)
     with Pre  => Present (Func) and then Library_Level,
          Post => not Library_Level;
   --  Create an entry basic block for this subprogram and position
   --  the builder at its end. Mark that we're in a subprogram.  To be
   --  used when starting the compilation of a subprogram body.

   procedure Leave_Subp
     with Pre  => not Library_Level,
          Post => Library_Level;
   --  Indicate that we're no longer compiling a subprogram

   function Library_Level return Boolean;
   --  Return True if we're at library level

   function Create_Basic_Block (Name : String := "") return Basic_Block_T
     with Post => Present (Create_Basic_Block'Result);
   --  Create a basic block in the current function

   procedure Initialize;
   --  Initialize module

   Current_Subp : Entity_Id := Empty;
   --  The spec entity for the subprogram currently being compiled

   Current_Func             : GL_Value := No_GL_Value;
   --  Pointer to the current function

   Activation_Rec_Param     : GL_Value;
   --  Parameter to this subprogram, if any, that represents an
   --  activtion record.

   Return_Address_Param     : GL_Value;
   --  Parameter to this subprogram, if any, that represent the address
   --  to which we are to copy the return value

   In_Elab_Proc             : Boolean := False;
   --  True if we're in the process of emitting the code for an elaboration
   --  procedure.

end GNATLLVM.Subprograms;
