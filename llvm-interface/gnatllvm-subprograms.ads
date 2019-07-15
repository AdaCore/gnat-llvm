------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2019, AdaCore                     --
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

with GNATLLVM.GLValue;     use GNATLLVM.GLValue;
with GNATLLVM.Utils;       use GNATLLVM.Utils;

package GNATLLVM.Subprograms is

   --  These indicate whether a type must be passed by reference or what the
   --  default pass-by-reference status is.

   type Param_By_Ref_Mech is (Must, Default_By_Ref, Default_By_Copy);

   function Get_Param_By_Ref_Mech (GT : GL_Type) return Param_By_Ref_Mech
     with Pre => Present (GT);

   function Get_Mechanism_Code (E : Entity_Id; Exprs : List_Id) return Uint
     with Pre => Ekind_In (E, E_Function, E_Procedure);
   --  This is inquiring about either the return of E (if No (Exprs)) or
   --  of the parameter number given by the first expression of Exprs.
   --  Return 2 is passed by reference, otherwise, return 1.

   function Create_Subprogram_Type (Def_Ident  : Entity_Id) return Type_T
     with Pre  => Present (Def_Ident),
          Post => Present (Create_Subprogram_Type'Result);
   --  Create subprogram type.  Def_Ident can either be a subprogram,
   --  in which case a subprogram type will be created from it or a
   --  subprogram type directly.

   function Create_Subprogram_Access_Type return Type_T
     with Post => Present (Create_Subprogram_Access_Type'Result);
   --  Return a structure type that embeds Subp_Type and a static link pointer

   function Add_Global_Function
     (S          : String;
      Subp_Type  : Type_T;
      GT         : GL_Type;
      Can_Throw  : Boolean := False;
      Can_Return : Boolean := True) return GL_Value
     with Pre => S'Length > 0 and then Present (Subp_Type)
                 and then Present (GT);
   --  Create a function with the give name and type, but handling the case
   --  where we're also compiling a function with that name.  By default,
   --  these functions can return, but will not throw an exception, but
   --  this can be changed.

   function Get_From_Activation_Record (E : Entity_Id) return GL_Value
     with Pre => not Is_Type (E);
   --  Checks whether E is present in the current activation record and
   --  returns an LValue pointing to the value of the object if so.

   function Get_Static_Link (Subp : Entity_Id) return GL_Value
     with Pre  => Ekind_In (Subp, E_Procedure, E_Function),
          Post => Present (Get_Static_Link'Result);
   --  Build and return the static link to pass to a call to Subp

   function Has_Activation_Record (Def_Ident : Entity_Id) return Boolean
     with Pre => Ekind (Def_Ident) in Subprogram_Kind | E_Subprogram_Type;
   --  Return True if Def_Ident is a nested subprogram or a subprogram type
   --  that needs an activation record.

   function Emit_Subprogram_Identifier
     (Def_Ident : Entity_Id; N : Node_Id; GT : GL_Type) return GL_Value
     with Pre  => not Is_Type (Def_Ident) and then Present (GT)
                  and then (N = Def_Ident or else Nkind (N) in N_Has_Entity),
          Post => Present (Emit_Subprogram_Identifier'Result);
   --  Emit the value (creating the subprogram if needed) of the N_Identifier
   --  or similar at N.  The entity if Def_Ident and its type is TE.

   function Emit_Call
     (N : Node_Id; LHS : GL_Value := No_GL_Value) return GL_Value
     with Pre  => Nkind (N) in N_Subprogram_Call;
   --  Compile a call statement/expression and return its result
   --  value.  If this is calling a procedure, there will be no return value.
   --  If LHS is Present, it's a place that we'll be storing the result of
   --  the function in case that turns out to be useful.

   function Call_Alloc
     (Proc : Entity_Id; Args : GL_Value_Array) return GL_Value
     with Pre => Ekind (Proc) = E_Procedure;
   --  Proc is a Procedure_To_Call for an allocation and Args are its
   --  arguments.  See if Proc needs a static link and pass one, if
   --  so.  This procedure has one out parameter, so the low-level
   --  call is as a function returning the memory that was allocated.

   procedure Call_Dealloc (Proc : Entity_Id; Args : GL_Value_Array)
     with Pre => Ekind (Proc) = E_Procedure;
   --  Proc is a Procedure_To_Call for a deallocation and Args are its
   --  arguments.  See if Proc needs a static link and pass one, if so.

   procedure Add_To_Elab_Proc (N : Node_Id; For_GT : GL_Type := No_GL_Type)
     with Pre => Library_Level and then Present (N);
   --  Add N to the elaboration table if it's not already there.  We assume
   --  here that if it's already there, it was the last one added.  If
   --  For_GT is Present, elaborate N as an expression, convert to
   --  For_GT, and save it as the value for N.

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

   procedure Emit_One_Body (N : Node_Id; For_Inline : Boolean := False)
     with Pre => Present (N);
   --  Generate code for one given subprogram body.  If For_Inline is
   --  True, we're compiling this just to possibly inline it.

   function Create_Subprogram (Def_Ident : Entity_Id) return GL_Value
     with Pre => Ekind (Def_Ident) in Subprogram_Kind;
   --  Create and save an LLVM object for Def_Ident, a subprogram

   function Emit_Subprogram_Decl (N : Node_Id;
      Frozen : Boolean := True) return GL_Value
     with Pre => Present (N);
   --  Compile a subprogram declaration, creating the subprogram if not
   --  already done.  Return the subprogram value.

   procedure Emit_Subprogram_Body (N : Node_Id; For_Inline : Boolean := False)
     with Pre => Present (N);
   --  Compile a subprogram body and save it in the environment.  If For_Inline
   --  is True, we're compiling this just to possibly inline it.

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

   procedure Output_Global_Constructors_Destructors;
   --  Called at end of compilation to output variables for either of the above

   procedure Initialize;
   --  Initialize module

   Current_Subp             : Entity_Id  := Empty;
   --  The spec entity for the subprogram currently being compiled

   Current_Func             : GL_Value   := No_GL_Value;
   --  Pointer to the current function

   Activation_Rec_Param     : GL_Value   := No_GL_Value;
   --  Parameter to this subprogram, if any, that represents an
   --  activation record, expressed as a reference to the record.

   Return_Address_Param     : GL_Value   := No_GL_Value;
   --  Parameter to this subprogram, if any, that represent the address
   --  to which we are to copy the return value

   In_Elab_Proc             : Boolean    := False;
   --  True if we're in the process of emitting the code for an elaboration
   --  procedure and processing the code for deferred declaration statements.

   In_Elab_Proc_Stmts       : Boolean    := False;
   --  Likewise, but we're in the part of the elab proc that handles
   --  statements explicitly in the body of the package.

   Entry_Block_Allocas      : Position_T := No_Position_T;
   --  If Present, a location to use to insert small alloca's into the entry
   --  block.

end GNATLLVM.Subprograms;
