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

with GNATLLVM.GLValue;      use GNATLLVM.GLValue;
with GNATLLVM.Instructions; use GNATLLVM.Instructions;

package GNATLLVM.Subprograms is

   --  These indicate whether a type must be passed by reference or what the
   --  default pass-by-reference status is.

   type Param_By_Ref_Mech is (Must, Default_By_Ref, Default_By_Copy);

   function Get_Param_By_Ref_Mech (GT : GL_Type) return Param_By_Ref_Mech
     with Pre => Present (GT);

   function Get_Mechanism_Code
     (E : Subprogram_Kind_Id; Exprs : List_Id) return Uint;
   --  This is inquiring about either the return of E (if No (Exprs)) or
   --  of the parameter number given by the first expression of Exprs.
   --  Return 2 is passed by reference, otherwise, return 1.

   function Create_Subprogram_Type
     (E : Subprogram_Type_Or_Kind_Id) return Type_T
     with Post => Present (Create_Subprogram_Type'Result);
   --  Create subprogram type. E can either be a subprogram, in which case
   --  a subprogram type will be created from it or a subprogram type
   --  directly.

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
   --  where we're also compiling a function with that name. By default,
   --  these functions can return, but will not throw an exception, but
   --  this can be changed.

   function Get_From_Activation_Record (E : Evaluable_Kind_Id) return GL_Value;
   --  Checks whether E is present in the current activation record and
   --  returns an LValue pointing to the value of the object if so.

   function Get_Static_Link (Subp : Subprogram_Kind_Id) return GL_Value
     with Post => Present (Get_Static_Link'Result);
   --  Build and return the static link to pass to a call to Subp

   function Has_Activation_Record
     (E : Subprogram_Type_Or_Kind_Id) return Boolean;
   --  Return True if E is a nested subprogram or a subprogram type
   --  that needs an activation record.

   function Emit_Subprogram_Identifier
     (E  : Subprogram_Kind_Id;
      N  : Opt_N_Has_Entity_Id;
      GT : GL_Type) return GL_Value
     with Pre  => Present (GT),
          Post => Present (Emit_Subprogram_Identifier'Result);
   --  Emit the value (creating the subprogram if needed) of E whose
   --  type is TE. N, if Present, is the N_Identifier showing how this
   --  was used.

   function Emit_Call
     (N         : N_Subprogram_Call_Id;
      Outer_LHS : GL_Value := No_GL_Value) return GL_Value;
   --  Compile a call statement/expression and return its result value. If
   --  this is calling a procedure, there will be no return value. If
   --  Outer_LHS is Present, it's a place that we'll be storing the result
   --  of the function in case that turns out to be useful.

   function Call_Alloc
     (Proc : E_Procedure_Id;
      N    : Node_Id;
      Args : GL_Value_Array) return GL_Value
     with Post => Present (Call_Alloc'Result);
   --  Proc is a Procedure_To_Call for an allocation, Args are its
   --  arguments, and N is the node causing the allocation. See if
   --  Proc needs a static link and pass one, if so. This procedure
   --  has one out parameter, so the low-level call is as a function
   --  returning the memory that was allocated.

   procedure Call_Dealloc (Proc : E_Procedure_Id; Args : GL_Value_Array);
   --  Proc is a Procedure_To_Call for a deallocation and Args are its
   --  arguments. See if Proc needs a static link and pass one, if so.

   procedure Add_To_Elab_Proc (N : Node_Id; For_GT : GL_Type := No_GL_Type)
     with Pre => Library_Level;
   --  Add N to the elaboration table if it's not already there. We assume
   --  here that if it's already there, it was the last one added. If
   --  For_GT is Present, elaborate N as an expression, convert to
   --  For_GT, and save it as the value for N. If N isn't Present, it
   --  denotes an empty entry.

   procedure Mark_Body_Elab;
   --  Indicate that anything added to an elab proc is now being added to
   --  the elab proc for the body.

   procedure Emit_Elab_Proc
     (N        : Node_Id;
      Stmts    : Opt_N_Handled_Sequence_Of_Statements_Id;
      CU       : N_Compilation_Unit_Id;
      For_Body : Boolean := False)
     with Pre => Library_Level
                 and then Nkind (N) in N_Package_Specification | N_Package_Body
                 and then Nkind (CU) = N_Compilation_Unit;
   --  Emit code for the elaboration procedure for N. For_Body says
   --  whether this is the elab proc for the body or the spec of a package.
   --  CU is the corresponding N_Compilation_Unit on which we set
   --  Has_No_Elaboration_Code if there is any. Stmts, if Present, is an
   --  N_Handled_Sequence_Of_Statements that also have to be in the
   --  elaboration procedure.

   function Get_Elab_Position return Nat;
   --  Get an index into the elab table that we can use for the following
   --  procedure to re-order the elab table.

   procedure Reorder_Elab_Table (Old_Pos, New_Start : Nat);
   --  Reorder the elab table so it contains, in order, entries before
   --  Old_Pos, entries from New_Start to the end, and entries after Old_Pos.

   procedure Emit_One_Body
     (N : N_Subprogram_Body_Id; For_Inline : Boolean := False);
   --  Generate code for one given subprogram body. If For_Inline is
   --  True, we're compiling this just to possibly inline it.

   --  For subprogram return, we have the mechanism for handling the
   --  subprogram return value, if any, and what the actual LLVM function
   --  returns, which is a combination of any return value and any scalar
   --  out parameters.

   type Return_Kind is
     (None,
      --  This is a procedure; there is no return value

      RK_By_Reference,
      --  The result is returned by reference

      Value_Return,
      --  The result is returned by value

      Return_By_Parameter);
      --  The result is returned via a pointer passed as an extra parameter

   function Get_Return_Kind
     (E : Subprogram_Type_Or_Kind_Id) return Return_Kind;
   --  Get the Return_Kind of E, a subprogram or subprogram type

   --  Next we have the actual LLVM return contents, which can be the
   --  subprogram return, one or more out parameters, or both. This
   --  says which.

   type L_Ret_Kind is
     (Void,
      --  The LLVM function has no return, meaning this is a procedure
      --  with no out parameters.

      Subprog_Return,
      --  The LLVM function returns what the Ada function returns.
      --  Return_Kind says if its by value or by reference

      Out_Return,
      --  The LLVM function returns the contents of the single out
      --  parameter of a procedure, which must be by value

      Struct_Out,
      --  The LLVM function returns the contents of more than one
      --  out parameter of a procedure, put together into a struct.

      Struct_Out_Subprog);
      --  The LLVM function retuns a struct which contains both the actual
      --  return data of the function (either by value or reference) and
      --  one or more out parameters.

   function Get_L_Ret_Kind (E : Subprogram_Type_Or_Kind_Id) return L_Ret_Kind;

   function Number_In_Params (E : Subprogram_Type_Or_Kind_Id) return Nat;
   --  Return a count of the number of parameters of E, that are
   --  explict input parameters to E. We may have to add a parameter for
   --  an activation record and/or address to place the return.

   function Number_Out_Params (E : Subprogram_Type_Or_Kind_Id) return Nat;
   --  Return a count of the number of parameters of E, that are
   --  output parameters to E.

   function First_In_Param
     (E : Subprogram_Type_Or_Kind_Id) return Opt_Formal_Kind_Id;
   --  Return the first formal of E that's an input to the subprogram,
   --  either because it's an input passed by copy or a reference.

   function Next_In_Param (E : Formal_Kind_Id) return Opt_Formal_Kind_Id;
   --  Given E, a formal of some subprogram, return the next In parameter,
   --  as defined above, of that subprogram.

   procedure Next_In_Param (E : in out Opt_Formal_Kind_Id)
     with Pre => Present (E);
   --  Given E, a formal of some subprogram, update it to be the next In
   --  parameter, as defined above, of that subprogram.

   function First_Out_Param
     (E : Subprogram_Type_Or_Kind_Id) return Opt_Formal_Kind_Id;
   --  Return the first formal of E that's an output from the subprogram

   function Next_Out_Param (E : Formal_Kind_Id) return Opt_Formal_Kind_Id;
   --  Given E, a formal of some subprogram, return the next Out parameter,
   --  as defined above, of that subprogram.

   procedure Next_Out_Param (E : in out Opt_Formal_Kind_Id)
     with Pre  => Present (E);
   --  Given E, a formal of some subprogram, update it to be the next Out
   --  parameter, as defined above, of that subprogram.

   function Param_Is_Reference (E : Formal_Kind_Id) return Boolean;
   --  Return True iff E, a subprogram parameter, is passed by reference

   function Create_Subprogram (E : Subprogram_Kind_Id) return GL_Value;
   --  Create and save an LLVM object for E, a subprogram

   function Emit_Subprogram_Decl
     (N      : N_Subprogram_Specification_Id;
      Frozen : Boolean := True) return GL_Value;
   --  Compile a subprogram specification, creating the subprogram if not
   --  already done. Return the subprogram value.

   procedure Emit_Subprogram_Body
     (N : N_Subprogram_Body_Id; For_Inline : Boolean := False);
   --  Compile a subprogram body and save it in the environment. If
   --  For_Inline is True, we're compiling this just to possibly inline it.

   procedure Emit_Return_Statement (N : N_Simple_Return_Statement_Id)
     with Pre => Nkind (N) = N_Simple_Return_Statement;
   --  Emit code for a return statement

   function Actual_Subprogram_Base_Type
     (E : Entity_Id) return Opt_Type_Kind_Id;
   --  If E is Present and a subprogram, return the base type of the GNAT
   --  type corresponding to the LLVM type of its return value, if any.

   function Add_Static_Link
     (Proc : Opt_Subprogram_Kind_Id;
      Args : GL_Value_Array) return GL_Value_Array;
   --  If Proc needs a static link, add it to the end of Args

   function Subp_Ptr (N : N_Subexpr_Id) return GL_Value
     with Post => Present (Subp_Ptr'Result), Inline;
   --  Return the subprogram pointer associated with N

   procedure Enter_Subp (Func : GL_Value)
     with Pre  => Present (Func) and then Library_Level,
          Post => not Library_Level, Inline;
   --  Create an entry basic block for this subprogram and position the
   --  builder at its end. Mark that we're in a subprogram. To be used when
   --  starting the compilation of a subprogram body.

   procedure Leave_Subp
     with Pre  => not Library_Level, Post => Library_Level, Inline;
   --  Indicate that we're no longer compiling a subprogram

   function Library_Level return Boolean with Inline;
   --  Return True if we're at library level

   function Create_Basic_Block (Name : String := "") return Basic_Block_T
     with Post => Present (Create_Basic_Block'Result);
   --  Create a basic block in the current function

   procedure Output_Global_Constructors_Destructors;
   --  Called at end of compilation to output variables for either of the above

   procedure Add_Functions_To_Module;
   --  Called at end of compilation to add functions to the module for which
   --  we haven't emitted a body.

   Current_Subp             : Opt_Subprogram_Kind_Id  := Empty;
   --  The spec entity for the subprogram currently being compiled

   Current_Func             : GL_Value                := No_GL_Value;
   --  Pointer to the current function

   Activation_Rec_Param     : GL_Value                := No_GL_Value;
   --  Parameter to this subprogram, if any, that represents an
   --  activation record, expressed as a reference to the record.

   Return_Address_Param     : GL_Value                := No_GL_Value;
   --  Parameter to this subprogram, if any, that represent the address
   --  to which we are to copy the return value

   In_Elab_Proc             : Boolean                 := False;
   --  True if we're in the process of emitting the code for an elaboration
   --  procedure and processing the code for deferred declaration statements.

   In_Elab_Proc_Stmts       : Boolean                 := False;
   --  Likewise, but we're in the part of the elab proc that handles
   --  statements explicitly in the body of the package.

   Entry_Block_Allocas      : Position_T              := No_Position_T;
   --  If Present, a location to use to insert small alloca's into the entry
   --  block.

end GNATLLVM.Subprograms;
