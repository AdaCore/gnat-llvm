------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2017, AdaCore                     --
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

with Ada.Containers;          use Ada.Containers;
with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;
with System;

with Atree; use Atree;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Eval_Fat; use Eval_Fat;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Sem_Util; use Sem_Util;
with Sinfo; use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

with LLVM.Analysis; use LLVM.Analysis;
with LLVM.Core; use LLVM.Core;
with LLVM.Target; use LLVM.Target;

with GNATLLVM.Arrays;       use GNATLLVM.Arrays;
with GNATLLVM.Bounds;       use GNATLLVM.Bounds;
with GNATLLVM.Builder;      use GNATLLVM.Builder;
with GNATLLVM.Nested_Subps; use GNATLLVM.Nested_Subps;
with GNATLLVM.Types;        use GNATLLVM.Types;
with GNATLLVM.Utils;        use GNATLLVM.Utils;

package body GNATLLVM.Compile is

   --  Note: in order to find the right LLVM instruction to generate,
   --  you can compare with what Clang generates on corresponding C or C++
   --  code. This can be done online via http://ellcc.org/demo/index.cgi

   function Get_Type_Size
     (Env : Environ;
      T   : Type_T) return Value_T;
   --  Return the size of an LLVM type, in bytes
   function Record_Field_Offset
     (Env : Environ;
      Record_Ptr : Value_T;
      Record_Field : Node_Id) return Value_T;

   function Build_Type_Conversion
     (Env                 : Environ;
      Src_Type, Dest_Type : Entity_Id;
      Value               : Value_T) return Value_T;
   --  Emit code to convert Src_Value to Dest_Type

   type Short_Circuit_Operator is (Op_Or, Op_And);

   function Build_Short_Circuit_Op
     (Env : Environ;
      Left, Right : Value_T;
      Op  : Short_Circuit_Operator) return Value_T;
   --  Emit the LLVM IR for a short circuit operator ("or else", "and then")

   function Emit_Attribute_Reference
     (Env  : Environ;
      Node : Node_Id) return Value_T
     with Pre => Nkind (Node) = N_Attribute_Reference;
   --  Helper for Emit_Expression: handle N_Attribute_Reference nodes

   function Emit_Call
     (Env : Environ; Call_Node : Node_Id) return Value_T;
   --  Helper for Emit/Emit_Expression: compile a call statement/expression and
   --  return its result value.

   function Emit_Comparison
     (Env          : Environ;
      Operation    : Pred_Mapping;
      Operand_Type : Entity_Id;
      LHS, RHS     : Node_Id) return Value_T;
   --  Helper for Emit_Expression: handle comparison operations

   function Emit_If
     (Env  : Environ;
      Node : Node_Id) return Value_T
     with Pre => Nkind (Node) in N_If_Statement | N_If_Expression;
   --  Helper for Emit and Emit_Expression: handle if statements and if
   --  expressions.

   procedure Emit_List
     (Env : Environ; List : List_Id);
   --  Helper for Emit/Emit_Expression: call Emit on every element of List

   function Emit_Min_Max
     (Env         : Environ;
      Exprs       : List_Id;
      Compute_Max : Boolean) return Value_T
     with Pre => List_Length (Exprs) = 2
     and then Is_Scalar_Type (Etype (First (Exprs)));
   --  Exprs must be a list of two scalar expressions with compatible types.
   --  Emit code to evaluate both expressions. If Compute_Max, return the
   --  maximum value and return the minimum otherwise.

   function Emit_Shift
     (Env       : Environ;
      Node      : Node_Id;
      LHS, RHS  : Value_T) return Value_T;
   --  Helper for Emit_Expression: handle shift and rotate operations

   function Emit_Subprogram_Decl
     (Env : Environ; Subp_Spec : Node_Id) return Value_T;
   --  Compile a subprogram declaration, save the corresponding LLVM value to
   --  the environment and return it.

   function Emit_Type_Size
     (Env                   : Environ;
      T                     : Entity_Id;
      Array_Descr           : Value_T;
      Containing_Record_Ptr : Value_T) return Value_T;
   --  Helper for Emit/Emit_Expression: emit code to compute the size of type
   --  T, getting information from Containing_Record_Ptr for types that are
   --  constrained by a discriminant record (in such case, this parameter
   --  should be a pointer to the corresponding record). If T is an
   --  unconstrained array, Array_Descr must be the corresponding fat
   --  pointer. Return the computed size as value.

   function Create_Callback_Wrapper
     (Env : Environ; Subp : Entity_Id) return Value_T;
   --  If Subp takes a static link, return its LLVM declaration. Otherwise,
   --  create a wrapper declaration to it that accepts a static link and
   --  return it.

   procedure Attach_Callback_Wrapper_Body
     (Env : Environ; Subp : Entity_Id; Wrapper : Value_T);
   --  If Subp takes a static link, do nothing. Otherwise, add the
   --  implementation of its wrapper.

   procedure Match_Static_Link_Variable
     (Env       : Environ;
      Def_Ident : Entity_Id;
      LValue    : Value_T);
   --  If Def_Ident belongs to the closure of the current static link
   --  descriptor, reference it to the static link structure. Do nothing
   --  if there is no current subprogram.

   function Get_Static_Link
     (Env  : Environ;
      Subp : Entity_Id) return Value_T;
   --  Build and return the appropriate static link to pass to a call to Subp

   -------------------
   -- Get_Type_Size --
   -------------------

   function Get_Type_Size
     (Env : Environ;
      T   : Type_T) return Value_T
   is
      T_Data : constant Target_Data_T :=
        Create_Target_Data (Get_Target (Env.Mdl));
   begin
      return Const_Int
        (Int_Ptr_Type,
         Size_Of_Type_In_Bits (T_Data, T) / 8,
         Sign_Extend => LLVM.Types.False);
   end Get_Type_Size;

   --------------------
   -- Emit_Type_Size --
   --------------------

   function Emit_Type_Size
     (Env                   : Environ;
      T                     : Entity_Id;
      Array_Descr           : Value_T;
      Containing_Record_Ptr : Value_T) return Value_T
   is
      LLVM_Type : constant Type_T := Create_Type (Env, T);
   begin
      if Is_Scalar_Type (T)
        or else Is_Access_Type (T)
      then
         return Get_Type_Size (Env, LLVM_Type);
      elsif Is_Array_Type (T) then
         return Mul
           (Env.Bld,
            Emit_Type_Size
              (Env, Component_Type (T), No_Value_T, Containing_Record_Ptr),
            Array_Size
              (Env, Array_Descr, T, Containing_Record_Ptr),
            "array-size");

      else
         Error_Msg_N ("unimplemented case for emit type size", T);
         raise Program_Error;
      end if;
   end Emit_Type_Size;

   -------------------------
   -- Record_Field_Offset --
   -------------------------

   function Record_Field_Offset
     (Env : Environ;
      Record_Ptr : Value_T;
      Record_Field : Node_Id) return Value_T
   is
      Field_Id   : constant Entity_Id := Defining_Identifier (Record_Field);
      Type_Id    : constant Entity_Id := Scope (Field_Id);
      R_Info     : constant Record_Info := Env.Get (Type_Id);
      F_Info     : constant Field_Info := R_Info.Fields.Element (Field_Id);
      Struct_Ptr : Value_T := Record_Ptr;

   begin
      if F_Info.Containing_Struct_Index > 1 then
         declare
            Int_Struct_Address : Value_T := Ptr_To_Int
              (Env.Bld,
               Record_Ptr, Int_Ptr_Type, "offset-calc");
            S_Info : constant Struct_Info :=
              R_Info.Structs (F_Info.Containing_Struct_Index);

         begin
            --  Accumulate the size of every field
            for Preceding_Field of S_Info.Preceding_Fields loop
               Int_Struct_Address := Add
                 (Env.Bld,
                  Int_Struct_Address,
                  Emit_Type_Size
                    (Env,
                     Etype (Preceding_Field.Entity),
                     No_Value_T,
                     Record_Ptr),
                  "offset-calc");
            end loop;

            Struct_Ptr := Int_To_Ptr
              (Env.Bld,
               Int_Struct_Address, Pointer_Type (S_Info.LLVM_Type, 0), "back");
         end;
      end if;

      return Struct_GEP
        (Env.Bld,
         Struct_Ptr, unsigned (F_Info.Index_In_Struct), "field_access");
   end Record_Field_Offset;

   ----------
   -- Emit --
   ----------

   procedure Emit (Env : Environ; Node : Node_Id) is
   begin
      case Nkind (Node) is
         when N_Compilation_Unit =>
            Emit_List (Env, Context_Items (Node));
            Emit_List (Env, Declarations (Aux_Decls_Node (Node)));
            Emit (Env, Unit (Node));
            Emit_List (Env, Actions (Aux_Decls_Node (Node)));
            Emit_List (Env, Pragmas_After (Aux_Decls_Node (Node)));

         when N_With_Clause =>
            null;

         when N_Use_Package_Clause =>
            null;

         when N_Package_Declaration =>
            Emit (Env, Specification (Node));

         when N_Package_Specification =>
            Emit_List (Env, Visible_Declarations (Node));
            Emit_List (Env, Private_Declarations (Node));

         when N_Package_Body =>
            declare
               Def_Id : constant Entity_Id := Unique_Defining_Entity (Node);
            begin
               if Ekind (Def_Id) not in Generic_Unit_Kind then
                  Emit_List (Env, Declarations (Node));
                  --  ??? Handle statements
               end if;
            end;

         when N_String_Literal =>
            Discard (Emit_Expression (Env, Node));

         when N_Subprogram_Body =>
            --  If we are processing only declarations, do not emit a
            --  subprogram body: just declare this subprogram and add it to
            --  the environment.

            if Env.In_Declarations then
               Discard (Emit_Subprogram_Decl (Env, Get_Acting_Spec (Node)));
               return;

            --  There is nothing to emit for the template of a generic
            --  subprogram body: ignore them.

            elsif Ekind (Defining_Unit_Name (Get_Acting_Spec (Node)))
              in Generic_Subprogram_Kind
            then
               return;
            end if;

            declare
               Spec       : constant Node_Id := Get_Acting_Spec (Node);
               Def_Ident  : constant Entity_Id := Defining_Unit_Name (Spec);
               Func       : constant Value_T :=
                 Emit_Subprogram_Decl (Env, Spec);
               Subp       : constant Subp_Env := Env.Enter_Subp (Node, Func);
               Wrapper    : Value_T;

               LLVM_Param : Value_T;
               LLVM_Var   : Value_T;
               Param      : Entity_Id;
               I          : Natural := 0;

            begin
               --  Create a value for the static-link structure

               Subp.S_Link := Alloca
                 (Env.Bld,
                  Create_Static_Link_Type (Env, Subp.S_Link_Descr),
                  "static-link");

               --  Create a wrapper for this function, if needed, and add its
               --  implementation, still if needed.

               Wrapper := Create_Callback_Wrapper (Env, Def_Ident);
               Attach_Callback_Wrapper_Body (Env, Def_Ident, Wrapper);

               --  Register each parameter into a new scope
               Env.Push_Scope;

               for P of Iterate (Parameter_Specifications (Spec)) loop
                  LLVM_Param := Get_Param (Subp.Func, unsigned (I));
                  Param := Defining_Identifier (P);

                  --  Define a name for the parameter P (which is the I'th
                  --  parameter), and associate the corresponding LLVM value to
                  --  its entity.

                  --  Set the name of the llvm value

                  Set_Value_Name (LLVM_Param, Get_Name (Param));

                  --  Special case for structures passed by value, we want to
                  --  store a pointer to them on the stack, so do an alloca,
                  --  to be able to do GEP on them.

                  if Param_Needs_Ptr (Param)
                    and then not
                      (Ekind (Etype (Param)) in Record_Kind
                       and (Get_Type_Kind (Type_Of (LLVM_Param))
                            = Struct_Type_Kind))
                  then
                     LLVM_Var := LLVM_Param;
                  else
                     LLVM_Var := Alloca
                       (Env.Bld,
                        Type_Of (LLVM_Param), Get_Name (Param));
                     Store (Env.Bld, LLVM_Param, LLVM_Var);
                  end if;

                  --  Add the parameter to the environnment

                  Env.Set (Param, LLVM_Var);

                  Match_Static_Link_Variable
                    (Env, Param, LLVM_Var);

                  I := I + 1;
               end loop;

               if Env.Takes_S_Link (Def_Ident) then

                  --  Rename the static link argument and link the static link
                  --  value to it.

                  declare
                     Parent_S_Link : constant Value_T :=
                       Get_Param (Subp.Func, unsigned (I));
                     Parent_S_Link_Type : constant Type_T :=
                       Pointer_Type
                         (Create_Static_Link_Type
                            (Env, Subp.S_Link_Descr.Parent),
                          0);
                     S_Link        : Value_T;

                  begin
                     Set_Value_Name (Parent_S_Link, "parent-static-link");
                     S_Link := Load (Env.Bld, Subp.S_Link, "static-link");
                     S_Link := Insert_Value
                       (Env.Bld,
                        S_Link,
                        Bit_Cast
                          (Env.Bld,
                           Parent_S_Link, Parent_S_Link_Type, ""),
                        0,
                        "updated-static-link");
                     Store (Env.Bld, S_Link, Subp.S_Link);
                  end;

                  --  Then "import" from the static link all the non-local
                  --  variables.

                  for Cur in Subp.S_Link_Descr.Accesses.Iterate loop
                     declare
                        use Local_Access_Maps;

                        Access_Info : Access_Record renames Element (Cur);
                        Depth       : Natural := Access_Info.Depth;
                        LValue      : Value_T := Subp.S_Link;

                        Idx_Type    : constant Type_T :=
                          Int32_Type_In_Context (Env.Ctx);
                        Zero        : constant Value_T :=
                          Const_Null (Idx_Type);
                        Idx         : Value_Array (1 .. 2) :=
                          (Zero, Zero);

                     begin
                        --  Get a pointer to the target parent static link
                        --  structure.

                        while Depth > 0 loop
                           LValue := Load
                             (Env.Bld,
                              GEP
                                (Env.Bld,
                                 LValue,
                                 Idx'Address, Idx'Length,
                                 ""),
                              "");
                           Depth := Depth - 1;
                        end loop;

                        --  And then get the non-local variable as an lvalue

                        Idx (2) := Const_Int
                          (Idx_Type,
                           unsigned_long_long (Access_Info.Field),
                           Sign_Extend => LLVM.Types.False);
                        LValue := Load
                          (Env.Bld,
                           GEP
                             (Env.Bld,
                              LValue, Idx'Address, Idx'Length, ""),
                           "");

                        Set_Value_Name (LValue, Get_Name (Key (Cur)));
                        Env.Set (Key (Cur), LValue);
                     end;
                  end loop;

               end if;

               Emit_List (Env, Declarations (Node));
               Emit_List
                 (Env, Statements (Handled_Statement_Sequence (Node)));

               --  This point should not be reached: a return must have
               --  already... returned!

               Discard (Build_Unreachable (Env.Bld));

               Env.Pop_Scope;
               Env.Leave_Subp;

               pragma Annotate (Xcov, Exempt_On, "Defensive programming");
               if Verify_Function
                    (Subp.Func, Print_Message_Action) = LLVM.Types.True
               then
                  Error_Msg_N
                    ("The backend generated bad LLVM for this subprogram.",
                     Node);
                  Dump_LLVM_Module (Env.Mdl);
               end if;
               pragma Annotate (Xcov, Exempt_Off);
            end;

         when N_Subprogram_Declaration =>
            Discard (Emit_Subprogram_Decl (Env, Specification (Node)));

         when N_Raise_Constraint_Error | N_Raise_Program_Error =>

            --  ??? When exceptions handling will be implemented, implement
            --  this.

            null;

         when N_Raise_Storage_Error =>

            --  ??? When exceptions handling will be implemented, implement
            --  this.

            null;

         when N_Object_Declaration | N_Exception_Declaration =>
            --  Object declarations are variables either allocated on the stack
            --  (local) or global.

            --  If we are processing only declarations, only declare the
            --  corresponding symbol at the LLVM level and add it to the
            --  environment.

            declare
               Def_Ident      : constant Node_Id := Defining_Identifier (Node);
               T              : constant Entity_Id :=
                 Get_Full_View (Etype (Def_Ident));
               LLVM_Type      : Type_T;
               LLVM_Var, Expr : Value_T;

            begin
               --  Nothing to do if this is a debug renaming type or if there
               --  is an address clause on the object (will be handled as part
               --  of N_Attribute_Definition_Clause)

               if T = Standard_Debug_Renaming_Type
               --  ???  or else Present (Address_Clause (Def_Ident))
               then
                  return;
               end if;

               --  Handle top-level declarations

               if Env.Library_Level then
                  --  ??? Will only work for objects of static sizes
                  LLVM_Type := Create_Type (Env, T);
                  LLVM_Var :=
                    Add_Global (Env.Mdl, LLVM_Type, Get_Name (Def_Ident));
                  Env.Set (Def_Ident, LLVM_Var);

                  if Env.In_Main_Unit then
                     --  if Is_Statically_Allocated (Def_Ident) then
                     --     Set_Linkage (LLVM_Var, Internal_Linkage);
                     --  end if;

                     --  Do we need to take Is_True_Constant (Def_Ident) into
                     --  account???

                     --  Take Expression (Node) into account
                     --  ??? Will only work if Expression is static

                     if Present (Expression (Node))
                       and then not
                         (Nkind (Node) = N_Object_Declaration
                          and then No_Initialization (Node))
                     then
                        if False then --  Static_Expression (Expression (Node))
                           Expr := Emit_Expression (Env, Expression (Node));
                           Set_Initializer (LLVM_Var, Expr);
                        else
                           --  ??? Should append to the elab procedure
                           --  Expr :=
                           --    Emit_Expression (Env, Expression (Node));
                           --  Store (Env.Bld, Expr, LLVM_Var);
                           null;
                        end if;
                     end if;
                  else
                     Set_Linkage (LLVM_Var, External_Linkage);
                  end if;

               else
                  if Is_Array_Type (T) then

                     --  Alloca arrays are handled as follows:
                     --  * The total size is computed with Compile_Array_Size.
                     --  * The type of the innermost component is computed with
                     --    Get_Innermost_Component type.
                     --  * The result of the alloca is bitcasted to the proper
                     --    array type, so that multidimensional LLVM GEP
                     --    operations work properly.

                     LLVM_Type := Create_Access_Type (Env, T);
                     LLVM_Var := Bit_Cast
                        (Env.Bld,
                         Array_Alloca
                           (Env.Bld,
                            Get_Innermost_Component_Type (Env, T),
                            Array_Size (Env, No_Value_T, T),
                            "array-alloca"),
                        LLVM_Type,
                        Get_Name (Def_Ident));

                  else
                     LLVM_Type := Create_Type (Env, T);
                     LLVM_Var := Alloca
                       (Env.Bld, LLVM_Type,
                        "local-" & Get_Name (Def_Ident));
                  end if;

                  Env.Set (Def_Ident, LLVM_Var);
                  Match_Static_Link_Variable (Env, Def_Ident, LLVM_Var);

                  if Present (Expression (Node))
                    and then not
                      (Nkind (Node) = N_Object_Declaration
                       and then No_Initialization (Node))
                  then
                     Expr := Emit_Expression (Env, Expression (Node));
                     Store (Env.Bld, Expr, LLVM_Var);

                     --  Would be nice to be able to use Set_Initializer, but
                     --  this seems to generate cast errors in the llvm code
                     --  generator right now???
                     --  Set_Initializer (LLVM_Var, Expr);
                  end if;
               end if;
            end;

         when N_Use_Type_Clause =>
            null;

         when N_Object_Renaming_Declaration =>
            declare
               Def_Ident : constant Node_Id := Defining_Identifier (Node);
               LLVM_Var  : Value_T;
            begin
               if Env.Library_Level then
                  --  ??? Handle top-level declarations

                  Error_Msg_N
                    ("library level object renaming not supported", Node);
                  return;
               end if;

               --  If the renamed object is already an l-value, keep it as-is.
               --  Otherwise, create one for it.

               if Is_LValue (Name (Node)) then
                  LLVM_Var := Emit_LValue (Env, Name (Node));
               else
                  LLVM_Var := Alloca
                    (Env.Bld,
                     Create_Type (Env, Etype (Def_Ident)),
                     Get_Name (Def_Ident));
                  Store
                    (Env.Bld, Emit_Expression (Env, Name (Node)), LLVM_Var);
               end if;

               Env.Set (Def_Ident, LLVM_Var);
               Match_Static_Link_Variable (Env, Def_Ident, LLVM_Var);
            end;

         when N_Subprogram_Renaming_Declaration =>
            declare
               Def_Ident : constant Entity_Id :=
                 Defining_Unit_Name (Specification (Node));
               Renamed_Subp : constant Value_T :=
                 Env.Get (Entity (Name (Node)));
            begin
               Env.Set (Def_Ident, Renamed_Subp);
            end;

         when N_Package_Renaming_Declaration =>
            --  At the moment, packages aren't materialized in LLVM IR, so
            --  there is nothing to do here.

            null;

         when N_Implicit_Label_Declaration =>
            Env.Set
              (Defining_Identifier (Node),
               Create_Basic_Block
                 (Env, Get_Name (Defining_Identifier (Node))));

         when N_Assignment_Statement =>
            declare
               Val : constant Value_T :=
                 Emit_Expression (Env, Expression (Node));
               Dest : constant Value_T := Emit_LValue (Env, Name (Node));

            begin
               Store (Env.Bld, Expr => Val, Ptr => Dest);
            end;

         when N_Procedure_Call_Statement =>
            Discard (Emit_Call (Env, Node));

         when N_Null_Statement =>
            null;

         when N_Label =>
            declare
               BB : constant Basic_Block_T :=
                 Env.Get (Entity (Identifier (Node)));
            begin
               Discard (Build_Br (Env.Bld, BB));
               Position_Builder_At_End (Env.Bld, BB);
            end;

         when N_Goto_Statement =>
            Discard (Build_Br (Env.Bld, Env.Get (Entity (Name (Node)))));
            Position_Builder_At_End
              (Env.Bld, Env.Create_Basic_Block ("after-goto"));

         when N_Exit_Statement =>
            declare
               Exit_Point : constant Basic_Block_T :=
                 (if Present (Name (Node))
                  then Env.Get_Exit_Point (Entity (Name (Node)))
                  else Env.Get_Exit_Point);
               Next_BB    : constant Basic_Block_T :=
                 Env.Create_Basic_Block ("loop-after-exit");

            begin
               if Present (Condition (Node)) then
                  Discard
                    (Build_Cond_Br
                       (Env.Bld,
                        Emit_Expression (Env, Condition (Node)),
                        Exit_Point,
                        Next_BB));

               else
                  Discard (Build_Br (Env.Bld, Exit_Point));
               end if;

               Position_Builder_At_End (Env.Bld, Next_BB);
            end;

         when N_Simple_Return_Statement =>
            if Present (Expression (Node)) then
               Discard
                 (Build_Ret
                   (Env.Bld, Emit_Expression (Env, Expression (Node))));
            else
               Discard (Build_Ret_Void (Env.Bld));
            end if;

            Position_Builder_At_End
              (Env.Bld, Env.Create_Basic_Block ("unreachable"));

         when N_If_Statement =>
            Discard (Emit_If (Env, Node));

         when N_Loop_Statement =>
            declare
               Loop_Identifier   : constant Entity_Id :=
                 (if Present (Identifier (Node))
                  then Entity (Identifier (Node))
                  else Empty);
               Iter_Scheme       : constant Node_Id :=
                 Iteration_Scheme (Node);
               Is_Mere_Loop      : constant Boolean :=
                 not Present (Iter_Scheme);
               Is_For_Loop       : constant Boolean :=
                 not Is_Mere_Loop
                 and then
                   Present (Loop_Parameter_Specification (Iter_Scheme));

               BB_Init, BB_Cond  : Basic_Block_T;
               BB_Stmts, BB_Iter : Basic_Block_T;
               BB_Next           : Basic_Block_T;
               Cond              : Value_T;
            begin
               --  The general format for a loop is:
               --    INIT;
               --    while COND loop
               --       STMTS;
               --       ITER;
               --    end loop;
               --    NEXT:
               --  Each step has its own basic block. When a loop does not need
               --  one of these steps, just alias it with another one.

               --  If this loop has an identifier, and it has already its own
               --  entry (INIT) basic block. Create one otherwise.
               BB_Init :=
                 (if Present (Identifier (Node))
                    and then Env.Has_BB (Entity (Identifier (Node)))
                  then Env.Get (Entity (Identifier (Node)))
                  else Create_Basic_Block (Env, ""));
               Discard (Build_Br (Env.Bld, BB_Init));
               Position_Builder_At_End (Env.Bld, BB_Init);

               --  If this is not a FOR loop, there is no initialization: alias
               --  it with the COND block.
               BB_Cond :=
                 (if not Is_For_Loop
                  then BB_Init
                  else Env.Create_Basic_Block ("loop-cond"));

               --  If this is a mere loop, there is even no condition block:
               --  alias it with the STMTS block.
               BB_Stmts :=
                 (if Is_Mere_Loop
                  then BB_Cond
                  else Env.Create_Basic_Block ("loop-stmts"));

               --  If this is not a FOR loop, there is no iteration: alias it
               --  with the COND block, so that at the end of every STMTS, jump
               --  on ITER or COND.
               BB_Iter :=
                 (if Is_For_Loop then Env.Create_Basic_Block ("loop-iter")
                  else BB_Cond);

               --  The NEXT step contains no statement that comes from the
               --  loop: it is the exit point.
               BB_Next := Create_Basic_Block (Env, "loop-exit");

               --  The front-end expansion can produce identifier-less loops,
               --  but exit statements can target them anyway, so register such
               --  loops.

               Env.Push_Loop (Loop_Identifier, BB_Next);
               Env.Push_Scope;

               --  First compile the iterative part of the loop: evaluation of
               --  the exit condition, etc.

               if not Is_Mere_Loop then
                  if not Is_For_Loop then

                     --  This is a WHILE loop: jump to the loop-body if the
                     --  condition evaluates to True, jump to the loop-exit
                     --  otherwise.

                     Position_Builder_At_End (Env.Bld, BB_Cond);
                     Cond := Emit_Expression (Env, Condition (Iter_Scheme));
                     Discard
                       (Build_Cond_Br (Env.Bld, Cond, BB_Stmts, BB_Next));

                  else
                     --  This is a FOR loop
                     declare
                        Loop_Param_Spec : constant Node_Id :=
                          Loop_Parameter_Specification (Iter_Scheme);
                        Def_Ident       : constant Node_Id :=
                          Defining_Identifier (Loop_Param_Spec);
                        Reversed        : constant Boolean :=
                          Reverse_Present (Loop_Param_Spec);
                        Unsigned_Type   : constant Boolean :=
                          Is_Unsigned_Type (Etype (Def_Ident));
                        LLVM_Type       : Type_T;
                        LLVM_Var        : Value_T;
                        Low, High       : Value_T;

                     begin
                        --  Initialization block: create the loop variable and
                        --  initialize it.
                        Create_Discrete_Type
                          (Env, Etype (Def_Ident), LLVM_Type, Low, High);
                        LLVM_Var := Alloca
                          (Env.Bld, LLVM_Type, Get_Name (Def_Ident));
                        Env.Set (Def_Ident, LLVM_Var);
                        Store
                          (Env.Bld,
                          (if Reversed then High else Low), LLVM_Var);

                        --  Then go to the condition block if the range isn't
                        --  empty.
                        Cond := I_Cmp
                          (Env.Bld,
                           (if Unsigned_Type then Int_ULE else Int_SLE),
                           Low, High,
                           "loop-entry-cond");
                        Discard
                          (Build_Cond_Br (Env.Bld, Cond, BB_Cond, BB_Next));

                        --  The FOR loop is special: the condition is evaluated
                        --  during the INIT step and right before the ITER
                        --  step, so there is nothing to check during the
                        --  COND step.
                        Position_Builder_At_End (Env.Bld, BB_Cond);
                        Discard (Build_Br (Env.Bld, BB_Stmts));

                        BB_Cond := Env.Create_Basic_Block ("loop-cond-iter");
                        Position_Builder_At_End (Env.Bld, BB_Cond);
                        Cond := I_Cmp
                          (Env.Bld,
                           Int_EQ,
                           Load (Env.Bld, LLVM_Var, "loop-var"),
                           (if Reversed then Low else High),
                            "loop-iter-cond");
                        Discard
                          (Build_Cond_Br (Env.Bld, Cond, BB_Next, BB_Iter));

                        --  After STMTS, stop if the loop variable was equal to
                        --  the "exit" bound. Increment/decrement it otherwise.
                        Position_Builder_At_End (Env.Bld, BB_Iter);

                        declare
                           Iter_Prev_Value : constant Value_T :=
                             Load (Env.Bld, LLVM_Var, "loop-var");
                           One             : constant Value_T :=
                             Const_Int (LLVM_Type, 1, LLVM.Types.False);
                           Iter_Next_Value : constant Value_T :=
                             (if Reversed
                              then Sub
                                (Env.Bld,
                                 Iter_Prev_Value, One, "next-loop-var")
                              else Add
                                (Env.Bld,
                                 Iter_Prev_Value, One, "next-loop-var"));
                        begin
                           Store (Env.Bld, Iter_Next_Value, LLVM_Var);
                        end;

                        Discard (Build_Br (Env.Bld, BB_Stmts));

                        --  The ITER step starts at this special COND step
                        BB_Iter := BB_Cond;
                     end;
                  end if;
               end if;

               Position_Builder_At_End (Env.Bld, BB_Stmts);
               Emit_List (Env, Statements (Node));
               Discard (Build_Br (Env.Bld, BB_Iter));

               Env.Pop_Scope;
               Env.Pop_Loop;

               Position_Builder_At_End (Env.Bld, BB_Next);
            end;

         when N_Block_Statement =>
            declare
               BE          : constant Entity_Id :=
                 (if Present (Identifier (Node))
                  then Entity (Identifier (Node))
                  else Empty);
               BB          : Basic_Block_T;
               Stack_State : Value_T;

            begin
               --  The frontend can generate basic blocks with identifiers
               --  that are not declared: try to get any existing basic block,
               --  create and register a new one if it does not exist yet.

               if Env.Has_BB (BE) then
                  BB := Env.Get (BE);
               else
                  BB := Create_Basic_Block (Env, "");

                  if Present (BE) then
                     Env.Set (BE, BB);
                  end if;
               end if;

               Discard (Build_Br (Env.Bld, BB));
               Position_Builder_At_End (Env.Bld, BB);

               Env.Push_Scope;
               Stack_State := Call
                 (Env.Bld,
                  Get_Stack_Save (Env), System.Null_Address, 0, "");

               Emit_List (Env, Declarations (Node));
               Emit_List
                 (Env, Statements (Handled_Statement_Sequence (Node)));

               Discard
                 (Call
                    (Env.Bld,
                     Get_Stack_Restore (Env), Stack_State'Address, 1, ""));

               Env.Pop_Scope;
            end;

         when N_Full_Type_Declaration | N_Subtype_Declaration
            | N_Incomplete_Type_Declaration | N_Private_Type_Declaration =>
            Env.Set (Defining_Identifier (Node),
                     Create_Type (Env, Defining_Identifier (Node)));

         when N_Freeze_Entity =>
            --  ??? Need to process Node itself

            Emit_List (Env, Actions (Node));

         when N_Pragma =>
            case Get_Pragma_Id (Node) is
               --  ??? While we aren't interested in most of the pragmas,
               --  there are some we should look at (see
               --  trans.c:Pragma_to_gnu). But still, the "others" case is
               --  necessary.
               when others => null;
            end case;

         --  Nodes we actually want to ignore
         when N_Call_Marker
            | N_Empty
            | N_Function_Instantiation
            | N_Package_Instantiation
            | N_Generic_Package_Declaration
            | N_Generic_Subprogram_Declaration
            | N_Itype_Reference
            | N_Number_Declaration
            | N_Procedure_Instantiation
            | N_Validate_Unchecked_Conversion
            | N_Variable_Reference_Marker =>
            null;

         --  ??? Ignore for now
         when N_Push_Constraint_Error_Label .. N_Pop_Storage_Error_Label =>
            null;

         --  ??? Ignore for now
         when N_Exception_Handler =>
            Error_Msg_N ("exception handler ignored??", Node);

         when N_Exception_Renaming_Declaration =>
            Error_Msg_N ("exception renaming ignored??", Node);

         when N_Attribute_Definition_Clause =>

            --  The only interesting case left after expansion is for Address
            --  clauses. We only deal with 'Address if the object has a Freeze
            --  node.

            if Get_Attribute_Id (Chars (Node)) = Attribute_Address
              and then Present (Freeze_Node (Entity (Name (Node))))
            then
               Error_Msg_N ("unsupported address clause", Node);
               --  ??? Save/Compile Expression (Node) for use in
            end if;

         when others =>
            Error_Msg_N
              ("unhandled statement kind: `" &
               Node_Kind'Image (Nkind (Node)) & "`", Node);
      end case;
   end Emit;

   -----------------
   -- Emit_LValue --
   -----------------

   function Emit_LValue (Env : Environ; Node : Node_Id) return Value_T is
   begin
      case Nkind (Node) is
         when N_Identifier | N_Expanded_Name =>
            declare
               Def_Ident : constant Entity_Id := Entity (Node);
            begin
               if Ekind (Def_Ident) = E_Function
                 or else Ekind (Def_Ident) = E_Procedure
               then
                  --  Return a callback, which is a couple: subprogram code
                  --  pointer, static link argument.

                  declare
                     Func   : constant Value_T :=
                       Create_Callback_Wrapper (Env, Def_Ident);
                     S_Link : constant Value_T :=
                       Get_Static_Link (Env, Def_Ident);

                     Fields_Types : constant array (1 .. 2) of Type_T :=
                       (Type_Of (Func),
                        Type_Of (S_Link));
                     Callback_Type : constant Type_T :=
                       Struct_Type_In_Context
                         (Env.Ctx,
                          Fields_Types'Address, Fields_Types'Length,
                          Packed => LLVM.Types.False);

                     Result : Value_T := Get_Undef (Callback_Type);

                  begin
                     Result := Insert_Value (Env.Bld, Result, Func, 0, "");
                     Result := Insert_Value
                       (Env.Bld,
                        Result, S_Link, 1, "callback");
                     return Result;
                  end;

               else
                  return Env.Get (Def_Ident);
               end if;
            end;

         when N_Explicit_Dereference =>
            return Emit_Expression (Env, Prefix (Node));

         when N_Aggregate | N_String_Literal =>
            declare
               --  The frontend can sometimes take a reference to an aggregate.
               --  In such cases, we have to create an anonymous object and use
               --  its value as the aggregate value.

               --  ??? This alloca will not necessarily be free'd before
               --  returning from the current subprogram: it's a leak.

               T : constant Type_T := Create_Type (Env, Etype (Node));
               V : constant Value_T := Alloca (Env.Bld, T, "anonymous-obj");

            begin
               Store (Env.Bld, Emit_Expression (Env, Node), V);
               return V;
            end;

         when N_Selected_Component =>
            declare
               Pfx_Ptr : constant Value_T :=
                 Emit_LValue (Env, Prefix (Node));
               Record_Component : constant Entity_Id :=
                 Parent (Entity (Selector_Name (Node)));
            begin
               return Record_Field_Offset (Env, Pfx_Ptr, Record_Component);
            end;

         when N_In | N_Not_In =>
            declare
               Is_In : constant Boolean := Nkind (Node) = N_In;
               Rng   : Node_Id := Right_Opnd (Node);
               First : Boolean := True;
               Comp1 : Value_T;
               Comp2 : Value_T;

            begin
               if Present (Rng) then
                  if Nkind (Rng) = N_Identifier then
                     Rng := Scalar_Range (Etype (Rng));
                  end if;

                  Comp1 := Emit_Comparison
                    (Env,
                     Get_Preds (if Is_In then N_Op_Ge else N_Op_Lt),
                     Get_Fullest_View (Etype (Left_Opnd (Node))),
                     Left_Opnd (Node), Low_Bound (Rng));
                  Comp2 := Emit_Comparison
                    (Env,
                     Get_Preds (if Is_In then N_Op_Le else N_Op_Gt),
                     Get_Fullest_View (Etype (Left_Opnd (Node))),
                     Left_Opnd (Node), High_Bound (Rng));
                  return Build_Short_Circuit_Op (Env, Comp1, Comp2, Op_And);

               else
                  for N of Iterate (Alternatives (Node)) loop
                     if First then
                        Comp1 := Emit_Expression (Env, N);
                        First := False;
                     else
                        Comp2 := Emit_Expression (Env, N);
                        Comp1 := Build_Or (Env.Bld, Comp1, Comp2, "or");
                     end if;
                  end loop;

                  if Is_In then
                     return Comp1;
                  else
                     return I_Cmp
                       (Env.Bld,
                        Int_NE,
                        Comp1,
                        Const_Int
                          (Create_Type (Env, Etype (Node)),
                           0, Sign_Extend => LLVM.Types.False),
                        "not");
                  end if;
               end if;
            end;

         when N_Indexed_Component =>
            declare
               Array_Node  : constant Node_Id := Prefix (Node);
               Array_Type  : constant Entity_Id := Etype (Array_Node);

               Array_Descr    : constant Value_T :=
                 Emit_LValue (Env, Prefix (Node));
               Array_Data_Ptr : constant Value_T :=
                 Array_Data (Env, Array_Descr, Array_Type);

               Idxs : Value_Array (1 .. List_Length (Expressions (Node)) + 1)
                 := (1 => Const_Int
                            (Intptr_T, 0, Sign_Extend => LLVM.Types.False),
                     others => <>);
               --  Operands for the GetElementPtr instruction: one for the
               --  pointer deference, and then one per array index.

               J : Nat := 2;

            begin
               for N of Iterate (Expressions (Node)) loop
                  --  Adjust the index according to the range lower bound

                  declare
                     User_Index    : constant Value_T :=
                       Emit_Expression (Env, N);
                     Dim_Low_Bound : constant Value_T :=
                       Array_Bound
                         (Env, Array_Descr, Array_Type, Low, Integer (J - 1));
                  begin
                     Idxs (J) :=
                       Sub (Env.Bld, User_Index, Dim_Low_Bound, "index");
                  end;

                  J := J + 1;
               end loop;

               return GEP
                 (Env.Bld, Array_Data_Ptr, Idxs, "array-element-access");
            end;

         when N_Slice =>
            declare
               Array_Node     : constant Node_Id := Prefix (Node);
               Array_Type     : constant Entity_Id := Etype (Array_Node);

               Array_Descr    : constant Value_T :=
                 Emit_LValue (Env, Array_Node);
               Array_Data_Ptr : constant Value_T :=
                 Array_Data (Env, Array_Descr, Array_Type);

               --  Compute how much we need to offset the array pointer. Slices
               --  can be built only on single-dimension arrays

               Index_Shift : constant Value_T :=
                 Sub
                   (Env.Bld,
                    Emit_Expression (Env, Low_Bound (Discrete_Range (Node))),
                    Array_Bound (Env, Array_Descr, Array_Type, Low),
                    "offset");
            begin
               return Bit_Cast
                 (Env.Bld,
                  GEP
                    (Env.Bld,
                     Array_Data_Ptr,
                     (Const_Int (Intptr_T, 0, Sign_Extend => LLVM.Types.False),
                      Index_Shift),
                     "array-shifted"),
                  Create_Access_Type (Env, Etype (Node)),
                  "slice");
            end;

         when others =>
            Error_Msg_N
              ("unhandled node kind: `" &
               Node_Kind'Image (Nkind (Node)) & "`", Node);
            raise Program_Error;
      end case;
   end Emit_LValue;

   ----------------------------
   -- Build_Short_Circuit_Op --
   ----------------------------

   function Build_Short_Circuit_Op
     (Env : Environ;
      Left, Right : Value_T;
      Op  : Short_Circuit_Operator) return Value_T
   is
      Result : constant Value_T :=
        Alloca (Env.Bld, Type_Of (Left), "scl-res-1");

      --  Block which contains the evaluation of the right part
      --  expression of the operator.

      Block_Right_Expr : constant Basic_Block_T :=
        Append_Basic_Block (Env.Current_Subp.Func, "scl-right-expr");

      --  Block containing the exit code (load the final cond value into
      --  Result

      Block_Exit : constant Basic_Block_T :=
        Append_Basic_Block (Env.Current_Subp.Func, "scl-exit");

   begin
      Store (Env.Bld, Left, Result);

      --  In the case of And, evaluate the right expression when Left is
      --  true. In the case of Or, evaluate it when Left is false.

      if Op = Op_And then
         Discard (Build_Cond_Br (Env.Bld, Left, Block_Right_Expr, Block_Exit));
      else
         Discard (Build_Cond_Br (Env.Bld, Left, Block_Exit, Block_Right_Expr));
      end if;

      --  Emit code for the evaluation of the right part expression

      Position_Builder_At_End (Env.Bld, Block_Right_Expr);

      declare
         Left_Result : constant Value_T := Load (Env.Bld, Result, "load-left");
         Res : Value_T;
      begin
         if Op = Op_And then
            Res := Build_And (Env.Bld, Left_Result, Right, "scl-and");
         else
            Res := Build_Or (Env.Bld, Left_Result, Right, "scl-or");
         end if;

         Store (Env.Bld, Res, Result);
         Discard (Build_Br (Env.Bld, Block_Exit));
      end;

      Position_Builder_At_End (Env.Bld, Block_Exit);
      return Load (Env.Bld, Result, "scl-final-res");
   end Build_Short_Circuit_Op;

   ---------------------
   -- Emit_Expression --
   ---------------------

   function Emit_Expression
     (Env : Environ; Node : Node_Id) return Value_T is

      function Emit_Expr (Node : Node_Id) return Value_T is
        (Emit_Expression (Env, Node));
      --  Shortcut to Emit_Expression. Used to implicitely pass the
      --  environment during recursion.

   begin
      if Is_Binary_Operator (Node) then
         case Nkind (Node) is
            when N_Op_Gt | N_Op_Lt | N_Op_Le | N_Op_Ge | N_Op_Eq | N_Op_Ne =>
               return Emit_Comparison
                 (Env,
                  Get_Preds (Nkind (Node)),
                  Get_Fullest_View (Etype (Left_Opnd (Node))),
                  Left_Opnd (Node), Right_Opnd (Node));

            when others =>
               null;
         end case;

         declare
            T    : constant Entity_Id := Etype (Left_Opnd (Node));
            LVal : constant Value_T :=
              Emit_Expr (Left_Opnd (Node));
            RVal : constant Value_T :=
              Emit_Expr (Right_Opnd (Node));
            Op : Value_T;

         begin
            case Nkind (Node) is
            when N_Op_Add =>
               if Is_Floating_Point_Type (T) then
                  Op := F_Add (Env.Bld, LVal, RVal, "add");
               else
                  Op := Add (Env.Bld, LVal, RVal, "add");
               end if;

            when N_Op_Subtract =>
               if Is_Floating_Point_Type (T) then
                  Op := F_Sub (Env.Bld, LVal, RVal, "sub");
               else
                  Op := Sub (Env.Bld, LVal, RVal, "sub");
               end if;

            when N_Op_Multiply =>
               if Is_Floating_Point_Type (T) then
                  Op := F_Mul (Env.Bld, LVal, RVal, "mul");
               else
                  Op := Mul (Env.Bld, LVal, RVal, "mul");
               end if;

            when N_Op_Divide =>
               if Is_Signed_Integer_Type (T) then
                  Op := S_Div (Env.Bld, LVal, RVal, "sdiv");
               elsif Is_Floating_Point_Type (T) then
                  Op := F_Div (Env.Bld, LVal, RVal, "fdiv");
               elsif Is_Unsigned_Type (T) then
                  return U_Div (Env.Bld, LVal, RVal, "udiv");
               else
                  Error_Msg_N
                    ("not handled: Division with type `" & T'Img & "`",
                     Node);
               end if;

            when N_Op_Rem =>
               Op :=
                 (if Is_Unsigned_Type (Etype (Left_Opnd (Node)))
                  then U_Rem (Env.Bld, LVal, RVal, "urem")
                  else S_Rem (Env.Bld, LVal, RVal, "srem"));

            when N_Op_And =>
               Op := Build_And (Env.Bld, LVal, RVal, "and");

            when N_Op_Or =>
               Op := Build_Or (Env.Bld, LVal, RVal, "or");

            when N_Op_Xor =>
               Op := Build_Xor (Env.Bld, LVal, RVal, "xor");

            when N_Op_Mod =>
               Op := U_Rem (Env.Bld, LVal, RVal, "mod");

            when N_Op_Shift_Left | N_Op_Shift_Right
               | N_Op_Shift_Right_Arithmetic
               | N_Op_Rotate_Left | N_Op_Rotate_Right
            =>
               return Emit_Shift (Env, Node, LVal, RVal);

            when others =>
               Error_Msg_N
                 ("unhandled node kind in expression: `" &
                  Node_Kind'Image (Nkind (Node)) & "`", Node);
            end case;

            --  No need to handle modulo manually for non binary modulus types,
            --  this is taken care of by the front-end.

            return Op;
         end;
      else
         case Nkind (Node) is
         when N_Expression_With_Actions =>
            if not Is_Empty_List (Actions (Node)) then
               --  ??? Should probably wrap this into a separate compound
               --  statement
               Emit_List (Env, Actions (Node));
            end if;

            return Emit_Expr (Expression (Node));

         when N_Character_Literal =>
            return Const_Int
              (Create_Type (Env, Etype (Node)),
               Char_Literal_Value (Node));

         when N_Integer_Literal =>
            return Const_Int
              (Create_Type (Env, Etype (Node)),
               Intval (Node));

         when N_Real_Literal =>
            if Is_Fixed_Point_Type (Underlying_Type (Etype (Node))) then
               return Const_Int
                 (Create_Type (Env, Etype (Node)),
                  Corresponding_Integer_Value (Node));
            else
               declare
                  Real_Type : constant Type_T :=
                    Create_Type (Env, Etype (Node));
                  Val       : Ureal := Realval (Node);

               begin
                  if UR_Is_Zero (Val) then
                     return Const_Real (Real_Type, 0.0);
                  end if;

                  --  First convert the value to a machine number if it isn't
                  --  already. That will force the base to 2 for non-zero
                  --  values and simplify the rest of the logic.

                  if not Is_Machine_Number (Node) then
                     Val := Machine
                       (Base_Type (Underlying_Type (Etype (Node))),
                        Val, Round_Even, Node);
                  end if;

                  --  ??? See trans.c (case N_Real_Literal) for handling of
                  --  N_Real_Literal in gigi.

                  if UI_Is_In_Int_Range (Numerator (Val))
                    and then UI_Is_In_Int_Range (Denominator (Val))
                  then
                     if UR_Is_Negative (Val) then
                        return Const_Real
                          (Real_Type,
                           -double (UI_To_Int (Numerator (Val))) /
                            double (UI_To_Int (Denominator (Val))));
                     else
                        return Const_Real
                          (Real_Type,
                           double (UI_To_Int (Numerator (Val))) /
                           double (UI_To_Int (Denominator (Val))));
                     end if;
                  else
                     --  ???
                     Error_Msg_N ("float point value too big", Node);
                     return Const_Real (Real_Type, 0.0);
                  end if;
               end;
            end if;

         when N_String_Literal =>
            declare
               String       : constant String_Id := Strval (Node);
               Array_Type   : constant Type_T :=
                 Create_Type (Env, Etype (Node));
               Element_Type : constant Type_T := Get_Element_Type (Array_Type);
               Length       : constant Interfaces.C.unsigned :=
                 Get_Array_Length (Array_Type);
               Elements     : array (1 .. Length) of Value_T;

            begin
               for J in Elements'Range loop
                  Elements (J) := Const_Int
                    (Element_Type,
                     unsigned_long_long
                       (Get_String_Char (String, Standard.Types.Int (J))),
                     Sign_Extend => LLVM.Types.False);
               end loop;

               return Const_Array (Element_Type, Elements'Address, Length);
            end;

         when N_And_Then =>
            return Build_Short_Circuit_Op
              (Env,
               Emit_Expr (Left_Opnd (Node)),
               Emit_Expr (Right_Opnd (Node)),
               Op_And);

         when N_Or_Else =>
            return Build_Short_Circuit_Op
              (Env,
               Emit_Expr (Left_Opnd (Node)),
               Emit_Expr (Right_Opnd (Node)),
               Op_Or);

         when N_Op_Not =>
            declare
               Expr : constant Value_T := Emit_Expr (Right_Opnd (Node));
            begin
               return Build_Xor
                 (Env.Bld, Expr, Const_Ones (Type_Of (Expr)), "not");
            end;

         when N_Op_Abs =>
            --  ??? Emit something like: X >= 0 ? X : -X;

            Error_Msg_N ("??unsupported operator abs", Node);
            return Emit_Expr (Right_Opnd (Node));

         when N_Op_Plus =>
            return Emit_Expr (Right_Opnd (Node));

         when N_Op_Minus =>
            if Is_Floating_Point_Type (Etype (Node)) then
               return F_Sub
                  (Env.Bld,
                   Const_Real
                     (Create_Type (Env, Etype (Node)), 0.0),
                   Emit_Expr (Right_Opnd (Node)),
                   "minus");

            else
               return Sub
                  (Env.Bld,
                   Const_Int
                     (Create_Type (Env, Etype (Node)), 0, LLVM.Types.False),
                   Emit_Expr (Right_Opnd (Node)),
                   "minus");
            end if;

         when N_Unchecked_Type_Conversion =>
            declare
               Val     : constant Value_T := Emit_Expr (Expression (Node));
               Dest_Ty : constant Type_T := Create_Type (Env, Etype (Node));
            begin
               if Is_Access_Type (Etype (Node))
                 and then (Is_Scalar_Type (Etype (Expression (Node)))
                           or else Is_Descendant_Of_Address
                             (Etype (Expression (Node))))
               then
                  return Int_To_Ptr (Env.Bld, Val, Dest_Ty, "unchecked-conv");
               elsif (Is_Scalar_Type (Etype (Node))
                      or else Is_Descendant_Of_Address (Etype (Node)))
                 and then (Is_Access_Type (Etype (Expression (Node))))
               then
                  return Ptr_To_Int (Env.Bld, Val, Dest_Ty, "unchecked-conv");
               elsif Is_Access_Type (Etype (Expression (Node))) then
                  return Pointer_Cast
                    (Env.Bld, Val, Dest_Ty, "unchecked-conv");
               elsif Is_Integer_Type (Etype (Node))
                 and then (Is_Integer_Type (Etype (Expression (Node))))
               then
                  return Int_Cast (Env.Bld, Val, Dest_Ty, "unchecked-conv");
               else
                  return Bit_Cast (Env.Bld, Val, Dest_Ty, "unchecked-conv");
               end if;
            end;

         when N_Type_Conversion | N_Qualified_Expression =>
            return Build_Type_Conversion
              (Env       => Env,
               Src_Type  => Etype (Expression (Node)),
               Dest_Type => Etype (Node),
               Value     => Emit_Expr (Expression (Node)));

         when N_Identifier | N_Expanded_Name =>
            --  What if Node is a formal parameter passed by reference???
            --  pragma Assert (not Is_Formal (Entity (Node)));

            --  N_Defining_Identifier nodes for enumeration literals are not
            --  stored in the environment. Handle them here.

            declare
               Def_Ident : constant Entity_Id := Entity (Node);
            begin
               if Ekind (Def_Ident) = E_Enumeration_Literal then
                  return Const_Int
                    (Create_Type (Env, Etype (Node)),
                     Enumeration_Rep (Def_Ident));

               --  Handle entities in Standard and ASCII on the fly

               elsif Sloc (Def_Ident) <= Standard_Location then
                  declare
                     N    : constant Node_Id := Get_Full_View (Def_Ident);
                     Decl : constant Node_Id := Declaration_Node (N);
                     Expr : Node_Id := Empty;

                  begin
                     if Nkind (Decl) /= N_Object_Renaming_Declaration then
                        Expr := Expression (Decl);
                     end if;

                     if Present (Expr)
                       and then Nkind_In (Expr, N_Character_Literal,
                                                N_Expanded_Name,
                                                N_Integer_Literal,
                                                N_Real_Literal)
                     then
                        return Emit_Expression (Env, Expr);

                     elsif Present (Expr)
                       and then Nkind (Expr) = N_Identifier
                       and then Ekind (Entity (Expr)) = E_Enumeration_Literal
                     then
                        return Const_Int
                          (Create_Type (Env, Etype (Node)),
                           Enumeration_Rep (Entity (Expr)));
                     else
                        return Emit_Expression (Env, N);
                     end if;
                  end;

               else
                  --  LLVM functions are pointers that cannot be dereferenced.
                  --  If Def_Ident is a subprogram, return it as-is, the caller
                  --  expects a pointer to a function anyway.

                  declare
                     Kind          : constant Entity_Kind := Ekind (Def_Ident);
                     Type_Kind     : constant Entity_Kind :=
                       Ekind (Etype (Def_Ident));
                     Is_Subprogram : constant Boolean :=
                       (Kind = E_Function
                        or else Kind = E_Procedure
                        or else Type_Kind = E_Subprogram_Type);
                     LValue        : constant Value_T := Env.Get (Def_Ident);

                  begin
                     return
                       (if Is_Subprogram
                        then LValue
                        else Load (Env.Bld, LValue, ""));
                  end;
               end if;
            end;

         when N_Function_Call =>
            return Emit_Call (Env, Node);

         when N_Explicit_Dereference =>
            --  Access to subprograms require special handling, see
            --  N_Identifier.

            declare
               Access_Value : constant Value_T := Emit_Expr (Prefix (Node));
            begin
               return
                 (if Ekind (Etype (Node)) = E_Subprogram_Type
                  then Access_Value
                  else Load (Env.Bld, Access_Value, ""));
            end;

         when N_Allocator =>
            declare
               Arg : array (1 .. 1) of Value_T :=
                 (1 => Size_Of (Create_Type (Env, Etype (Expression (Node)))));
            begin
               if Nkind (Expression (Node)) = N_Identifier then
                  return Bit_Cast
                    (Env.Bld,
                     Call
                       (Env.Bld,
                        Env.Default_Alloc_Fn, Arg'Address, 1, "alloc"),
                     Create_Type (Env, Etype (Node)),
                     "alloc_bc");

               else
                  Error_Msg_N ("unsupported form of N_Allocator", Node);
                  raise Program_Error;
               end if;
            end;

         when N_Reference =>
            return Emit_LValue (Env, Prefix (Node));

         when N_Attribute_Reference =>
            return Emit_Attribute_Reference (Env, Node);

         when N_Selected_Component =>
            declare
               Pfx_Val : constant Value_T :=
                 Emit_Expression (Env, Prefix (Node));
               Pfx_Ptr : constant Value_T :=
                 Alloca (Env.Bld, Type_Of (Pfx_Val), "pfx_ptr");
               Record_Component : constant Entity_Id :=
                 Parent (Entity (Selector_Name (Node)));

            begin
               Store (Env.Bld, Pfx_Val, Pfx_Ptr);
               return Load
                 (Env.Bld,
                  Record_Field_Offset (Env, Pfx_Ptr, Record_Component), "");
            end;

         when N_Indexed_Component | N_Slice =>
            return Load (Env.Bld, Emit_LValue (Env, Node), "");

         when N_Aggregate =>
            declare
               Agg_Type   : constant Entity_Id := Etype (Node);
               LLVM_Type  : constant Type_T :=
                 Create_Type (Env, Agg_Type);
               Result     : Value_T := Get_Undef (LLVM_Type);
               Cur_Expr   : Value_T;
               Cur_Index  : Integer;

            begin
               if Ekind (Agg_Type) in Record_Kind then
                  for Assoc of Iterate (Component_Associations (Node)) loop
                     Cur_Expr := Emit_Expr (Expression (Assoc));

                     for Choice of Iterate (Choices (Assoc)) loop
                        Cur_Index := Index_In_List
                          (Parent (Entity (Choice)));
                        Result := Insert_Value
                          (Env.Bld,
                           Result, Cur_Expr, unsigned (Cur_Index - 1), "");
                     end loop;
                  end loop;

                  --  Must be an array

               else
                  Cur_Index := 0;
                  for Expr of Iterate (Expressions (Node)) loop
                     Cur_Expr := Emit_Expr (Expr);
                     Result := Insert_Value
                       (Env.Bld, Result, Cur_Expr, unsigned (Cur_Index), "");
                     Cur_Index := Cur_Index + 1;
                  end loop;
               end if;

               return Result;
            end;

         when N_If_Expression =>
            return Emit_If (Env, Node);

         when N_Null =>
            return Const_Null (Create_Type (Env, Etype (Node)));

         when N_Defining_Identifier =>
            return Env.Get (Node);

         when others =>
            Error_Msg_N
              ("unsupported node kind: `" &
               Node_Kind'Image (Nkind (Node)) & "`", Node);
            raise Program_Error;
         end case;
      end if;
   end Emit_Expression;

   ---------------
   -- Emit_List --
   ---------------

   procedure Emit_List (Env : Environ; List : List_Id) is
   begin
      if Present (List) then
         for N of Iterate (List) loop
            Emit (Env, N);
         end loop;
      end if;
   end Emit_List;

   ---------------
   -- Emit_Call --
   ---------------

   function Emit_Call (Env : Environ; Call_Node : Node_Id) return Value_T is
      Subp        : constant Node_Id := Name (Call_Node);
      Params      : constant Entity_Iterator :=
        Get_Params (if Nkind (Subp) = N_Identifier
                    or else Nkind (Subp) = N_Expanded_Name
                    then Entity (Subp)
                    else Etype (Subp));
      Param_Assoc, Actual : Node_Id;
      Actual_Type         : Entity_Id;
      Current_Needs_Ptr   : Boolean;

      --  If it's not an identifier, it must be an access to a subprogram and
      --  in such a case, it must accept a static link.

      Takes_S_Link   : constant Boolean :=
        (Nkind (Subp) /= N_Identifier
         and then Nkind (Subp) /= N_Expanded_Name)
        or else Env.Takes_S_Link (Entity (Subp));

      S_Link         : Value_T;
      LLVM_Func      : Value_T;
      Args_Count     : constant Nat :=
        Params'Length + (if Takes_S_Link then 1 else 0);

      Args           : array (1 .. Args_Count) of Value_T;
      I, Idx         : Standard.Types.Int := 1;
      P_Type         : Entity_Id;
      Params_Offsets : Name_Maps.Map;

   begin
      for Param of Params loop
         Params_Offsets.Include (Chars (Param), I);
         I := I + 1;
      end loop;

      I := 1;

      LLVM_Func := Emit_Expression (Env, Name (Call_Node));

      if Nkind (Name (Call_Node)) /= N_Identifier
        and then Nkind (Name (Call_Node)) /= N_Expanded_Name
      then
         S_Link := Extract_Value (Env.Bld, LLVM_Func, 1, "static-link-ptr");
         LLVM_Func := Extract_Value (Env.Bld, LLVM_Func, 0, "callback");
      else
         S_Link := Get_Static_Link (Env, Entity (Name (Call_Node)));
      end if;

      Param_Assoc := First (Parameter_Associations (Call_Node));

      while Present (Param_Assoc) loop
         if Nkind (Param_Assoc) = N_Parameter_Association then
            Actual := Explicit_Actual_Parameter (Param_Assoc);
            Idx := Params_Offsets (Chars (Selector_Name (Param_Assoc)));
         else
            Actual := Param_Assoc;
            Idx := I;
         end if;

         Actual_Type := Etype (Actual);

         Current_Needs_Ptr := Param_Needs_Ptr (Params (Idx));
         Args (Idx) :=
           (if Current_Needs_Ptr
            then Emit_LValue (Env, Actual)
            else Emit_Expression (Env, Actual));

         P_Type := Etype (Params (Idx));

         --  At this point we need to handle view conversions: from array thin
         --  pointer to array fat pointer, unconstrained array pointer type
         --  conversion, ... For other parameters that needs to be passed
         --  as pointers, we should also make sure the pointed type fits
         --  the LLVM formal.

         if Is_Array_Type (Actual_Type) then
            if Is_Constrained (Actual_Type)
              and then not Is_Constrained (P_Type)
            then
               --  Convert from thin to fat pointer

               Args (Idx) :=
                 Array_Fat_Pointer (Env, Args (Idx), Actual_Type);

            elsif not Is_Constrained (Actual_Type)
              and then Is_Constrained (P_Type)
            then
               --  Convert from fat to thin pointer

               Args (Idx) := Array_Data (Env, Args (Idx), Actual_Type);
            end if;

         elsif Current_Needs_Ptr then
            Args (Idx) := Bit_Cast
              (Env.Bld,
               Args (Idx), Create_Access_Type (Env, P_Type),
               "param-bitcast");
         end if;

         I := I + 1;
         Param_Assoc := Next (Param_Assoc);
      end loop;

      --  Set the argument for the static link, if any

      if Takes_S_Link then
         Args (Args'Last) := S_Link;
      end if;

      --  If there are any types mismatches for arguments passed by reference,
      --  bitcast the pointer type.

      declare
         Args_Types : constant Type_Array :=
           Get_Param_Types (Type_Of (LLVM_Func));
      begin
         for J in Args'Range loop
            if Type_Of (Args (J)) /= Args_Types (J)
              and then Get_Type_Kind (Type_Of (Args (J))) = Pointer_Type_Kind
            then
               Args (J) := Bit_Cast
                 (Env.Bld, Args (J), Args_Types (J), "param-bitcast");
            end if;
         end loop;
      end;

      return
        Call
          (Env.Bld,
           LLVM_Func, Args'Address, Args'Length,
           --  Assigning a name to a void value is not possible with LLVM
           (if Nkind (Call_Node) = N_Function_Call then "subpcall" else ""));
   end Emit_Call;

   --------------------------
   -- Emit_Subprogram_Decl --
   --------------------------

   function Emit_Subprogram_Decl
     (Env : Environ; Subp_Spec : Node_Id) return Value_T
   is
      Def_Ident : constant Node_Id := Defining_Unit_Name (Subp_Spec);
   begin
      --  If this subprogram specification has already been compiled, do
      --  nothing.

      if Env.Has_Value (Def_Ident) then
         return Env.Get (Def_Ident);

      else
         declare
            Subp_Type : constant Type_T :=
              Create_Subprogram_Type_From_Spec (Env, Subp_Spec);

            Subp_Base_Name : constant String :=
                Get_Name_String (Chars (Def_Ident));
            Subp_Name : constant String :=
              (if Scope_Depth_Value (Def_Ident) > 1
               then Subp_Base_Name
               else "_ada_" & Subp_Base_Name);

            LLVM_Func : constant Value_T :=
              Add_Function (Env.Mdl, Subp_Name, Subp_Type);
         begin
            --  Define the appropriate linkage

            if not Is_Public (Def_Ident) then
               Set_Linkage (LLVM_Func, Internal_Linkage);
            end if;

            Env.Set (Def_Ident, LLVM_Func);
            return LLVM_Func;
         end;
      end if;
   end Emit_Subprogram_Decl;

   -----------------------------
   -- Create_Callback_Wrapper --
   -----------------------------

   function Create_Callback_Wrapper
     (Env : Environ; Subp : Entity_Id) return Value_T
   is
      use Value_Maps;
      Wrapper : constant Cursor := Env.Subp_Wrappers.Find (Subp);

      Result : Value_T;
   begin
      if Wrapper /= No_Element then
         return Element (Wrapper);
      end if;

      --  This subprogram is referenced, and thus should at least already be
      --  declared. Thus, it must be registered in the environment.

      Result := Env.Get (Subp);

      if not Env.Takes_S_Link (Subp) then
         --  This is a top-level subprogram: wrap it so it can take a static
         --  link as its last argument.

         declare
            Func_Type   : constant Type_T :=
              Get_Element_Type (Type_Of (Result));
            Name        : constant String := Get_Value_Name (Result) & "__CB";
            Return_Type : constant Type_T := Get_Return_Type (Func_Type);
            Args_Count  : constant unsigned :=
              Count_Param_Types (Func_Type) + 1;
            Args        : array (1 .. Args_Count) of Type_T;
         begin
            Get_Param_Types (Func_Type, Args'Address);
            Args (Args'Last) :=
              Pointer_Type (Int8_Type_In_Context (Env.Ctx), 0);
            Result := Add_Function
              (Env.Mdl,
               Name,
               Function_Type
                 (Return_Type,
                  Args'Address, Args'Length,
                  Is_Var_Arg => LLVM.Types.False));
         end;
      end if;

      Env.Subp_Wrappers.Insert (Subp, Result);
      return Result;
   end Create_Callback_Wrapper;

   ----------------------------------
   -- Attach_Callback_Wrapper_Body --
   ----------------------------------

   procedure Attach_Callback_Wrapper_Body
     (Env : Environ; Subp : Entity_Id; Wrapper : Value_T)
   is
   begin
      if Env.Takes_S_Link (Subp) then
         return;
      end if;

      declare
         BB        : constant Basic_Block_T := Get_Insert_Block (Env.Bld);
         --  Back up the current insert block not to break the caller's
         --  workflow.

         Subp_Spec : constant Node_Id := Parent (Subp);
         Func      : constant Value_T := Emit_Subprogram_Decl (Env, Subp_Spec);
         Func_Type : constant Type_T := Get_Element_Type (Type_Of (Func));

         Call      : Value_T;
         Args      : array (1 .. Count_Param_Types (Func_Type) + 1) of Value_T;
      begin
         Position_Builder_At_End
           (Env.Bld,
            Append_Basic_Block_In_Context (Env.Ctx, Wrapper, ""));

         --  The wrapper must call the wrapped function with the same argument
         --  and return its result, if any.

         Get_Params (Wrapper, Args'Address);
         Call := LLVM.Core.Call
           (Env.Bld, Func, Args'Address, Args'Length - 1, "");

         if Get_Return_Type (Func_Type) = Void_Type then
            Discard (Build_Ret_Void (Env.Bld));
         else
            Discard (Build_Ret (Env.Bld, Call));
         end if;

         Position_Builder_At_End (Env.Bld, BB);
      end;
   end Attach_Callback_Wrapper_Body;

   --------------------------------
   -- Match_Static_Link_Variable --
   --------------------------------

   procedure Match_Static_Link_Variable
     (Env       : Environ;
      Def_Ident : Entity_Id;
      LValue    : Value_T)
   is
      use Defining_Identifier_Vectors;

      Subp   : Subp_Env;
      S_Link : Value_T;
   begin
      --  There is no static link variable to look for if we are at compilation
      --  unit top-level.

      if Env.Current_Subps.Length < 1 then
         return;
      end if;

      Subp := Env.Current_Subp;

      for Cur in Subp.S_Link_Descr.Closure.Iterate loop
         if Element (Cur) = Def_Ident then
            S_Link := Load (Env.Bld, Subp.S_Link, "static-link");
            S_Link := Insert_Value
              (Env.Bld,
               S_Link,
               LValue,
               unsigned (To_Index (Cur)),
               "updated-static-link");
            Store (Env.Bld, S_Link, Subp.S_Link);

            return;
         end if;
      end loop;
   end Match_Static_Link_Variable;

   ---------------------
   -- Get_Static_Link --
   ---------------------

   function Get_Static_Link
     (Env  : Environ;
      Subp : Entity_Id) return Value_T
   is
      Result_Type : constant Type_T :=
        Pointer_Type (Int8_Type_In_Context (Env.Ctx), 0);
      Result      : Value_T;

      --  In this context, the "caller" is the subprogram that creates an
      --  access to subprogram or that calls directly a subprogram, and the
      --  "caller" is the target subprogram.

      Caller_SLD, Callee_SLD : Static_Link_Descriptor;

      Idx_Type : constant Type_T := Int32_Type_In_Context (Env.Ctx);
      Zero     : constant Value_T := Const_Null (Idx_Type);
      Idx      : constant Value_Array (1 .. 2) := (Zero, Zero);

   begin
      if Env.Takes_S_Link (Subp) then
         Caller_SLD := Env.Current_Subp.S_Link_Descr;
         Callee_SLD := Env.Get_S_Link (Subp);
         Result     := Env.Current_Subp.S_Link;

         --  The language rules force the parent subprogram of the callee to be
         --  the caller or one of its parent.

         while Callee_SLD.Parent /= Caller_SLD loop
            Caller_SLD := Caller_SLD.Parent;
            Result := Load
              (Env.Bld,
               GEP (Env.Bld, Result, Idx'Address, Idx'Length, ""), "");
         end loop;

         return Bit_Cast (Env.Bld, Result, Result_Type, "");

      else
         --  We end up here for external (and thus top-level) subprograms, so
         --  they take no static link.

         return Const_Null (Result_Type);
      end if;
   end Get_Static_Link;

   ---------------------------
   -- Build_Type_Conversion --
   ---------------------------

   function Build_Type_Conversion
     (Env                 : Environ;
      Src_Type, Dest_Type : Entity_Id;
      Value               : Value_T) return Value_T
   is
      S_Type  : constant Entity_Id := Get_Fullest_View (Src_Type);
      D_Type  : constant Entity_Id := Get_Fullest_View (Dest_Type);
   begin
      --  For the moment, we handle only the simple cases of scalar and
      --  float conversions.

      if Is_Access_Type (D_Type) then
         return Pointer_Cast
           (Env.Bld,
            Value, Create_Type (Env, D_Type), "ptr-conv");

      elsif Is_Floating_Point_Type (S_Type)
        and then Is_Floating_Point_Type (D_Type)
      then
         if RM_Size (S_Type) = RM_Size (D_Type) then
            return Value;
         else
            Error_Msg_N ("unsupported floating point conversion", Src_Type);
            return Value;
         end if;

      elsif Is_Discrete_Or_Fixed_Point_Type (S_Type)
        and then Is_Discrete_Or_Fixed_Point_Type (D_Type)
      then
         --  ??? Consider using Int_Cast instead
         --  return Int_Cast
         --    (Env.Bld, Val, Create_Type (Env, D_Type), "int-conv");

         declare
            Dest_LLVM_Type : constant Type_T := Create_Type (Env, D_Type);
         begin
            if RM_Size (S_Type) = RM_Size (D_Type) then
               return Value;

            elsif RM_Size (S_Type) < RM_Size (D_Type) then
               if Is_Unsigned_Type (Dest_Type) then

                  --  ??? raise an exception if the value is negative (hence
                  --  the source type has to be checked).

                  return Z_Ext (Env.Bld, Value, Dest_LLVM_Type, "int_conv");

               else
                  return S_Ext (Env.Bld, Value, Dest_LLVM_Type, "int_conv");
               end if;
            else
               return Trunc (Env.Bld, Value, Dest_LLVM_Type, "int_conv");
            end if;
         end;

      elsif Is_Descendant_Of_Address (Src_Type)
        and then Is_Descendant_Of_Address (Dest_Type)
      then
         return Bit_Cast
           (Env.Bld,
            Value,
            Create_Type (Env, Dest_Type),
            "address-conv");

      else
         Error_Msg_N ("unsupported type conversion", Src_Type);
         raise Program_Error;
      end if;
   end Build_Type_Conversion;

   ------------------
   -- Emit_Min_Max --
   ------------------

   function Emit_Min_Max
     (Env         : Environ;
      Exprs       : List_Id;
      Compute_Max : Boolean) return Value_T
   is
      Name      : constant String :=
        (if Compute_Max then "max" else "min");

      Expr_Type : constant Entity_Id := Etype (First (Exprs));
      Left      : constant Value_T := Emit_Expression (Env, First (Exprs));
      Right     : constant Value_T := Emit_Expression (Env, Last (Exprs));

      Comparison_Operators : constant
        array (Boolean, Boolean) of Int_Predicate_T :=
        (True  => (True => Int_UGT, False => Int_ULT),
         False => (True => Int_SGT, False => Int_SLT));
      --  Provide the appropriate scalar comparison operator in order to select
      --  the min/max. First index = is unsigned? Second one = computing max?

      Choose_Left : constant Value_T := I_Cmp
        (Env.Bld,
         Comparison_Operators (Is_Unsigned_Type (Expr_Type), Compute_Max),
         Left, Right,
         "choose-left-as-" & Name);

   begin
      return Build_Select (Env.Bld, Choose_Left, Left, Right, Name);
   end Emit_Min_Max;

   ------------------------------
   -- Emit_Attribute_Reference --
   ------------------------------

   function Emit_Attribute_Reference
     (Env  : Environ;
      Node : Node_Id) return Value_T
   is
      Attr : constant Attribute_Id := Get_Attribute_Id (Attribute_Name (Node));
   begin
      case Attr is
         when Attribute_Access
            | Attribute_Unchecked_Access
            | Attribute_Unrestricted_Access =>

            --  We store values as pointers, so, getting an access to an
            --  expression is the same thing as getting an LValue, and has
            --  the same constraints.

            return Emit_LValue (Env, Prefix (Node));

         when Attribute_Address =>

            --  Likewise for addresses

            return Ptr_To_Int
              (Env.Bld,
               Emit_LValue
                 (Env, Prefix (Node)), Get_Address_Type, "to-address");

         when Attribute_First
            | Attribute_Last
            | Attribute_Length =>

            --  Note that there is no need to handle these attributes for
            --  scalar subtypes since the front-end expands them into
            --  constant references.

            declare
               Array_Descr : Value_T;
               Array_Type  : Entity_Id;
            begin
               Extract_Array_Info
                 (Env, Prefix (Node), Array_Descr, Array_Type);
               if Attr = Attribute_Length then
                  return Array_Length (Env, Array_Descr, Array_Type);
               else
                  return Array_Bound
                    (Env, Array_Descr, Array_Type,
                     (if Attr = Attribute_First then Low else High));
               end if;
            end;

         when Attribute_Max
            | Attribute_Min =>
            return Emit_Min_Max
              (Env,
               Expressions (Node),
               Attr = Attribute_Max);

         when Attribute_Pos
            | Attribute_Val =>
            pragma Assert (List_Length (Expressions (Node)) = 1);
            return Build_Type_Conversion
              (Env,
               Etype (First (Expressions (Node))),
               Etype (Node),
               Emit_Expression (Env, First (Expressions (Node))));

         when Attribute_Succ
            | Attribute_Pred =>
            declare
               Exprs : constant List_Id := Expressions (Node);
               pragma Assert (List_Length (Exprs) = 1);

               Base : constant Value_T := Emit_Expression (Env, First (Exprs));
               T    : constant Type_T := Type_Of (Base);
               pragma Assert (Get_Type_Kind (T) = Integer_Type_Kind);

               One  : constant Value_T :=
                 Const_Int (T, 1, Sign_Extend => LLVM.Types.False);

            begin
               return
                 (if Attr = Attribute_Succ
                  then Add (Env.Bld, Base, One, "attr-succ")
                  else Sub (Env.Bld, Base, One, "attr-pred"));
            end;

         when Attribute_Machine =>
            --  ??? For now return the prefix itself. Would need to force a
            --  store in some cases.

            return Emit_Expression (Env, First (Expressions (Node)));

         when others =>
            Error_Msg_N
              ("unsupported attribute: `" &
               Attribute_Id'Image (Attr) & "`", Node);
            raise Program_Error;
      end case;
   end Emit_Attribute_Reference;

   ---------------------
   -- Emit_Comparison --
   ---------------------

   function Emit_Comparison
     (Env          : Environ;
      Operation    : Pred_Mapping;
      Operand_Type : Entity_Id;
      LHS, RHS     : Node_Id) return Value_T is
   begin
      --  LLVM treats pointers as integers regarding comparison

      if Is_Floating_Point_Type (Operand_Type) then
         return F_Cmp
           (Env.Bld,
            Operation.Real,
            Emit_Expression (Env, LHS),
            Emit_Expression (Env, RHS),
            "fcmp");

      elsif Is_Discrete_Or_Fixed_Point_Type (Operand_Type)
        or else Is_Access_Type (Operand_Type)
      then
         return I_Cmp
           (Env.Bld,
            (if Is_Unsigned_Type (Operand_Type)
             then Operation.Unsigned
             else Operation.Signed),
            Emit_Expression (Env, LHS),
            Emit_Expression (Env, RHS),
            "icmp");

      elsif Is_Record_Type (Operand_Type) then
         Error_Msg_N ("unsupported record comparison", LHS);
         raise Program_Error;

      elsif Is_Array_Type (Operand_Type) then
         pragma Assert (Operation.Signed in Int_EQ | Int_NE);

         --  ??? Handle multi-dimensional arrays

         declare
            --  Because of runtime length checks, the comparison is made as
            --  follows:
            --     L_Length <- LHS'Length
            --     R_Length <- RHS'Length
            --     if L_Length /= R_Length then
            --        return False;
            --     elsif L_Length = 0 then
            --        return True;
            --     else
            --        return memory comparison;
            --     end if;
            --  We are generating LLVM IR (SSA form), so the return mechanism
            --  is implemented with control-flow and PHI nodes.

            Bool_Type    : constant Type_T := Int_Ty (1);
            False_Val    : constant Value_T :=
              Const_Int (Bool_Type, 0, LLVM.Types.False);
            True_Val     : constant Value_T :=
              Const_Int (Bool_Type, 1, LLVM.Types.False);

            LHS_Descr    : constant Value_T := Emit_LValue (Env, LHS);
            LHS_Type     : constant Entity_Id := Etype (LHS);
            RHS_Descr    : constant Value_T := Emit_LValue (Env, RHS);
            RHS_Type     : constant Entity_Id := Etype (RHS);

            Left_Length  : constant Value_T :=
              Array_Length (Env, LHS_Descr, LHS_Type);
            Right_Length : constant Value_T :=
              Array_Length (Env, RHS_Descr, RHS_Type);
            Null_Length  : constant Value_T :=
              Const_Null (Type_Of (Left_Length));
            Same_Length  : constant Value_T := I_Cmp
              (Env.Bld, Int_NE, Left_Length, Right_Length, "test-same-length");

            Basic_Blocks : constant Basic_Block_Array (1 .. 3) :=
              (Get_Insert_Block (Env.Bld),
               Create_Basic_Block (Env, "when-null-length"),
               Create_Basic_Block (Env, "when-same-length"));
            Results      : Value_Array (1 .. 3);
            BB_Merge     : constant Basic_Block_T :=
              Create_Basic_Block (Env, "array-cmp-merge");
            Phi          : Value_T;

         begin
            Discard
              (Build_Cond_Br
                (Env.Bld,
                 C_If   => Same_Length,
                 C_Then => BB_Merge,
                 C_Else => Basic_Blocks (2)));
            Results (1) := False_Val;

            --  If we jump from here to BB_Merge, we are returning False

            Position_Builder_At_End (Env.Bld, Basic_Blocks (2));
            Discard
              (Build_Cond_Br
                (Env.Bld,
                 C_If   => I_Cmp
                   (Env.Bld, Int_EQ, Left_Length,
                    Null_Length, "test-null-length"),
                 C_Then => BB_Merge,
                 C_Else => Basic_Blocks (3)));
            Results (2) := True_Val;

            --  If we jump from here to BB_Merge, we are returning True

            Position_Builder_At_End (Env.Bld, Basic_Blocks (3));

            declare
               Left        : constant Value_T :=
                 Array_Data (Env, LHS_Descr, LHS_Type);
               Right       : constant Value_T :=
                 Array_Data (Env, RHS_Descr, RHS_Type);

               Void_Ptr_Type : constant Type_T := Pointer_Type (Int_Ty (8), 0);
               Size_Type     : constant Type_T := Int_Ty (64);
               Size          : constant Value_T :=
                 Mul
                   (Env.Bld,
                    Z_Ext (Env.Bld, Left_Length, Size_Type, ""),
                    Get_Type_Size
                      (Env, Create_Type (Env, Component_Type (Etype (LHS)))),
                    "byte-size");

               Memcmp_Args : constant Value_Array (1 .. 3) :=
                 (Bit_Cast (Env.Bld, Left, Void_Ptr_Type, ""),
                  Bit_Cast (Env.Bld, Right, Void_Ptr_Type, ""),
                  Size);
               Memcmp      : constant Value_T := Call
                 (Env.Bld,
                  Env.Memory_Cmp_Fn,
                  Memcmp_Args'Address, Memcmp_Args'Length,
                  "");
            begin
               --  The two arrays are equal iff. the call to memcmp returned 0

               Results (3) := I_Cmp
                 (Env.Bld,
                  Operation.Signed,
                  Memcmp,
                  Const_Null (Type_Of (Memcmp)),
                  "array-comparison");
            end;
            Discard (Build_Br (Env.Bld, BB_Merge));

            --  If we jump from here to BB_Merge, we are returning the result
            --  of the memory comparison.

            Position_Builder_At_End (Env.Bld, BB_Merge);
            Phi := LLVM.Core.Phi (Env.Bld, Bool_Type, "");
            Add_Incoming (Phi, Results'Address, Basic_Blocks'Address, 3);
            return Phi;
         end;

      else
         Error_Msg_N
           ("unsupported operand type for comparison: `"
            & Entity_Kind'Image (Ekind (Operand_Type)) & "`", LHS);
         raise Program_Error;
      end if;
   end Emit_Comparison;

   -------------
   -- Emit_If --
   -------------

   function Emit_If
     (Env  : Environ;
      Node : Node_Id) return Value_T
   is
      Is_Stmt : constant Boolean := Nkind (Node) = N_If_Statement;
      --  Depending on the node to translate, we will have to compute and
      --  return an expression.

      GNAT_Cond : constant Node_Id :=
        (if Is_Stmt
         then Condition (Node)
         else Pick (Expressions (Node), 1));
      Cond      : constant Value_T := Emit_Expression (Env, GNAT_Cond);

      BB_Then, BB_Else, BB_Next : Basic_Block_T;
      --  BB_Then is the basic block we jump to if the condition is true.
      --  BB_Else is the basic block we jump to if the condition is false.
      --  BB_Next is the BB we jump to after the IF is executed.

      Then_Value, Else_Value : Value_T;

   begin
      BB_Next := Create_Basic_Block (Env, "if-next");
      BB_Then := Create_Basic_Block (Env, "if-then");

      --  If this is an IF statement without ELSE part, then we jump to the
      --  BB_Next when the condition is false. Thus, BB_Else and BB_Next
      --  should be the same in this case.

      BB_Else :=
        (if not Is_Stmt or else not Is_Empty_List (Else_Statements (Node))
         then Create_Basic_Block (Env, "if-else")
         else BB_Next);

      Discard (Build_Cond_Br (Env.Bld, Cond, BB_Then, BB_Else));

      --  Emit code for the THEN part

      Position_Builder_At_End (Env.Bld, BB_Then);

      if Is_Stmt then
         Emit_List (Env, Then_Statements (Node));
      else
         Then_Value := Emit_Expression (Env, Pick (Expressions (Node), 2));

         --  The THEN part may be composed of multiple basic blocks. We want
         --  to get the one that jumps to the merge point to get the PHI node
         --  predecessor.

         BB_Then := Get_Insert_Block (Env.Bld);
      end if;

      Discard (Build_Br (Env.Bld, BB_Next));

      --  Emit code for the ELSE part

      Position_Builder_At_End (Env.Bld, BB_Else);

      if not Is_Stmt then
         Else_Value := Emit_Expression (Env, Pick (Expressions (Node), 3));
         Discard (Build_Br (Env.Bld, BB_Next));

         --  We want to get the basic blocks that jumps to the merge point: see
         --  above.

         BB_Else := Get_Insert_Block (Env.Bld);

      elsif not Is_Empty_List (Else_Statements (Node)) then
         Emit_List (Env, Else_Statements (Node));
         Discard (Build_Br (Env.Bld, BB_Next));
      end if;

      --  Then prepare the instruction builder for the next
      --  statements/expressions and return a merged expression if needed.

      Position_Builder_At_End (Env.Bld, BB_Next);

      if Is_Stmt then
         return No_Value_T;
      else
         declare
            Values : constant Value_Array (1 .. 2) :=
              (Then_Value, Else_Value);
            BBs    : constant Basic_Block_Array (1 .. 2) :=
              (BB_Then, BB_Else);
            Phi    : constant Value_T :=
              LLVM.Core.Phi (Env.Bld, Type_Of (Then_Value), "");
         begin
            Add_Incoming (Phi, Values'Address, BBs'Address, 2);
            return Phi;
         end;
      end if;
   end Emit_If;

   ----------------
   -- Emit_Shift --
   ----------------

   function Emit_Shift
     (Env       : Environ;
      Node      : Node_Id;
      LHS, RHS  : Value_T) return Value_T
   is
      To_Left, Rotate, Arithmetic : Boolean := False;

      Operation : constant Node_Kind := Nkind (Node);
      Result    : Value_T := LHS;
      LHS_Type  : constant Type_T := Type_Of (LHS);
      N         : Value_T := S_Ext (Env.Bld, RHS, LHS_Type, "bits");
      LHS_Bits  : constant Value_T := Const_Int
        (LHS_Type,
         unsigned_long_long (Get_Int_Type_Width (LHS_Type)),
         Sign_Extend => LLVM.Types.False);

      Saturated  : Value_T;

   begin
      --  Extract properties for the operation we are asked to generate code
      --  for.

      case Operation is
         when N_Op_Shift_Left =>
            To_Left := True;
         when N_Op_Shift_Right =>
            null;
         when N_Op_Shift_Right_Arithmetic =>
            Arithmetic := True;
         when N_Op_Rotate_Left =>
            To_Left := True;
            Rotate := True;
         when N_Op_Rotate_Right =>
            Rotate := True;
         when others =>
            Error_Msg_N
              ("unsupported shift/rotate operation: `"
               & Node_Kind'Image (Operation) & "`", Node);
            raise Program_Error;
      end case;

      if Rotate then

         --  While LLVM instructions will return an undefined value for
         --  rotations with too many bits, we must handle "multiple turns",
         --  so first get the number of bit to rotate modulo the size of the
         --  operand.

         --  Note that the front-end seems to already compute the modulo, but
         --  just in case...

         N := U_Rem (Env.Bld, N, LHS_Bits, "effective-rotating-bits");

         declare
            --  There is no "rotate" instruction in LLVM, so we have to stick
            --  to shift instructions, just like in C. If we consider that we
            --  are rotating to the left:

            --     Result := (Operand << Bits) | (Operand >> (Size - Bits));
            --               -----------------   --------------------------
            --                    Upper                   Lower

            --  If we are rotating to the right, we switch the direction of the
            --  two shifts.

            Lower_Shift : constant Value_T :=
              Sub (Env.Bld, LHS_Bits, N, "lower-shift");
            Upper       : constant Value_T :=
              (if To_Left
               then Shl (Env.Bld, LHS, N, "rotate-upper")
               else L_Shr (Env.Bld, LHS, N, "rotate-upper"));
            Lower       : constant Value_T :=
              (if To_Left
               then L_Shr (Env.Bld, LHS, Lower_Shift, "rotate-lower")
               else Shl (Env.Bld, LHS, Lower_Shift, "rotate-lower"));

         begin
            return Build_Or (Env.Bld, Upper, Lower, "rotate-result");
         end;

      else
         --  If the number of bits shifted is bigger or equal than the number
         --  of bits in LHS, the underlying LLVM instruction returns an
         --  undefined value, so build what we want ourselves (we call this
         --  a "saturated value").

         Saturated :=
           (if Arithmetic

            --  If we are performing an arithmetic shift, the saturated value
            --  is 0 if LHS is positive, -1 otherwise (in this context, LHS is
            --  always interpreted as a signed integer).

            then Build_Select
              (Env.Bld,
               C_If   => I_Cmp
                 (Env.Bld, Int_SLT, LHS,
                  Const_Null (LHS_Type), "is-lhs-negative"),
               C_Then => Const_Ones (LHS_Type),
               C_Else => Const_Null (LHS_Type),
               Name   => "saturated")

            else Const_Null (LHS_Type));

         --  Now, compute the value using the underlying LLVM instruction
         Result :=
           (if To_Left
            then Shl (Env.Bld, LHS, N, "shift-left-raw")
            else
              (if Arithmetic
               then A_Shr (Env.Bld, LHS, N, "lshift-right-raw")
               else L_Shr (Env.Bld, LHS, N, "ashift-right-raw")));

         --  Now, we must decide at runtime if it is safe to rely on the
         --  underlying LLVM instruction. If so, use it, otherwise return
         --  the saturated value.

         return Build_Select
           (Env.Bld,
            C_If   => I_Cmp (Env.Bld, Int_UGE, N, LHS_Bits, "is-saturated"),
            C_Then => Saturated,
            C_Else => Result,
            Name   => "shift-rotate-result");
      end if;
   end Emit_Shift;

end GNATLLVM.Compile;
