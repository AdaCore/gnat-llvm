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

with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;
with System;

with Exp_Unst; use Exp_Unst;
with Errout;   use Errout;
with Eval_Fat; use Eval_Fat;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Sem_Aggr; use Sem_Aggr;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Table;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

with LLVM.Analysis; use LLVM.Analysis;
with LLVM.Core; use LLVM.Core;

with GNATLLVM.Arrays;       use GNATLLVM.Arrays;
with GNATLLVM.Types;        use GNATLLVM.Types;
with GNATLLVM.Utils;        use GNATLLVM.Utils;

package body GNATLLVM.Compile is

   --  Note: in order to find the right LLVM instruction to generate,
   --  you can compare with what Clang generates on corresponding C or C++
   --  code. This can be done online via http://ellcc.org/demo/index.cgi

   --  See also DragonEgg sources for comparison on how GCC nodes are converted
   --  to LLVM nodes: http://llvm.org/svn/llvm-project/dragonegg/trunk

   function Allocate_For_Type
     (Env : Environ; TE : Entity_Id; Name : String) return Value_T
     with Pre  => Env /= null and Is_Type (TE),
          Post => Allocate_For_Type'Result /= No_Value_T;
   --  Allocate space on the stack for an object of type TE and return
   --  a pointer to the space.  Name is the name to use for the LLVM value.

   function Build_Type_Conversion
     (Env                 : Environ;
      Src_Type, Dest_Type : Entity_Id;
      Expr                : Node_Id) return Value_T;
   --  Emit code to convert Expr to Dest_Type

   function Build_Unchecked_Conversion
     (Env                 : Environ;
      S_Type, D_Type      : Entity_Id;
      Expr                : Node_Id) return Value_T;
   --  Emit code to emit an unchecked conversion of Expr to Dest_Type

   function Get_Static_Link (Env : Environ; Node : Entity_Id) return Value_T
     with Pre  => Env /= null and then Present (Node),
          Post => Get_Static_Link'Result /= No_Value_T;
   --  Build and return the static link to pass to a call to Node

   function Compute_Size
     (Env                 : Environ;
      Left_Typ, Right_Typ : Entity_Id;
      Right_Value         : Value_T) return Value_T
     with Pre  => Env /= null and then Is_Type (Left_Typ)
                  and then Present (Right_Typ)
                  and then Right_Value /= No_Value_T,
          Post =>  Compute_Size'Result /= No_Value_T;
   --  Helper for assignments

   function Convert_Scalar_Types
     (Env            : Environ;
      S_Type, D_Type : Entity_Id;
      Expr           : Node_Id) return Value_T
     with Pre  => Env /= null and then Is_Type (S_Type)
                  and then Is_Type (D_Type) and then Present (Expr),
          Post => Convert_Scalar_Types'Result /= No_Value_T;
   --  Helper of Build_Type_Conversion if both types are scalar.

   function Convert_To_Scalar_Type
     (Env         : Environ;
      Expr        : Value_T;
      T           : Entity_Id;
      Is_Unsigned : Boolean) return Value_T
     with Pre  => Env /= null and then Expr /= No_Value_T
                  and then Is_Type (T),
          Post => Convert_To_Scalar_Type'Result /= No_Value_T;
   --  Variant of above to convert an LLVM Expr to the type T.

   function Build_Short_Circuit_Op
     (Env                   : Environ;
      Node_Left, Node_Right : Node_Id;
      Orig_Left, Orig_Right : Value_T;
      Op                    : Node_Kind) return Value_T
     with Pre  => Env /= null
                  and then (Present (Node_Left)
                              or else Orig_Left /= No_Value_T)
                  and then (Present (Node_Right)
                              or else Orig_Right /= No_Value_T),
          Post => Build_Short_Circuit_Op'Result /= No_Value_T;
   --  Emit the LLVM IR for a short circuit operator ("or else", "and then")
   --  If we've already computed one or more of the expressions, we
   --  pass those as Orig_Left and Orig_Right; if not, Node_Left and
   --  Node_Right will be the Node_Ids to be used for the computation.  This
   --  allows sharing this code for multiple cases.

   function Emit_Attribute_Reference
     (Env    : Environ;
      Node   : Node_Id;
      LValue : Boolean) return Value_T
     with Pre  => Env /= null and then Nkind (Node) = N_Attribute_Reference,
          Post => Emit_Attribute_Reference'Result /= No_Value_T;
   --  Helper for Emit_Expression: handle N_Attribute_Reference nodes

   procedure Emit_Assignment
     (Env                       : Environ;
      LHS_Typ, RHS_Typ          : Entity_Id;
      LValue                    : Value_T;
      E                         : Node_Id;
      E_Value                   : Value_T;
      Forwards_OK, Backwards_OK : Boolean)
     with Pre => Env /= null and then Is_Type (LHS_Typ)
                 and then Is_Type (RHS_Typ)
                 and then (LValue /= No_Value_T or else Present (E));
   --  Helper for Emit: Copy the value of the expression E to LValue
   --  with the specified destination and expression types

   function Emit_Call
     (Env : Environ; Call_Node : Node_Id) return Value_T
     with Pre  => Env /= null and then Nkind (Call_Node) in N_Subprogram_Call,
          Post => Emit_Call'Result /= No_Value_T;
   --  Helper for Emit/Emit_Expression: compile a call statement/expression and
   --  return its result value.

   function Emit_Comparison
     (Env      : Environ;
      Kind     : Node_Kind;
      Op_Type  : Entity_Id;
      LHS, RHS : Node_Id) return Value_T
     with Pre  => Env /= null and then Is_Type (Op_Type)
                  and then Present (LHS) and then Present (RHS),
          Post => Emit_Comparison'Result /= No_Value_T;

   function Emit_Comparison
     (Env          : Environ;
      Kind         : Node_Kind;
      Operand_Type : Entity_Id;
      Node         : Node_Id;
      LHS, RHS     : Value_T) return Value_T
     with Pre  => Env /= null and then Is_Type (Operand_Type)
                  and then Present (Node) and then LHS /= No_Value_T
                  and then RHS /= No_Value_T,
          Post => Emit_Comparison'Result /= No_Value_T;
   --  Helpers for Emit_Expression: handle comparison operations.
   --  The second form only supports discrete or pointer types.

   procedure Emit_If (Env : Environ; Node : Node_Id)
     with Pre => Env /= null and then Nkind (Node) = N_If_Statement;
   --  Helper for Emit: handle if statements

   procedure Emit_If_Cond
     (Env               : Environ;
      Cond              : Node_Id;
      BB_True, BB_False : Basic_Block_T)
     with Pre => Env /= null and then Present (Cond)
                 and then BB_True /= No_BB_T and then BB_False /= No_BB_T;
   --  Helper for Emit_If to generate branch to BB_True or BB_False
   --  depending on whether Node is true or false.

   function Emit_If_Expression
     (Env  : Environ;
      Node : Node_Id) return Value_T
     with Pre  => Env /= null and then Nkind (Node) = N_If_Expression,
          Post => Emit_If_Expression'Result /= No_Value_T;
   --  Helper for Emit_Expression: handle if expressions

   procedure Emit_If_Range
     (Env               : Environ;
      Node              : Node_Id;
      Operand_Type      : Entity_Id;
      LHS               : Value_T;
      Low, High         : Uint;
      BB_True, BB_False : Basic_Block_T)
     with Pre => Env /= null and then Present (Node)
                 and then Is_Type (Operand_Type) and then LHS /= No_Value_T
                 and then BB_True /= No_BB_T and then BB_False /= No_BB_T;
   --  Emit code to branch to BB_True or BB_False depending on whether LHS,
   --  which is of type Operand_Type, is in the range from Low to High.  Node
   --  is used only for error messages.

   procedure Emit_Case (Env : Environ; Node : Node_Id)
     with Pre => Env /= null and then Nkind (Node) = N_Case_Statement;
   --  Handle case statements

   function Emit_LCH_Call (Env : Environ; Node : Node_Id) return Value_T
     with Pre  => Env /= null and then Present (Node),
          Post => Emit_LCH_Call'Result /= No_Value_T;
   --  Generate a call to __gnat_last_chance_handler

   function Emit_Literal (Env : Environ; Node : Node_Id) return Value_T
     with Pre  => Env /= null and then Present (Node),
          Post => Emit_Literal'Result /= No_Value_T;

   function Emit_LValue_Internal (Env : Environ; Node : Node_Id) return Value_T
     with Pre => Env /= null and then Present (Node),
          Post => Emit_LValue_Internal'Result /= No_Value_T;
   --  Called by Emit_LValue to walk the tree saving values

   function Emit_LValue_Main (Env : Environ; Node : Node_Id) return Value_T
     with Pre => Env /= null and then Present (Node),
          Post => Emit_LValue_Main'Result /= No_Value_T;
   --  Called by Emit_LValue_Internal to do the work at each level

   function Emit_Min_Max
     (Env         : Environ;
      Exprs       : List_Id;
      Compute_Max : Boolean) return Value_T
     with Pre  => Env /= null and then List_Length (Exprs) = 2
                 and then Is_Scalar_Type (Full_Etype (First (Exprs))),
          Post => Emit_Min_Max'Result /= No_Value_T;
   --  Exprs must be a list of two scalar expressions with compatible types.
   --  Emit code to evaluate both expressions. If Compute_Max, return the
   --  maximum value and return the minimum otherwise.

   procedure Emit_One_Body (Env : Environ; Node : Node_Id)
     with Pre => Env /= null and then Present (Node);
   --  Generate code for one given subprogram body

   function Emit_Array_Aggregate
     (Env           : Environ;
      Node          : Node_Id;
      Dims_Left     : Pos;
      Typ, Comp_Typ : Type_T) return Value_T
     with Pre  => Env /= null and then Nkind (Node) = N_Aggregate
                  and then Typ /= No_Type_T and then Comp_Typ /= No_Type_T,
          Post => Emit_Array_Aggregate'Result /= No_Value_T;
   --  Emit an N_Aggregate of LLVM type Typ, which is an array, returning the
   --  Value_T that contains the data.  Dims_Left says how many dimensions of
   --  the outer array type we still can recurse into.

   function Emit_Shift
     (Env                 : Environ;
      Node                : Node_Id;
      LHS_Node, RHS_Node  : Node_Id) return Value_T
     with Pre  => Env /= null and then Nkind (Node) in N_Op_Shift
                  and then Present (LHS_Node) and then Present (RHS_Node),
          Post => Emit_Shift'Result /= No_Value_T;
   --  Helper for Emit_Expression: handle shift and rotate operations

   function Emit_Subprogram_Decl
     (Env : Environ; Subp_Spec : Node_Id) return Value_T
     with Pre  => Env /= null,
          Post => Emit_Subprogram_Decl'Result /= No_Value_T;
   --  Compile a subprogram declaration, save the corresponding LLVM value to
   --  the environment and return it.

   function Get_Label_BB (Env : Environ; E : Entity_Id) return Basic_Block_T
     with Pre  => Env /= null and then Ekind (E) = E_Label,
          Post => Get_Label_BB'Result /= No_BB_T;
   --  Lazily get the basic block associated with label E, creating it
   --  if we don't have it already.

   procedure Decode_Range (Rng : Node_Id; Low, High : out Uint)
     with Pre => Present (Rng);
   --  Decode the right operand of an N_In or N_Not_In or of a Choice in
   --  a case statement into the low and high bounds.  If either Low or High
   --  is No_Uint, it means that we have a nonstatic value, a non-discrete
   --  value, or we can't find the value.  This should not happen in switch
   --  statements.

   procedure Emit_Subprogram_Body (Env : Environ; Node : Node_Id)
     with Pre => Env /= null and then Present (Node);
   --  Compile a subprogram body and save it in the environment

   function Needs_Deref (Def_Ident : Entity_Id) return Boolean
   is (Present (Address_Clause (Def_Ident))
       and then not Is_Array_Type (Full_Etype (Def_Ident)));
   --  Return whether Def_Ident requires an extra level of indirection

   function Is_Constant_Folded (E : Entity_Id) return Boolean
   is (Ekind (E) = E_Constant
       and then Is_Scalar_Type (Get_Full_View (Full_Etype (E))));

   procedure Verify_Function
     (Env : Environ; Func : Value_T; Node : Node_Id; Msg : String)
     with Pre => Env /= null and then Func /= No_Value_T
                 and then Present (Node);
   --  Verify the validity of the given function, emit an error message if not
   --  and dump the generated byte code.

   function Node_Enclosing_Subprogram (Node : Node_Id) return Node_Id
     with Pre =>  Present (Node);
   --  Return the enclosing subprogram containing Node.

   package Elaboration_Table is new Table.Table
     (Table_Component_Type => Node_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 1024,
      Table_Increment      => 100,
      Table_Name           => "Elaboration_Table");
   --  Table of statements part of the current elaboration procedure

   package Nested_Functions_Table is new Table.Table
     (Table_Component_Type => Node_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "Nested_Function_Table");
   --  Table of nested functions to elaborate

   --  We save pairs of GNAT type and LLVM Value_T for each level of
   --  processing of an Emit_LValue so we can find it if we have a
   --  self-referential item (a discriminated record).

   type LValue_Pair is record T : Entity_Id; Value : Value_T; end record;

   package LValue_Pair_Table is new Table.Table
     (Table_Component_Type => LValue_Pair,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "LValue_Pair_Table");
   --  Table of intermediate results for Emit_LValue

   ---------------------
   -- Verify_Function --
   ---------------------

   procedure Verify_Function
     (Env : Environ; Func : Value_T; Node : Node_Id; Msg : String) is
   begin
      if Verify_Function (Func, Print_Message_Action) then
         Error_Msg_N (Msg, Node);
         Dump_LLVM_Module (Env.Mdl);
      end if;
   end Verify_Function;

   -----------------------
   -- Allocate_For_Type --
   -----------------------

   function Allocate_For_Type
     (Env : Environ; TE : Entity_Id; Name : String) return Value_T
   is
      LLVM_Type   : constant Type_T := Create_Type (Env, TE);
      Element_Typ : Type_T;
      Num_Elts    : Value_T;
   begin

      --  We have three cases.  If the object is not of a dynamic size,
      --  we just do the alloca and that's all.

      if not Is_Dynamic_Size (Env, TE) then
         return Alloca (Env.Bld, LLVM_Type, Name);
      end if;

      --  Otherwise, we have to do some sort of dynamic allocation.  If
      --  this is an array of a component that's not of dynamic size, then
      --  we can allocate an array of the component type corresponding to
      --  the array type and cast it to a pointer to the actual type.
      --  If not, we have to allocate it as an array of bytes.

      if Is_Array_Type (TE)
        and not Is_Dynamic_Size (Env, Component_Type (TE))
      then
         Element_Typ := Create_Type (Env, Component_Type (TE));
         Num_Elts    := Get_Array_Size (Env, No_Value_T, TE);
      else
         Element_Typ := Int_Ty (8);
         Num_Elts    := Get_Type_Size (Env, LLVM_Type, TE, No_Value_T);
      end if;

      return Bit_Cast
        (Env.Bld,
         Array_Alloca (Env.Bld, Element_Typ, Num_Elts, "dyn-array"),
         Pointer_Type (LLVM_Type, 0), Name);

   end Allocate_For_Type;

   -------------------
   -- Emit_One_Body --
   -------------------

   procedure Emit_One_Body (Env : Environ; Node : Node_Id) is
      Spec       : constant Node_Id := Get_Acting_Spec (Node);
      Func       : constant Value_T := Emit_Subprogram_Decl (Env, Spec);
      Def_Ident  : constant Entity_Id := Defining_Entity (Spec);
      Return_Typ : constant Entity_Id := Full_Etype (Def_Ident);
      Param      : Entity_Id;
      LLVM_Param : Value_T;
      LLVM_Var   : Value_T;
      Param_Num  : Natural := 0;

   begin
      Enter_Subp (Env, Func);
      Param := First_Formal_With_Extras (Def_Ident);
      while Present (Param) loop
         LLVM_Param := Get_Param (Func, unsigned (Param_Num));

         --  Define a name for the parameter Param (which is the
         --  Param_Num'th parameter), and associate the corresponding
         --  LLVM value to its entity.

         --  Set the name of the llvm value

         Set_Value_Name (LLVM_Param, Get_Name (Param));

         --  Special case for structures passed by value, we want to
         --  store a pointer to them on the stack, so do an alloca,
         --  to be able to do GEP on them.

         if Param_Needs_Ptr (Param)
           and then not
           (Ekind (Full_Etype (Param)) in Record_Kind
              and (Get_Type_Kind (Type_Of (LLVM_Param)) = Struct_Type_Kind))
         then
            LLVM_Var := LLVM_Param;
         else
            LLVM_Var := Alloca
              (Env.Bld,
               Type_Of (LLVM_Param), Get_Name (Param));
            Store (Env.Bld, LLVM_Param, LLVM_Var);
         end if;

         --  Add the parameter to the environnment

         Set_Value (Env, Param, LLVM_Var);

         if Ekind (Param) = E_In_Parameter
           and then Is_Activation_Record (Param)
         then
            Env.Activation_Rec_Param := LLVM_Param;
         end if;

         Param_Num := Param_Num + 1;
         Param := Next_Formal_With_Extras (Param);
      end loop;

      --  If the return type has dynamic size, we've added a parameter
      --  that's passed the address to which we want to copy our return
      --  value.

      if Ekind (Return_Typ) /= E_Void
        and then Is_Dynamic_Size (Env, Return_Typ)
      then
         LLVM_Param := Get_Param (Func, unsigned (Param_Num));
         Set_Value_Name (LLVM_Param, "return");
         Env.Return_Address_Param := LLVM_Param;
      end if;

      Emit_List (Env, Declarations (Node));
      Emit_List (Env, Statements (Handled_Statement_Sequence (Node)));

      --  This point should not be reached: a return must have
      --  already... returned!

      Discard (Build_Unreachable (Env.Bld));
      Leave_Subp (Env);

      Verify_Function
        (Env, Func, Node,
         "the backend generated bad `LLVM` for this subprogram");
   end Emit_One_Body;

   --------------------------
   -- Emit_Subprogram_Body --
   --------------------------

   procedure Emit_Subprogram_Body (Env : Environ; Node : Node_Id) is
      Nest_Table_First : constant Nat := Nested_Functions_Table.Last + 1;
   begin
      --  If we're not at library level, this a nested function.  Defer it
      --  until we complete elaboration of the enclosing function.  But do
      --  ensure that the spec has been elaborated.
      if not Library_Level (Env) then
         Discard (Emit_Subprogram_Decl (Env, Get_Acting_Spec (Node)));
         Nested_Functions_Table.Append (Node);
         return;
      end if;

      --  Otherwise, elaborate this function and then any nested functions
      --  within in.

      Emit_One_Body (Env, Node);

      for I in Nest_Table_First .. Nested_Functions_Table.Last loop
         Emit_Subprogram_Body (Env, Nested_Functions_Table.Table (I));
      end loop;

      Nested_Functions_Table.Set_Last (Nest_Table_First);
   end Emit_Subprogram_Body;

   ------------------
   -- Decode_Range --
   ------------------

   procedure Decode_Range (Rng : Node_Id; Low, High : out Uint) is
   begin
      case Nkind (Rng) is
         when N_Identifier =>

            --  An N_Identifier can either be a type, in which case we look
            --  at the range of the type, or a constant, in which case we
            --  look at the initializing expression.

            if Is_Type (Entity (Rng)) then
               Decode_Range (Scalar_Range (Full_Etype (Rng)), Low, High);
            else
               Low := Get_Uint_Value (Rng);
               High := Low;
            end if;

         when N_Subtype_Indication =>
            Decode_Range (Range_Expression (Constraint (Rng)), Low, High);

         when N_Range | N_Signed_Integer_Type_Definition =>
            Low := Get_Uint_Value (Low_Bound (Rng));
            High := Get_Uint_Value (High_Bound (Rng));

         when N_Character_Literal | N_Integer_Literal =>
            Low := Get_Uint_Value (Rng);
            High := Low;

         when others =>
            Error_Msg_N ("unknown range operand", Rng);
            Low := No_Uint;
            High := No_Uint;
      end case;
   end Decode_Range;

   ----------
   -- Emit --
   ----------

   procedure Emit (Env : Environ; Node : Node_Id) is
   begin
      if Library_Level (Env)
        and then (Nkind (Node) in N_Statement_Other_Than_Procedure_Call
                   or else Nkind (Node) in N_Subprogram_Call
                   or else Nkind (Node) = N_Handled_Sequence_Of_Statements
                   or else Nkind (Node) in N_Raise_xxx_Error
                   or else Nkind (Node) = N_Raise_Statement)
      then
         --  Append to list of statements to put in the elaboration procedure
         --  if in main unit, otherwise simply ignore the statement.

         if Env.In_Main_Unit then
            Elaboration_Table.Append (Node);
         end if;

         return;
      end if;

      case Nkind (Node) is
         when N_Abstract_Subprogram_Declaration =>
            null;

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

            --  Only generate elaboration procedures for library-level packages
            --  and when part of the main unit.

            if Env.In_Main_Unit
              and then Nkind (Parent (Parent (Node))) = N_Compilation_Unit
            then
               if Elaboration_Table.Last = 0 then
                  Set_Has_No_Elaboration_Code (Parent (Parent (Node)), True);
               else
                  declare
                     Unit      : Node_Id := Defining_Unit_Name (Node);
                     Elab_Type : constant Type_T :=
                       Fn_Ty ((1 .. 0 => <>), Void_Type_In_Context (Env.Ctx));
                     LLVM_Func : Value_T;

                  begin
                     if Nkind (Unit) = N_Defining_Program_Unit_Name then
                        Unit := Defining_Identifier (Unit);
                     end if;

                     LLVM_Func :=
                       Add_Function
                         (Env.Mdl,
                          Get_Name_String (Chars (Unit)) & "___elabs",
                          Elab_Type);
                     Enter_Subp (Env, LLVM_Func);

                     Env.Special_Elaboration_Code := True;

                     for J in 1 .. Elaboration_Table.Last loop
                        Env.Current_Elab_Entity := Elaboration_Table.Table (J);
                        Emit (Env, Elaboration_Table.Table (J));
                     end loop;

                     Elaboration_Table.Set_Last (0);
                     Env.Current_Elab_Entity := Empty;
                     Env.Special_Elaboration_Code := False;
                     Discard (Build_Ret_Void (Env.Bld));
                     Leave_Subp (Env);

                     Verify_Function
                       (Env, LLVM_Func, Node,
                        "the backend generated bad `LLVM` for package " &
                        "spec elaboration");
                  end;
               end if;
            end if;

         when N_Package_Body =>
            declare
               Def_Id : constant Entity_Id := Unique_Defining_Entity (Node);
            begin
               if Ekind (Def_Id) in Generic_Unit_Kind then
                  if Nkind (Parent (Node)) = N_Compilation_Unit then
                     Set_Has_No_Elaboration_Code (Parent (Node), True);
                  end if;
               else
                  Emit_List (Env, Declarations (Node));

                  if not Env.In_Main_Unit then
                     return;
                  end if;

                  --  Handle statements

                  declare
                     Stmts     : constant Node_Id :=
                                   Handled_Statement_Sequence (Node);
                     Has_Stmts : constant Boolean :=
                                   Present (Stmts)
                                     and then Has_Non_Null_Statements
                                                (Statements (Stmts));

                     Elab_Type : Type_T;
                     LLVM_Func : Value_T;
                     Unit      : Node_Id;

                  begin
                     --  For packages inside subprograms, generate elaboration
                     --  code as standard code as part of the enclosing unit.

                     if not Library_Level (Env) then
                        if Has_Stmts then
                           Emit_List (Env, Statements (Stmts));
                        end if;

                     elsif Nkind (Parent (Node)) /= N_Compilation_Unit then
                        if Has_Stmts then
                           Elaboration_Table.Append (Stmts);
                        end if;

                     elsif Elaboration_Table.Last = 0
                       and then not Has_Stmts
                     then
                        Set_Has_No_Elaboration_Code (Parent (Node), True);

                     --  Generate the elaboration code for this library level
                     --  package.

                     else
                        Unit := Defining_Unit_Name (Node);

                        if Nkind (Unit) = N_Defining_Program_Unit_Name then
                           Unit := Defining_Identifier (Unit);
                        end if;

                        Elab_Type := Fn_Ty
                          ((1 .. 0 => <>), Void_Type_In_Context (Env.Ctx));
                        LLVM_Func :=
                          Add_Function
                            (Env.Mdl,
                             Get_Name_String (Chars (Unit)) & "___elabb",
                             Elab_Type);
                        Enter_Subp (Env, LLVM_Func);
                        Env.Special_Elaboration_Code := True;

                        for J in 1 .. Elaboration_Table.Last loop
                           Env.Current_Elab_Entity :=
                             Elaboration_Table.Table (J);
                           Emit (Env, Elaboration_Table.Table (J));
                        end loop;

                        Elaboration_Table.Set_Last (0);
                        Env.Current_Elab_Entity := Empty;
                        Env.Special_Elaboration_Code := False;

                        if Has_Stmts then
                           Emit_List (Env, Statements (Stmts));
                        end if;

                        Discard (Build_Ret_Void (Env.Bld));
                        Leave_Subp (Env);

                        Verify_Function
                          (Env, LLVM_Func, Node,
                           "the backend generated bad `LLVM` for package " &
                           "body elaboration");
                     end if;
                  end;
               end if;
            end;

         when N_Subprogram_Body =>
            --  If we are processing only declarations, do not emit a
            --  subprogram body: just declare this subprogram and add it to
            --  the environment.

            if not Env.In_Main_Unit then
               Discard (Emit_Subprogram_Decl (Env, Get_Acting_Spec (Node)));
               return;

            --  Skip generic subprograms

            elsif Present (Corresponding_Spec (Node))
              and then Ekind (Corresponding_Spec (Node)) in
                         Generic_Subprogram_Kind
            then
               return;
            end if;

            Emit_Subprogram_Body (Env, Node);

         when N_Subprogram_Declaration =>
            declare
               Subp : constant Entity_Id := Unique_Defining_Entity (Node);
            begin
               --  Do not print intrinsic subprogram as calls to those will be
               --  expanded.

               if Convention (Subp) = Convention_Intrinsic
                 or else Is_Intrinsic_Subprogram (Subp)
               then
                  null;
               else
                  Discard (Emit_Subprogram_Decl (Env, Specification (Node)));
               end if;
            end;

         when N_Raise_Statement =>
            Discard (Emit_LCH_Call (Env, Node));

         when N_Raise_xxx_Error =>
            if Present (Condition (Node)) then
               declare
                  BB_Then    : Basic_Block_T;
                  BB_Next    : Basic_Block_T;
               begin
                  BB_Then := Create_Basic_Block (Env, "if-then");
                  BB_Next := Create_Basic_Block (Env, "if-next");
                  Discard (Build_Cond_Br
                    (Env.Bld,
                     Emit_Expression (Env, Condition (Node)),
                     BB_Then, BB_Next));
                  Position_Builder_At_End (Env.Bld, BB_Then);
                  Discard (Emit_LCH_Call (Env, Node));
                  Discard (Build_Br (Env.Bld, BB_Next));
                  Position_Builder_At_End (Env.Bld, BB_Next);
               end;
            else
               Discard (Emit_LCH_Call (Env, Node));
            end if;

         when N_Object_Declaration | N_Exception_Declaration =>
            --  Object declarations are variables either allocated on the stack
            --  (local) or global.

            --  If we are processing only declarations, only declare the
            --  corresponding symbol at the LLVM level and add it to the
            --  environment.

            declare
               Def_Ident      : constant Node_Id := Defining_Identifier (Node);
               T              : Entity_Id := Full_Etype (Def_Ident);
               LLVM_Type      : Type_T;
               LLVM_Var, Expr : Value_T;

            begin
               --  Nothing to do if this is a debug renaming type.

               if T = Standard_Debug_Renaming_Type then
                  return;
               end if;

               while Is_Array_Type (T)
                 and then Present (Packed_Array_Impl_Type (T))
               loop
                  T := Packed_Array_Impl_Type (T);
               end loop;

               --  Handle top-level declarations

               if Library_Level (Env) then
                  --  ??? Will only work for objects of static sizes

                  LLVM_Type := Create_Type (Env, T);

                  --  ??? Should use Needs_Deref instead and handle case of
                  --  global arrays with an address clause as done for local
                  --  variables.

                  if Present (Address_Clause (Def_Ident)) then
                     LLVM_Type := Pointer_Type (LLVM_Type, 0);
                  end if;

                  LLVM_Var :=
                    Add_Global (Env.Mdl, LLVM_Type,
                                Get_Subprog_Ext_Name (Def_Ident));
                  Set_Value (Env, Def_Ident, LLVM_Var);

                  if Env.In_Main_Unit then
                     if Is_Statically_Allocated (Def_Ident) then
                        Set_Linkage (LLVM_Var, Internal_Linkage);
                     end if;

                     if Present (Address_Clause (Def_Ident)) then
                        Set_Initializer
                          (LLVM_Var,
                           Emit_Expression
                             (Env, Expression (Address_Clause (Def_Ident))));
                        --  ??? Should also take Expression (Node) into account

                     else
                        if Is_Imported (Def_Ident) then
                           Set_Linkage (LLVM_Var, External_Linkage);
                        end if;

                        --  Take Expression (Node) into account

                        if Present (Expression (Node))
                          and then not
                            (Nkind (Node) = N_Object_Declaration
                             and then No_Initialization (Node))
                        then
                           if Compile_Time_Known_Value (Expression (Node)) then
                              Expr := Emit_Expression (Env, Expression (Node));
                              Set_Initializer (LLVM_Var, Expr);
                           else
                              Elaboration_Table.Append (Node);

                              if not Is_Imported (Def_Ident) then
                                 Set_Initializer
                                   (LLVM_Var, Const_Null (LLVM_Type));
                              end if;
                           end if;
                        elsif not Is_Imported (Def_Ident) then
                           Set_Initializer (LLVM_Var, Const_Null (LLVM_Type));
                        end if;
                     end if;
                  else
                     Set_Linkage (LLVM_Var, External_Linkage);
                  end if;

               else
                  if Env.Special_Elaboration_Code then
                     LLVM_Var := Get_Value (Env, Def_Ident);

                  elsif Is_Array_Type (T) then

                     --  Alloca arrays are handled as follows:
                     --  * The total size is computed with Array_Size.
                     --  * The type of the innermost component is computed with
                     --    Get_Innermost_Component_Type.
                     --  * The result of the alloca is bitcasted to the proper
                     --    array type, so that multidimensional LLVM GEP
                     --    operations work properly.
                     --  * If an address clause is specified, then simply
                     --    cast the address into an array.

                     LLVM_Type := Create_Access_Type (Env, T);

                     if Present (Address_Clause (Def_Ident)) then
                        LLVM_Var := Int_To_Ptr
                           (Env.Bld,
                            Emit_Expression
                              (Env,
                               Expression (Address_Clause (Def_Ident))),
                           LLVM_Type,
                           Get_Name (Def_Ident));
                     else
                        LLVM_Var := Bit_Cast
                           (Env.Bld,
                            Array_Alloca
                              (Env.Bld,
                               Int_Ty (8),
                               Get_Type_Size (Env, Create_Type (Env, T), T,
                                              No_Value_T),
                               "array-alloca"),
                           LLVM_Type,
                           Get_Name (Def_Ident));
                     end if;

                     Set_Value (Env, Def_Ident, LLVM_Var);

                  elsif Record_With_Dynamic_Size (Env, T) then
                     LLVM_Type := Create_Access_Type (Env, T);
                     LLVM_Var := Bit_Cast
                       (Env.Bld,
                        Array_Alloca
                          (Env.Bld,
                           Int_Ty (8),
                           Get_Type_Size (Env, Create_Type (Env, T),
                                          T, No_Value_T, True),
                           "record-alloca"),
                        LLVM_Type,
                        Get_Name (Def_Ident));
                     Set_Value (Env, Def_Ident, LLVM_Var);

                  else
                     LLVM_Type := Create_Type (Env, T);

                     if Present (Address_Clause (Def_Ident)) then
                        LLVM_Type := Pointer_Type (LLVM_Type, 0);
                     end if;

                     LLVM_Var := Alloca
                       (Env.Bld, LLVM_Type, Get_Name (Def_Ident));
                     Set_Value (Env, Def_Ident, LLVM_Var);
                  end if;

                  if Needs_Deref (Def_Ident) then
                     Expr := Emit_Expression
                       (Env, Expression (Address_Clause (Def_Ident)));
                     Expr := Int_To_Ptr (Env.Bld, Expr, LLVM_Type, "to-ptr");
                     Store (Env.Bld, Expr, LLVM_Var);
                  end if;

                  if Present (Expression (Node))
                    and then not
                      (Nkind (Node) = N_Object_Declaration
                       and then No_Initialization (Node))
                  then
                     Emit_Assignment (Env, T, Full_Etype (Expression (Node)),
                                      LLVM_Var, Expression (Node),
                                      No_Value_T, True, True);
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
               if Library_Level (Env) then
                  if Is_LValue (Name (Node)) then
                     LLVM_Var := Emit_LValue (Env, Name (Node));
                     Set_Value (Env, Def_Ident, LLVM_Var);
                  else
                     --  ??? Handle top-level declarations
                     Error_Msg_N
                       ("library level object renaming not supported", Node);
                  end if;

                  return;
               end if;

               --  If the renamed object is already an l-value, keep it as-is.
               --  Otherwise, create one for it.

               if Is_LValue (Name (Node)) then
                  LLVM_Var := Emit_LValue (Env, Name (Node));
               else
                  LLVM_Var := Alloca
                    (Env.Bld,
                     Create_Type (Env, Full_Etype (Def_Ident)),
                     Get_Name (Def_Ident));
                  Store
                    (Env.Bld, Emit_Expression (Env, Name (Node)), LLVM_Var);
               end if;

               Set_Value (Env, Def_Ident, LLVM_Var);
            end;

         when N_Subprogram_Renaming_Declaration =>
            --  Nothing is needed except for debugging information.
            --  Skip it for now???
            --  Note that in any case, we should skip Intrinsic subprograms

            null;

         when N_Implicit_Label_Declaration =>
            --  Don't do anything here in case this label isn't actually
            --  used as a label.  In that case, the basic block we create
            --  here will be empty, which LLVM doesn't allow.  This can't
            --  occur for user-defined labels, but can occur with some
            --  labels placed by the front end.  Instead, lazily create
            --  the basic block where it's placed or when its the target
            --  of a goto.
            null;

         when N_Assignment_Statement =>
            Emit_Assignment (Env,
                             Full_Etype (Name (Node)),
                             Full_Etype (Expression (Node)),
                             Emit_LValue (Env, Name (Node)),
                             Expression (Node), No_Value_T,
                             Forwards_OK (Node), Backwards_OK (Node));

         when N_Procedure_Call_Statement =>
            Discard (Emit_Call (Env, Node));

         when N_Null_Statement =>
            null;

         when N_Label =>
            declare
               BB : constant Basic_Block_T :=
                 Get_Label_BB (Env, Entity (Identifier (Node)));
            begin
               Discard (Build_Br (Env.Bld, BB));
               Position_Builder_At_End (Env.Bld, BB);
            end;

         when N_Goto_Statement =>
            Discard (Build_Br (Env.Bld,
                               Get_Label_BB (Env, Entity (Name (Node)))));
            Position_Builder_At_End
              (Env.Bld, Create_Basic_Block (Env, "after-goto"));

         when N_Exit_Statement =>
            declare
               Exit_Point : constant Basic_Block_T :=
                 (if Present (Name (Node))
                  then Get_Exit_Point (Entity (Name (Node)))
                  else Get_Exit_Point);
               Next_BB    : constant Basic_Block_T :=
                 Create_Basic_Block (Env, "loop-after-exit");

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

               declare
                  Subp        : constant Node_Id :=
                    Node_Enclosing_Subprogram (Node);
                  Our_Typ     : constant Entity_Id := Full_Etype (Subp);
                  Return_Expr : constant Node_Id := Expression (Node);
                  Expr_Typ    : constant Entity_Id := Full_Etype (Return_Expr);
                  Expr : Value_T;
               begin
                  --  If we have a parameter giving the address to which to
                  --  copy the return value, do that copy instead of returning
                  --  it.

                  if Env.Return_Address_Param /= No_Value_T then
                     Emit_Assignment (Env, Our_Typ, Expr_Typ,
                                      Env.Return_Address_Param,
                                      Return_Expr, No_Value_T, False, False);

                     Discard (Build_Ret_Void (Env.Bld));

                  else

                     if Our_Typ /= Expr_Typ then
                        Expr := Build_Type_Conversion
                          (Env       => Env,
                           Src_Type  => Expr_Typ,
                           Dest_Type => Our_Typ,
                           Expr      => Return_Expr);

                     else
                        Expr := Emit_Expression (Env, Return_Expr);
                     end if;

                     Discard (Build_Ret (Env.Bld, Expr));
                  end if;
               end;

            else
               Discard (Build_Ret_Void (Env.Bld));
            end if;

            Position_Builder_At_End
              (Env.Bld, Create_Basic_Block (Env, "unreachable"));

         when N_If_Statement =>
            Emit_If (Env, Node);

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
                    and then Has_BB (Env, Entity (Identifier (Node)))
                  then Get_Basic_Block (Env, Entity (Identifier (Node)))
                  else Create_Basic_Block (Env, ""));
               Discard (Build_Br (Env.Bld, BB_Init));
               Position_Builder_At_End (Env.Bld, BB_Init);

               --  If this is not a FOR loop, there is no initialization: alias
               --  it with the COND block.
               BB_Cond :=
                 (if not Is_For_Loop
                  then BB_Init
                  else Create_Basic_Block (Env, "loop-cond"));

               --  If this is a mere loop, there is even no condition block:
               --  alias it with the STMTS block.
               BB_Stmts :=
                 (if Is_Mere_Loop
                  then BB_Cond
                  else Create_Basic_Block (Env, "loop-stmts"));

               --  If this is not a FOR loop, there is no iteration: alias it
               --  with the COND block, so that at the end of every STMTS, jump
               --  on ITER or COND.
               BB_Iter :=
                 (if Is_For_Loop then Create_Basic_Block (Env, "loop-iter")
                  else BB_Cond);

               --  The NEXT step contains no statement that comes from the
               --  loop: it is the exit point.
               BB_Next := Create_Basic_Block (Env, "loop-exit");

               --  The front-end expansion can produce identifier-less loops,
               --  but exit statements can target them anyway, so register such
               --  loops.

               Push_Loop (Loop_Identifier, BB_Next);

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
                          Is_Unsigned_Type (Full_Etype (Def_Ident));
                        LLVM_Type       : Type_T;
                        LLVM_Var        : Value_T;
                        Low, High       : Value_T;

                     begin
                        --  Initialization block: create the loop variable and
                        --  initialize it.
                        Create_Discrete_Type
                          (Env, Full_Etype (Def_Ident), LLVM_Type, Low, High);
                        LLVM_Var := Alloca
                          (Env.Bld, LLVM_Type, Get_Name (Def_Ident));
                        Set_Value (Env, Def_Ident, LLVM_Var);
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

                        BB_Cond := Create_Basic_Block (Env, "loop-cond-iter");
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
                             Const_Int (LLVM_Type, 1, False);
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
               Pop_Loop;

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

               if Has_BB (Env, BE) then
                  BB := Get_Basic_Block (Env, BE);
               else
                  BB := Create_Basic_Block (Env, "");

                  if Present (BE) then
                     Set_Basic_Block (Env, BE, BB);
                  end if;
               end if;

               Discard (Build_Br (Env.Bld, BB));
               Position_Builder_At_End (Env.Bld, BB);

               Stack_State := Call
                 (Env.Bld,
                  Env.Stack_Save_Fn, System.Null_Address, 0, "");

               Emit_List (Env, Declarations (Node));
               Emit_List
                 (Env, Statements (Handled_Statement_Sequence (Node)));

               Discard
                 (Call
                    (Env.Bld,
                     Env.Stack_Restore_Fn, Stack_State'Address, 1, ""));
            end;

         when N_Full_Type_Declaration | N_Subtype_Declaration
            | N_Incomplete_Type_Declaration | N_Private_Type_Declaration
            | N_Private_Extension_Declaration
         =>
            Discard
              (GNAT_To_LLVM_Type (Env, Defining_Identifier (Node), True));

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

         when N_Case_Statement =>
            Emit_Case (Env, Node);

         when N_Body_Stub =>
            if Nkind_In (Node, N_Protected_Body_Stub, N_Task_Body_Stub) then
               raise Program_Error;
            end if;

            --  No action if the separate unit is not available

            if No (Library_Unit (Node)) then
               Error_Msg_N ("separate unit not available", Node);
            else
               Emit (Env, Get_Body_From_Stub (Node));
            end if;

         --  Nodes we actually want to ignore
         when N_Call_Marker
            | N_Empty
            | N_Enumeration_Representation_Clause
            | N_Enumeration_Type_Definition
            | N_Function_Instantiation
            | N_Freeze_Generic_Entity
            | N_Itype_Reference
            | N_Number_Declaration
            | N_Procedure_Instantiation
            | N_Validate_Unchecked_Conversion
            | N_Variable_Reference_Marker =>
            null;

         when N_Package_Instantiation
            | N_Package_Renaming_Declaration
            | N_Generic_Package_Declaration
            | N_Generic_Subprogram_Declaration
         =>
            if Nkind (Parent (Node)) = N_Compilation_Unit then
               Set_Has_No_Elaboration_Code (Parent (Node), True);
            end if;

         --  ??? Ignore for now
         when N_Push_Constraint_Error_Label .. N_Pop_Storage_Error_Label =>
            null;

         --  ??? Ignore for now
         when N_Exception_Handler =>
            Error_Msg_N ("exception handler ignored??", Node);

         when N_Exception_Renaming_Declaration =>
            Set_Value
              (Env, Defining_Identifier (Node),
               (Get_Value (Env, Entity (Name (Node)))));

         when N_Attribute_Definition_Clause =>

            --  The only interesting case left after expansion is for Address
            --  clauses. We only deal with 'Address if the object has a Freeze
            --  node.

            --  ??? For now keep it simple and deal with this case in
            --  N_Object_Declaration.

            if Get_Attribute_Id (Chars (Node)) = Attribute_Address
              and then Present (Freeze_Node (Entity (Name (Node))))
            then
               null;
            end if;

         when others =>
            Error_Msg_N
              ("unhandled statement kind: `" &
               Node_Kind'Image (Nkind (Node)) & "`", Node);
      end case;
   end Emit;

   ---------------------
   -- Get_Static_Link --
   ---------------------

   function Get_Static_Link (Env : Environ; Node : Entity_Id) return Value_T is
      Subp        : constant Entity_Id := Entity (Node);
      Result_Type : constant Type_T :=
        Pointer_Type (Int8_Type_In_Context (Env.Ctx), 0);
      Result      : Value_T;

      Parent : constant Entity_Id := Enclosing_Subprogram (Subp);
      Caller : Node_Id;

   begin
      if Present (Parent) then
         Caller := Node_Enclosing_Subprogram (Node);

         declare
            Ent : constant Subp_Entry := Subps.Table (Subp_Index (Parent));
            Ent_Caller : constant Subp_Entry :=
              Subps.Table (Subp_Index (Caller));

         begin
            if Parent = Caller then
               Result := Get_Value (Env, Ent.ARECnP);
            else
               Result := Get_Value (Env, Ent_Caller.ARECnF);

               --  Go levels up via the ARECnU field if needed

               for J in 1 .. Ent_Caller.Lev - Ent.Lev - 1 loop
                  Result :=
                    Struct_GEP
                    (Env.Bld,
                     Load (Env.Bld, Result, ""),
                     0,
                     "ARECnF.all.ARECnU");
               end loop;
            end if;

            return Bit_Cast
              (Env.Bld,
               Load (Env.Bld, Result, ""),
               Result_Type,
               "static-link");
         end;
      else
         return Const_Null (Result_Type);
      end if;
   end Get_Static_Link;

   -----------------
   -- Emit_LValue --
   -----------------

   function Emit_LValue
     (Env : Environ; Node : Node_Id) return Value_T
   is
   begin
      LValue_Pair_Table.Set_Last (0);
      --  Each time we start a new recursive call, we free the entries
      --  from the last one.

      return Emit_LValue_Internal (Env, Node);
   end Emit_LValue;

   --------------------------
   -- Emit_LValue_Internal --
   --------------------------

   function Emit_LValue_Internal
     (Env : Environ; Node : Node_Id) return Value_T
   is
      Typ      : constant Entity_Id := Full_Etype (Node);
      Value    : Value_T := Emit_LValue_Main (Env, Node);
   begin

      --  Unless Typ is an unconstrained array type, convert the LValue
      --  we have so far into a pointer to that type (unless it already is).
      --  We also don't do this if the value is a subprogram since we may
      --  we may have made a complex object for it.  We have to be careful
      --  of the order in which we test since Typ may be E_Void

      if (not Is_Array_Type (Typ) or else Is_Constrained (Typ))
        and then not (Is_Entity_Name (Node)
                        and then Ekind (Entity (Node)) in Subprogram_Kind)
        and then Type_Of (Value) /= Create_Access_Type (Env, Typ)
      then
         Value := Bit_Cast (Env.Bld, Value, Create_Access_Type (Env, Typ), "");
      end if;

      --  If the object is not of void type, save the result in the
      --  pair table under the base type of the fullest view.

      if Ekind (Typ) /= E_Void then
         LValue_Pair_Table.Append
           ((Implementation_Base_Type (Get_Fullest_View (Typ)), Value));
      end if;

      return Value;
   end Emit_LValue_Internal;

   ----------------------
   -- Emit_LValue_Main --
   ----------------------

   function Emit_LValue_Main (Env : Environ; Node : Node_Id) return Value_T is
   begin
      case Nkind (Node) is
         when N_Identifier | N_Expanded_Name =>
            declare
               Def_Ident : constant Entity_Id := Entity (Node);
               N         : Node_Id;
            begin
               if Ekind (Def_Ident) in Subprogram_Kind then
                  N := Associated_Node_For_Itype (Full_Etype (Parent (Node)));

                  if No (N) or else Nkind (N) = N_Full_Type_Declaration then
                     return Get_Value (Env, Def_Ident);
                  else
                     --  Return a callback, which is a pair: subprogram
                     --  code pointer and static link argument.

                     declare
                        Func   : constant Value_T :=
                          Get_Value (Env, Def_Ident);
                        S_Link : constant Value_T :=
                          Get_Static_Link (Env, Node);

                        Fields_Types  : constant array (1 .. 2) of Type_T :=
                          (Type_Of (S_Link),
                           Type_Of (S_Link));
                        Callback_Type : constant Type_T :=
                          Struct_Type_In_Context
                          (Env.Ctx,
                           Fields_Types'Address, Fields_Types'Length,
                           Packed => False);

                        Result : Value_T := Get_Undef (Callback_Type);

                     begin
                        Result := Insert_Value
                          (Env.Bld, Result,
                           Pointer_Cast
                             (Env.Bld, Func, Fields_Types (1), ""), 0, "");
                        Result := Insert_Value
                          (Env.Bld, Result, S_Link, 1, "callback");
                        return Result;
                     end;
                  end if;

               elsif Needs_Deref (Def_Ident) then
                  return Load (Env.Bld, Get_Value (Env, Def_Ident), "");
               else
                  return Get_Value (Env, Def_Ident);
               end if;
            end;

         when N_Defining_Identifier =>
            if Needs_Deref (Node) then
               return Load (Env.Bld, Get_Value (Env, Node), "");
            else
               return Get_Value (Env, Node);
            end if;

         when N_Attribute_Reference =>
            return Emit_Attribute_Reference (Env, Node, LValue => True);

         when N_Explicit_Dereference =>
            return Emit_Expression (Env, Prefix (Node));

         when N_Aggregate =>
            declare
               --  The frontend can sometimes take a reference to an aggregate.
               --  In such cases, we have to create an anonymous object and use
               --  its value as the aggregate value.

               --  ??? This alloca will not necessarily be free'd before
               --  returning from the current subprogram: it's a leak.

               V : constant Value_T :=
                 Allocate_For_Type (Env, Full_Etype (Node), "anon-obj");

            begin
               Store (Env.Bld, Emit_Expression (Env, Node), V);
               return V;
            end;

         when N_String_Literal =>
            declare
               T : constant Type_T := Create_Type (Env, Full_Etype (Node));
               V : constant Value_T := Add_Global (Env.Mdl, T, "str-lit");

            begin
               Set_Value (Env, Node, V);
               Set_Initializer (V, Emit_Expression (Env, Node));
               Set_Linkage (V, Private_Linkage);
               Set_Global_Constant (V, True);
               return GEP
                 (Env.Bld,
                  V,
                  (Const_Int (Intptr_T, 0, Sign_Extend => False),
                   Const_Int (Create_Type (Env, Standard_Positive),
                              0, Sign_Extend => False)),
                  "str-addr");
            end;

         when N_Selected_Component =>
            declare
               Pfx_Ptr : constant Value_T :=
                 Emit_LValue_Internal (Env, Prefix (Node));
               Record_Component : constant Entity_Id :=
                 Original_Record_Component (Entity (Selector_Name (Node)));

            begin
               return Record_Field_Offset (Env, Pfx_Ptr, Record_Component);
            end;

         when N_Indexed_Component =>
            return Get_Indexed_LValue
              (Env, Full_Etype (Prefix (Node)), Expressions (Node),
               Emit_LValue_Internal (Env, Prefix (Node)));

         when N_Slice =>
            return Get_Slice_LValue
              (Env, Full_Etype (Prefix (Node)), Full_Etype (Node),
               Discrete_Range (Node),
               Emit_LValue_Internal (Env, Prefix (Node)));

         when N_Unchecked_Type_Conversion | N_Type_Conversion =>

            --  ??? Strip the type conversion, likely not always correct
            return Emit_LValue_Internal (Env, Expression (Node));

         when others =>
            if not Library_Level (Env) then
               --  Otherwise, create a temporary: is that always
               --  adequate???

               declare
                  Result : constant Value_T :=
                    Allocate_For_Type (Env, Full_Etype (Node), "");
               begin
                  Emit_Assignment (Env, Full_Etype (Node), Full_Etype (Node),
                                   Result, Node,
                                   Emit_Expression (Env, Node), True, True);
                  return Result;
               end;
            else
               Error_Msg_N
                 ("unhandled node kind: `" &
                  Node_Kind'Image (Nkind (Node)) & "`", Node);
               return Get_Undef (Create_Type (Env, Full_Etype (Node)));
            end if;
      end case;
   end Emit_LValue_Main;

   ------------------------
   -- Get_Matching_Value --
   ------------------------

   function Get_Matching_Value (T : Entity_Id) return Value_T is
   begin
      for I in 1 .. LValue_Pair_Table.Last loop
         if Implementation_Base_Type (T) = LValue_Pair_Table.Table (I).T then
            return LValue_Pair_Table.Table (I).Value;
         end if;
      end loop;

      --  Should never get here and postcondition verifies.

      return No_Value_T;
   end Get_Matching_Value;

   ----------------------------
   -- Build_Short_Circuit_Op --
   ----------------------------

   function Build_Short_Circuit_Op
     (Env                   : Environ;
      Node_Left, Node_Right : Node_Id;
      Orig_Left, Orig_Right : Value_T;
      Op                    : Node_Kind) return Value_T
   is
      Left  : Value_T := Orig_Left;
      Right : Value_T := Orig_Right;

      --  We start evaluating the LHS in the current block, but we need to
      --  record which block it completes in, since it may not be the
      --  same block.

      Block_Left_Expr_End : Basic_Block_T;

      --  Block which contains the evaluation of the right part
      --  expression of the operator and its end.

      Block_Right_Expr : constant Basic_Block_T :=
        Create_Basic_Block (Env, "scl-right-expr");
      Block_Right_Expr_End : Basic_Block_T;

      --  Block containing the exit code (the phi that selects that value)

      Block_Exit : constant Basic_Block_T :=
        Create_Basic_Block (Env, "scl-exit");

   begin
      --  In the case of And, evaluate the right expression when Left is
      --  true. In the case of Or, evaluate it when Left is false.
      if Left = No_Value_T then
         Left := Emit_Expression (Env, Node_Left);
      end if;

      Block_Left_Expr_End := Get_Insert_Block (Env.Bld);

      if Op = N_And_Then then
         Discard (Build_Cond_Br (Env.Bld, Left, Block_Right_Expr, Block_Exit));
      else
         Discard (Build_Cond_Br (Env.Bld, Left, Block_Exit, Block_Right_Expr));
      end if;

      --  Emit code for the evaluation of the right part expression

      Position_Builder_At_End (Env.Bld, Block_Right_Expr);
      if Right = No_Value_T then
         Right := Emit_Expression (Env, Node_Right);
      end if;

      Block_Right_Expr_End := Get_Insert_Block (Env.Bld);
      Discard (Build_Br (Env.Bld, Block_Exit));

      Position_Builder_At_End (Env.Bld, Block_Exit);

      --  If we exited the entry block, it means that for AND, the result
      --  is false and for OR, it's true.  Otherwise, the result is the right.

      declare
         LHS_Const : constant unsigned_long_long :=
           (if Op = N_And_Then then 0 else 1);
         Values    : constant Value_Array (1 .. 2) :=
             (Const_Int (Int_Ty (1), LHS_Const, False), Right);
         BBs       : constant Basic_Block_Array (1 .. 2) :=
             (Block_Left_Expr_End, Block_Right_Expr_End);
         Phi       : constant Value_T :=
             LLVM.Core.Phi (Env.Bld, Int_Ty (1), "");
      begin
         Add_Incoming (Phi, Values'Address, BBs'Address, 2);
         return Phi;
      end;
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
      if Nkind (Node) in N_Binary_Op then

         --  Handle comparisons and shifts with helper functions, then
         --  the rest are by generating the appropriate LLVM IR entry.

         if Nkind (Node) in N_Op_Compare then

            return Emit_Comparison
              (Env, Nkind (Node), Full_Etype (Left_Opnd (Node)),
               Left_Opnd (Node), Right_Opnd (Node));

         elsif Nkind (Node) in N_Op_Shift then
            return Emit_Shift (Env, Node, Left_Opnd (Node), Right_Opnd (Node));
         end if;

         declare
            type Opf is access function
              (Bld : Builder_T; LHS, RHS : Value_T; Name : String)
              return Value_T;

            Left_Type : constant Entity_Id := Full_Etype (Left_Opnd (Node));
            Right_Type : constant Entity_Id := Full_Etype (Right_Opnd (Node));
            Left_BT : constant Entity_Id :=
              Implementation_Base_Type (Left_Type);
            Right_BT : constant Entity_Id :=
              Implementation_Base_Type (Right_Type);
            LVal   : constant Value_T :=
              Build_Type_Conversion
              (Env, Left_Type, Left_BT, Left_Opnd (Node));
            RVal   : constant Value_T :=
              Build_Type_Conversion
              (Env, Right_Type, Right_BT, Right_Opnd (Node));
            FP     : constant Boolean := Is_Floating_Point_Type (Left_BT);
            Unsign : constant Boolean := Is_Unsigned_Type (Right_BT);
            Subp   : Opf := null;

         begin
            case Nkind (Node) is
               when N_Op_Add =>
                  Subp := (if FP then F_Add'Access else NSW_Add'Access);

               when N_Op_Subtract =>
                  Subp := (if FP then F_Sub'Access else NSW_Sub'Access);

               when N_Op_Multiply =>
                  Subp := (if FP then F_Mul'Access else NSW_Mul'Access);

               when N_Op_Divide =>
                  Subp :=
                    (if FP then F_Div'Access
                     elsif Unsign then U_Div'Access else S_Div'Access);

               when N_Op_Rem =>
                  Subp := (if Unsign then U_Rem'Access else S_Rem'Access);

               when N_Op_And =>
                  Subp := Build_And'Access;

               when N_Op_Or =>
                  Subp := Build_Or'Access;

               when N_Op_Xor =>
                  Subp := Build_Xor'Access;

               when N_Op_Mod =>
                  Subp := U_Rem'Access;

               when others =>
                  null;

            end case;

            if Subp /= null then
               return Subp (Env.Bld, LVal, RVal, "");
            else
               Error_Msg_N
                 ("unhandled node kind in expression: `" &
                    Node_Kind'Image (Nkind (Node)) & "`", Node);
               return Get_Undef (Create_Type (Env, Left_BT));
            end if;
         end;

      else
         case Nkind (Node) is
         when N_Expression_With_Actions =>
            Emit_List (Env, Actions (Node));
            return Emit_Expr (Expression (Node));

         when  N_Character_Literal | N_Numeric_Or_String_Literal =>
            return Emit_Literal (Env, Node);

         when N_And_Then | N_Or_Else =>
            return Build_Short_Circuit_Op
              (Env, Left_Opnd (Node), Right_Opnd (Node),
               No_Value_T, No_Value_T, Nkind (Node));

         when N_Op_Not =>
            return Build_Not (Env.Bld, Emit_Expr (Right_Opnd (Node)), "");

         when N_Op_Abs =>

            --  Emit: X >= 0 ? X : -X;

            declare
               Expr_Type : constant Entity_Id
                 := Full_Etype (Right_Opnd (Node));
               Expr      : constant Value_T := Emit_Expr (Right_Opnd (Node));
               Zero      : constant Value_T := Const_Null
                 (Create_Type (Env, Expr_Type));

            begin
               if Is_Floating_Point_Type (Expr_Type) then
                  return Build_Select
                    (Env.Bld,
                     C_If   => F_Cmp
                       (Env.Bld, Real_OGE, Expr, Zero, ""),
                     C_Then => Expr,
                     C_Else => F_Neg (Env.Bld, Expr, ""),
                     Name   => "abs");
               elsif Is_Unsigned_Type (Expr_Type) then
                  return Expr;
               else
                  return Build_Select
                    (Env.Bld,
                     C_If   => I_Cmp (Env.Bld, Int_SGE, Expr, Zero, ""),
                     C_Then => Expr,
                     C_Else => NSW_Neg (Env.Bld, Expr, ""),
                     Name   => "abs");
               end if;
            end;

         when N_Op_Plus =>
            return Emit_Expr (Right_Opnd (Node));

         when N_Op_Minus =>
            if Is_Floating_Point_Type (Full_Etype (Node)) then
               return F_Neg (Env.Bld, Emit_Expr (Right_Opnd (Node)), "");
            else
               return NSW_Neg (Env.Bld, Emit_Expr (Right_Opnd (Node)), "");
            end if;

         when N_Unchecked_Type_Conversion =>
            return Build_Unchecked_Conversion
              (Env    => Env,
               S_Type => Full_Etype (Expression (Node)),
               D_Type => Full_Etype (Node),
               Expr   => Expression (Node));

         when N_Qualified_Expression =>
            --  We can simply strip the type qualifier
            --  ??? Need to take Do_Overflow_Check into account

            return Emit_Expr (Expression (Node));

         when N_Type_Conversion =>
            --  ??? Need to take Do_Overflow_Check into account
            return Build_Type_Conversion
              (Env       => Env,
               Src_Type  => Full_Etype (Expression (Node)),
               Dest_Type => Full_Etype (Node),
               Expr      => Expression (Node));

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
                    (Create_Type (Env, Full_Etype (Node)),
                     Enumeration_Rep (Def_Ident));

               --  See if this is an entity that's present in our
               --  activation record.

               elsif Ekind_In (Def_Ident, E_Constant,
                               E_Discriminant,
                               E_In_Parameter,
                               E_In_Out_Parameter,
                               E_Loop_Parameter,
                               E_Out_Parameter,
                               E_Variable)
                 and then Present (Activation_Record_Component (Def_Ident))
                 and then Env.Activation_Rec_Param /= No_Value_T
                 and then Get_Value (Env, Scope (Def_Ident)) /= Env.Func
               then
                  declare
                     Component         : constant Entity_Id :=
                       Activation_Record_Component (Def_Ident);
                     Activation_Record : constant Value_T :=
                       Env.Activation_Rec_Param;
                     Pointer           : constant Value_T :=
                       Record_Field_Offset (Env, Activation_Record, Component);
                     Value_Address     : constant Value_T :=
                       Load (Env.Bld, Pointer, "");
                     Typ               : constant Type_T :=
                       Pointer_Type (Create_Type
                                       (Env, Full_Etype (Def_Ident)), 0);
                     Value_Ptr         : constant Value_T :=
                       Int_To_Ptr (Env.Bld, Value_Address, Typ, "");
                  begin
                     return Load (Env.Bld, Value_Ptr, "");
                  end;

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
                          (Create_Type (Env, Full_Etype (Node)),
                           Enumeration_Rep (Entity (Expr)));
                     else
                        return Emit_Expression (Env, N);
                     end if;
                  end;

               elsif Nkind (Node) in N_Subexpr
                 and then Is_Constant_Folded (Entity (Node))
               then
                  --  Replace constant references by the direct values, to
                  --  avoid a level of indirection for e.g. private values and
                  --  to allow generation of static values and static
                  --  aggregates.

                  declare
                     N    : constant Node_Id := Get_Full_View (Entity (Node));
                     Decl : constant Node_Id := Declaration_Node (N);
                     Expr : Node_Id := Empty;

                  begin
                     if Nkind (Decl) /= N_Object_Renaming_Declaration then
                        Expr := Expression (Decl);
                     end if;

                     if Present (Expr) then
                        if Nkind_In (Expr, N_Character_Literal,
                                           N_Expanded_Name,
                                           N_Integer_Literal,
                                           N_Real_Literal)
                          or else (Nkind (Expr) = N_Identifier
                                   and then Ekind (Entity (Expr)) =
                                     E_Enumeration_Literal)
                        then
                           return Emit_Expression (Env, Expr);
                        end if;
                     end if;
                  end;
               end if;

               declare
                  Kind          : constant Entity_Kind := Ekind (Def_Ident);
                  Type_Kind     : constant Entity_Kind :=
                    Ekind (Full_Etype (Def_Ident));
                  Is_Subprogram : constant Boolean :=
                    (Kind in Subprogram_Kind
                     or else Type_Kind = E_Subprogram_Type);
                  LValue        : constant Value_T :=
                    Get_Value (Env, Def_Ident);

               begin
                  --  LLVM functions are pointers that cannot be
                  --  dereferenced. If Def_Ident is a subprogram, return it
                  --  as-is, the caller expects a pointer to a function
                  --  anyway.  For dynamic-sized types, we always return
                  --  the address of the object, so leave it the way it is.

                  if Is_Subprogram
                    or else Is_Dynamic_Size (Env, Full_Etype (Def_Ident))
                  then
                     return LValue;
                  elsif Needs_Deref (Def_Ident) then
                     return Load (Env.Bld, Load (Env.Bld, LValue, ""), "");
                  else
                     return Load (Env.Bld, LValue, "");
                  end if;
               end;
            end;

         when N_Defining_Operator_Symbol =>
            return Get_Value (Env, Node);

         when N_Function_Call =>
            return Emit_Call (Env, Node);

         when N_Explicit_Dereference =>
            --  Access to subprograms require special handling, see
            --  N_Identifier.

            declare
               Access_Value : constant Value_T := Emit_Expr (Prefix (Node));
            begin
               return
                 (if Ekind (Full_Etype (Node)) = E_Subprogram_Type
                  then Access_Value
                  else Load_With_Type (Env, Full_Etype (Node), Access_Value));
            end;

         when N_Allocator =>
            if Present (Storage_Pool (Node)) then
               Error_Msg_N ("unsupported form of N_Allocator", Node);
               return Get_Undef (Create_Type (Env, Full_Etype (Node)));
            end if;

            declare
               Expr             : constant Node_Id := Expression (Node);
               Typ              : Entity_Id;
               Arg              : array (1 .. 1) of Value_T;
               Value            : Value_T;
               Result_Type      : constant Entity_Id := Full_Etype (Node);
               LLVM_Result_Type : Type_T;
               Result           : Value_T;

            begin
               --  There are two cases: the Expression operand can either be
               --  an N_Identifier or Expanded_Name, which must represent a
               --  type, or a N_Qualified_Expression, which contains both
               --  the object type and an initial value for the object.

               if Is_Entity_Name (Expr) then
                  Typ         := Entity (Expr);
                  Value       := No_Value_T;
               else
                  pragma Assert (Nkind (Expr) = N_Qualified_Expression);
                  Typ         := Full_Etype (Expression (Expr));
                  Value       := Emit_Expr (Expression (Expr));
               end if;

               Arg := (1 => Get_Type_Size (Env, Create_Type (Env, Typ),
                                           Typ, Value,
                                           For_Type => Value = No_Value_T));
               Result := Call
                 (Env.Bld, Env.Default_Alloc_Fn, Arg'Address, 1, "alloc");
               LLVM_Result_Type := Create_Type (Env, Result_Type);

               if Nkind (Expr) = N_Qualified_Expression then
                  Emit_Assignment (Env, Typ, Typ, Result, Empty, Value,
                                   True, True);
               end if;

               --  ??? This should be common code at some point.
               --  If we need a fat pointer, make one.  Otherwise, just do
               --  a bitwise conversion.

               if Is_Array_Type (Designated_Type (Result_Type))
                 and then not Is_Constrained (Designated_Type (Result_Type))
               then
                  Result := Array_Fat_Pointer (Env, Result, Typ);
               else
                  Result := Bit_Cast (Env.Bld, Result, LLVM_Result_Type, "");
               end if;

               return Result;
            end;

         when N_Reference =>
            return Emit_LValue (Env, Prefix (Node));

         when N_Attribute_Reference =>
            return Emit_Attribute_Reference (Env, Node, LValue => False);

         when N_Selected_Component | N_Indexed_Component  | N_Slice =>
            return Load_With_Type (Env, Full_Etype (Node),
                                   Emit_LValue (Env, Node));

         when N_Aggregate =>
            if Null_Record_Present (Node) then
               return Const_Null (Create_Type (Env, Full_Etype (Node)));
            end if;

            declare
               Agg_Type   : constant Entity_Id := Full_Etype (Node);
               LLVM_Type  : constant Type_T := Create_Type (Env, Agg_Type);
               Result     : Value_T := Get_Undef (LLVM_Type);
               Cur_Index  : Integer := 0;
               Ent        : Entity_Id;
               Expr       : Node_Id;

            begin
               if Ekind (Agg_Type) in Record_Kind then

                  --  The GNAT expander will always put fields in the right
                  --  order, so we can ignore Choices (Expr).

                  Expr := First (Component_Associations (Node));
                  while Present (Expr) loop
                     Ent := Entity (First (Choices (Expr)));

                     --  Ignore discriminants that have
                     --  Corresponding_Discriminants in tagged types since
                     --  we'll be setting those fields in the parent subtype.
                     --  ???

                     if Ekind (Ent) = E_Discriminant
                       and then Present (Corresponding_Discriminant (Ent))
                       and then Is_Tagged_Type (Scope (Ent))
                     then
                        null;

                     --  Also ignore discriminants of Unchecked_Unions.

                     elsif Ekind (Ent) = E_Discriminant
                       and then Is_Unchecked_Union (Agg_Type)
                     then
                        null;
                     else
                        Result := Insert_Value
                          (Env.Bld,
                           Result,
                           Emit_Expr (Expression (Expr)),
                           unsigned (Cur_Index),
                           "");
                        Cur_Index := Cur_Index + 1;
                     end if;

                     Expr := Next (Expr);
                  end loop;
               else
                  pragma Assert (Ekind (Agg_Type) in Array_Kind);
                  return Emit_Array_Aggregate
                    (Env, Node, Number_Dimensions (Agg_Type),
                     LLVM_Type, Create_Type (Env, Component_Type (Agg_Type)));
               end if;

               return Result;
            end;

         when N_If_Expression =>
            return Emit_If_Expression (Env, Node);

         when N_Null =>
            return Const_Null (Create_Type (Env, Full_Etype (Node)));

         when N_Defining_Identifier =>
            return Get_Value (Env, Node);

         when N_In | N_Not_In =>
            declare
               Rng   : Node_Id := Right_Opnd (Node);
               Left  : constant Value_T := Emit_Expr (Left_Opnd (Node));
               Comp1 : Value_T;
               Comp2 : Value_T;

            begin
               pragma Assert (No (Alternatives (Node)));
               pragma Assert (Present (Rng));
               --  The front end guarantees the above.

               if Nkind (Rng) = N_Identifier then
                  Rng := Scalar_Range (Full_Etype (Rng));
               end if;

               Comp1 := Emit_Comparison
                 (Env,
                  (if Nkind (Node) = N_In then N_Op_Ge else N_Op_Lt),
                  Full_Etype (Left_Opnd (Node)), Node,
                  Left, Emit_Expr (Low_Bound (Rng)));

               Comp2 := Emit_Comparison
                 (Env,
                  (if Nkind (Node) = N_In then N_Op_Le else N_Op_Gt),
                  Full_Etype (Left_Opnd (Node)), Node,
                  Left, Emit_Expr (High_Bound (Rng)));

               return Build_Short_Circuit_Op
                    (Env, Empty, Empty, Comp1, Comp2, N_And_Then);
            end;

         when N_Raise_Expression =>
            Discard (Emit_LCH_Call (Env, Node));
            return Get_Undef (Create_Type (Env, Full_Etype (Node)));

         when N_Raise_xxx_Error =>
            pragma Assert (No (Condition (Node)));
            Discard (Emit_LCH_Call (Env, Node));
            return Get_Undef (Create_Type (Env, Full_Etype (Node)));

         when others =>
            Error_Msg_N
              ("unsupported node kind: `" &
               Node_Kind'Image (Nkind (Node)) & "`", Node);
            return Get_Undef (Create_Type (Env, Full_Etype (Node)));
         end case;
      end if;
   end Emit_Expression;

   -------------------
   -- Emit_LCH_Call --
   -------------------

   function Emit_LCH_Call (Env : Environ; Node : Node_Id) return Value_T is
      Void_Ptr_Type : constant Type_T := Pointer_Type (Int_Ty (8), 0);
      Int_Type      : constant Type_T := Create_Type (Env, Standard_Integer);
      Args          : Value_Array (1 .. 2);

      File : constant String :=
        Get_Name_String (Reference_Name (Get_Source_File_Index (Sloc (Node))));

      Element_Type : constant Type_T :=
        Int_Type_In_Context (Env.Ctx, 8);
      Array_Type   : constant Type_T :=
        LLVM.Core.Array_Type (Element_Type, File'Length + 1);
      Elements     : array (1 .. File'Length + 1) of Value_T;
      V            : constant Value_T :=
                       Add_Global (Env.Mdl, Array_Type, "str-lit");

   begin
      --  Build a call to __gnat_last_chance_handler (FILE, LINE)

      --  First build a string literal for FILE

      for J in File'Range loop
         Elements (J) := Const_Int
           (Element_Type,
            unsigned_long_long (Character'Pos (File (J))),
            Sign_Extend => False);
      end loop;

      --  Append NUL character

      Elements (Elements'Last) :=
        Const_Int (Element_Type, 0, Sign_Extend => False);

      Set_Initializer
        (V, Const_Array (Element_Type, Elements'Address, Elements'Length));
      Set_Linkage (V, Private_Linkage);
      Set_Global_Constant (V, True);

      Args (1) := Bit_Cast
        (Env.Bld,
         GEP
           (Env.Bld,
            V,
            (Const_Int (Intptr_T, 0, Sign_Extend => False),
             Const_Int (Create_Type (Env, Standard_Positive),
                        0, Sign_Extend => False)),
            ""),
         Void_Ptr_Type,
         "");

      --  Then provide the line number

      Args (2) := Const_Int
        (Int_Type,
         unsigned_long_long (Get_Logical_Line_Number (Sloc (Node))),
         Sign_Extend => False);
      return Call
        (Env.Bld, Env.LCH_Fn, Args'Address, Args'Length, "");
   end Emit_LCH_Call;

   ---------------
   -- Emit_List --
   ---------------

   procedure Emit_List (Env : Environ; List : List_Id) is
      N : Node_Id;
   begin
      if Present (List) then
         N := First (List);
         while Present (N) loop
            Emit (Env, N);
            N := Next (N);
         end loop;
      end if;
   end Emit_List;

   ---------------------
   -- Emit_Assignment --
   ---------------------

   procedure Emit_Assignment
     (Env                       : Environ;
      LHS_Typ, RHS_Typ          : Entity_Id;
      LValue                    : Value_T;
      E                         : Node_Id;
      E_Value                   : Value_T;
      Forwards_OK, Backwards_OK : Boolean)
   is
      Src_Node : Node_Id := E;
      Dest     : Value_T := LValue;
      Src      : Value_T;
      Inner    : Node_Id;
      Dest_Typ : Entity_Id := LHS_Typ;
      Typ      : Entity_Id := RHS_Typ;
   begin

      --  If we have checked or unchecked conversions between aggregate types
      --  on the RHS, we don't care about then and can strip them off.

      while Present (Src_Node)
        and then (Nkind (Src_Node) = N_Type_Conversion
            or else Nkind (Src_Node) = N_Unchecked_Type_Conversion)
        and then Is_Aggregate_Type (Typ)
        and then Is_Aggregate_Type (Full_Etype (Expression (Src_Node)))
      loop
         Src_Node := Expression (Src_Node);
         Typ := Full_Etype (Src_Node);
      end loop;

      --  Make sure all types have been elaborated.
      Discard (Create_Type (Env, Dest_Typ));
      Discard (Create_Type (Env, Typ));

      while Is_Array_Type (Dest_Typ)
        and then Present (Packed_Array_Impl_Type (Dest_Typ))
      loop
         Dest_Typ := Packed_Array_Impl_Type (Dest_Typ);
      end loop;

      while Is_Array_Type (Typ)
        and then Present (Packed_Array_Impl_Type (Typ))
      loop
         Typ := Packed_Array_Impl_Type (Typ);
      end loop;

      if Is_Array_Type (Dest_Typ) and then Present (Src_Node)
        and then Nkind (Src_Node) = N_Aggregate
        and then Is_Others_Aggregate (Src_Node)
      then
         --  We'll use memset, so we need to find the inner expression

         Inner := Expression (First (Component_Associations (Src_Node)));
         while Nkind (Inner) = N_Aggregate
           and then Is_Others_Aggregate (Inner)
         loop
            Inner := Expression (First (Component_Associations (Inner)));
         end loop;

         if Nkind (Inner) = N_Integer_Literal then
            Src := Const_Int (Int_Ty (8), Intval (Inner));
         elsif Ekind (Entity (Inner)) = E_Enumeration_Literal then
            Src := Const_Int (Int_Ty (8), Enumeration_Rep (Entity (Inner)));
         else
            Error_Msg_N ("unsupported kind of aggregate", E);
            Src := Get_Undef (Int_Ty (8));
         end if;

         declare
            Void_Ptr_Type : constant Type_T := Pointer_Type (Int_Ty (8), 0);

            Args : constant Value_Array (1 .. 5) :=
              (Bit_Cast (Env.Bld, Dest, Void_Ptr_Type, ""),
               Src,
               Compute_Size (Env, Dest_Typ, Typ, Src),
               Const_Int (Int_Ty (32), 1, False),  --  Alignment
               Const_Int (Int_Ty (1), 0, False));  --  Is_Volatile

         begin
            Discard (Call
                       (Env.Bld,
                        Env.Memory_Set_Fn,
                        Args'Address, Args'Length,
                        ""));
         end;

      elsif not Is_Dynamic_Size (Env, Typ)
        and then not Is_Dynamic_Size (Env, Dest_Typ)
      then
         Src := (if E_Value = No_Value_T then Emit_Expression (Env, Src_Node)
                 else E_Value);

         --  If the pointer type of Src is not the same as the type of
         --  Dest, convert it.
         if Pointer_Type (Type_Of (Src),  0) /= Type_Of (Dest) then
            Dest := Bit_Cast (Env.Bld, Dest,
                              Pointer_Type (Type_Of (Src), 0), "");
         end if;

         Store_With_Type (Env, Dest_Typ, Src, Dest);

      else
         Src := (if E_Value = No_Value_T then Emit_LValue (Env, Src_Node)
                 else E_Value);

         if Is_Array_Type (Dest_Typ) then
            Dest := Array_Data (Env, Dest, Dest_Typ);
            Src  := Array_Data (Env, Src, Typ);
         end if;

         declare
            Void_Ptr_Type : constant Type_T := Pointer_Type (Int_Ty (8), 0);

            Args : constant Value_Array (1 .. 5) :=
              (Bit_Cast (Env.Bld, Dest, Void_Ptr_Type, ""),
               Bit_Cast (Env.Bld, Src, Void_Ptr_Type, ""),
               Compute_Size (Env, Dest_Typ, Typ, Src),
               Const_Int (Int_Ty (32), 1, False),  --  Alignment
               Const_Int (Int_Ty (1), 0, False));  --  Is_Volatile

         begin
            Discard (Call
                       (Env.Bld,
                        (if Forwards_OK and then Backwards_OK
                         then Env.Memory_Copy_Fn
                         else Env.Memory_Move_Fn),
                        Args'Address, Args'Length,
                        ""));
         end;
      end if;
   end Emit_Assignment;

   ---------------
   -- Emit_Call --
   ---------------

   function Emit_Call (Env : Environ; Call_Node : Node_Id) return Value_T is
      Subp        : Node_Id := Name (Call_Node);
      Return_Typ  : constant Entity_Id := Full_Etype (Call_Node);
      Void_Return : constant Boolean := Ekind (Return_Typ) = E_Void;
      LLVM_Return_Typ : constant Type_T :=
        (if Void_Return then No_Type_T else Create_Type (Env, Return_Typ));
      Dynamic_Return : constant Boolean :=
        not Void_Return and then Is_Dynamic_Size (Env, Return_Typ);
      Direct_Call : constant Boolean := Nkind (Subp) /= N_Explicit_Dereference;
      Subp_Typ    : constant Entity_Id :=
        (if Direct_Call then Entity (Subp) else Full_Etype (Subp));
      Params      : constant Entity_Iterator := Get_Params (Subp_Typ);
      Param_Assoc, Actual : Node_Id;
      Actual_Type         : Entity_Id;
      Current_Needs_Ptr   : Boolean;

      --  If it's not an identifier, it must be an access to a subprogram and
      --  in such a case, it must accept a static link.

      Anonymous_Access : constant Boolean := not Direct_Call
        and then Present (Associated_Node_For_Itype (Etype (Subp)))
        and then Nkind (Associated_Node_For_Itype (Etype (Subp)))
          /= N_Full_Type_Declaration;
      This_Takes_S_Link     : constant Boolean := Anonymous_Access;

      S_Link         : Value_T;
      LLVM_Func      : Value_T;
      Args_Count     : constant Nat :=
        Params'Length + (if This_Takes_S_Link then 1 else 0) +
                        (if Dynamic_Return then 1 else 0);
      Args           : Value_Array (1 .. Args_Count);
      I, Idx         : Standard.Types.Int := 1;
      P_Type         : Entity_Id;
      Params_Offsets : Name_Maps.Map;
      pragma Unreferenced (LLVM_Return_Typ);

   begin
      for Param of Params loop
         Params_Offsets.Include (Chars (Param), I);
         I := I + 1;
      end loop;

      I := 1;

      if Direct_Call then
         Subp := Entity (Subp);
      end if;

      LLVM_Func := Emit_Expression (Env, Subp);

      if This_Takes_S_Link then
         S_Link := Extract_Value (Env.Bld, LLVM_Func, 1, "static-link");
         LLVM_Func := Extract_Value (Env.Bld, LLVM_Func, 0, "callback");

         if Anonymous_Access then
            LLVM_Func := Bit_Cast
              (Env.Bld, LLVM_Func,
               Create_Access_Type
                 (Env, Designated_Type (Full_Etype (Prefix (Subp)))),
               "");
         end if;
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

         Actual_Type := Full_Etype (Actual);
         while Is_Array_Type (Actual_Type)
           and then Present (Packed_Array_Impl_Type (Actual_Type))
         loop
            Actual_Type := Packed_Array_Impl_Type (Actual_Type);
         end loop;

         Current_Needs_Ptr := Param_Needs_Ptr (Params (Idx));
         Args (Idx) :=
           (if Current_Needs_Ptr
            then Emit_LValue (Env, Actual)
            else Emit_Expression (Env, Actual));

         P_Type := Full_Etype (Params (Idx));

         --  At this point we need to handle view conversions: from array thin
         --  pointer to array fat pointer, unconstrained array pointer type
         --  conversion, ... For other parameters that needs to be passed
         --  as pointers, we should also make sure the pointed type fits
         --  the LLVM formal.

         if Is_Array_Type (Actual_Type) then
            if Is_Constrained (Actual_Type)
              and then not Is_Constrained (P_Type)
            then
               --  Convert from raw to fat pointer

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

      if This_Takes_S_Link then
         Args (Params'Length + 1) := S_Link;
      end if;

      --  Add a pointer to the location of the return value if the return
      --  type is of dynamic size.

      if Dynamic_Return then
         Args (Args'Last) :=
           Allocate_For_Type (Env, Return_Typ, "call-return");
      end if;

      --  If there are any types mismatches for arguments passed by reference,
      --  cast the pointer type.

      declare
         Args_Types : constant Type_Array :=
           Get_Param_Types (Type_Of (LLVM_Func));
      begin
         pragma Assert (Args'Length = Args_Types'Length);

         for J in Args'Range loop
            if Type_Of (Args (J)) /= Args_Types (J)
              and then Get_Type_Kind (Type_Of (Args (J))) = Pointer_Type_Kind
              and then Get_Type_Kind (Args_Types (J)) = Pointer_Type_Kind
            then
               Args (J) := Bit_Cast
                 (Env.Bld, Args (J), Args_Types (J), "param-bitcast");
            end if;
         end loop;
      end;

      --  Set the argument for the static link, if any

      if This_Takes_S_Link then
         Args (Params'Length + 1) := S_Link;
      end if;

      --  If the return type is of dynamic size, call as a procedure and
      --  return the address we set as the last parameter.

      if Dynamic_Return then
         Discard (Call (Env.Bld, LLVM_Func, Args'Address, Args'Length, ""));
         return Args (Args'Last);
      else
         return Call (Env.Bld, LLVM_Func, Args'Address, Args'Length, "");
      end if;
   end Emit_Call;

   --------------------------
   -- Emit_Subprogram_Decl --
   --------------------------

   function Emit_Subprogram_Decl
     (Env : Environ; Subp_Spec : Node_Id) return Value_T
   is
      Def_Ident : constant Node_Id := Defining_Entity (Subp_Spec);
   begin
      --  If this subprogram specification has already been compiled, do
      --  nothing.

      if Has_Value (Env, Def_Ident) then
         return Get_Value (Env, Def_Ident);
      else
         declare
            Subp_Type : constant Type_T :=
              Create_Subprogram_Type_From_Spec (Env, Subp_Spec);

            Subp_Base_Name : constant String :=
              Get_Subprog_Ext_Name (Def_Ident);
            LLVM_Func      : Value_T;

         begin
            --  ??? Special case __gnat_last_chance_handler which is
            --  already defined as Env.LCH_Fn

            if Subp_Base_Name = "__gnat_last_chance_handler" then
               return Env.LCH_Fn;
            end if;

            LLVM_Func :=
              Add_Function
                (Env.Mdl,
                 (if Is_Compilation_Unit (Def_Ident)
                  then "_ada_" & Subp_Base_Name
                  else Subp_Base_Name),
                 Subp_Type);

            --  Define the appropriate linkage

            if not Is_Public (Def_Ident) then
               Set_Linkage (LLVM_Func, Internal_Linkage);
            end if;

            Set_Value (Env, Def_Ident, LLVM_Func);
            return LLVM_Func;
         end;
      end if;
   end Emit_Subprogram_Decl;

   ---------------------------
   -- Build_Type_Conversion --
   ---------------------------

   function Build_Type_Conversion
     (Env                 : Environ;
      Src_Type, Dest_Type : Entity_Id;
      Expr                : Node_Id) return Value_T
   is
      S_Type  : Entity_Id := Get_Fullest_View (Src_Type);
      D_Type  : Entity_Id := Get_Fullest_View (Dest_Type);

   begin

      while Is_Array_Type (S_Type)
        and then Present (Packed_Array_Impl_Type (S_Type))
      loop
         S_Type := Packed_Array_Impl_Type (S_Type);
      end loop;

      while Is_Array_Type (D_Type)
        and then Present (Packed_Array_Impl_Type (D_Type))
      loop
         D_Type := Packed_Array_Impl_Type (D_Type);
      end loop;

      --  If both types are scalar, hand that off to our helper.

      if Is_Scalar_Type (S_Type) and then Is_Scalar_Type (D_Type) then
         return Convert_Scalar_Types (Env, S_Type, D_Type, Expr);

      --  Otherwise, we do the same as an unchecked conversion.

      else
         return Build_Unchecked_Conversion (Env, Src_Type, Dest_Type, Expr);

      end if;
   end Build_Type_Conversion;

   ------------------
   -- Compute_Size --
   ------------------

   function Compute_Size
     (Env                 : Environ;
      Left_Typ, Right_Typ : Entity_Id;
      Right_Value         : Value_T) return Value_T
   is
   begin

      --  If the left type is of constant size, return the size of that
      --  one, otherwise return the size of the right one (for which we have
      --  a value) unless that type is an unconstrained array.

      if not Is_Dynamic_Size (Env, Left_Typ)
        or else (Is_Array_Type (Right_Typ)
                   and then not Is_Constrained (Right_Typ))
      then
         return Get_Type_Size
           (Env, Create_Type (Env, Left_Typ), Left_Typ, No_Value_T);
      else
         return Get_Type_Size
           (Env, Create_Type (Env, Right_Typ), Right_Typ, Right_Value);
      end if;

   end Compute_Size;

   --------------------------
   -- Convert_Scalar_Types --
   --------------------------

   function Convert_Scalar_Types
     (Env            : Environ;
      S_Type, D_Type : Entity_Id;
      Expr           : Node_Id) return Value_T
   is
      Value : constant Value_T := Emit_Expression (Env, Expr);
      type Cvtf is access function
        (Bld : Builder_T; Value : Value_T; Typ : Type_T; Name : String)
        return Value_T;

      Subp      : Cvtf := null;
      Typ       : constant Type_T  := Create_Type (Env, D_Type);
      Src_FP    : constant Boolean := Is_Floating_Point_Type (S_Type);
      Dest_FP   : constant Boolean := Is_Floating_Point_Type (D_Type);
      Src_Uns   : constant Boolean := Is_Unsigned_Type (S_Type);
      Dest_Uns  : constant Boolean := Is_Unsigned_Type (D_Type);
      Src_Size  : constant Uint :=
        (if Is_Modular_Integer_Type (S_Type) then RM_Size (S_Type)
         else Esize (S_Type));
      Dest_Size  : constant Uint :=
        (if Is_Modular_Integer_Type (D_Type) then RM_Size (D_Type)
         else Esize (D_Type));
      Is_Trunc  : constant Boolean := Dest_Size < Src_Size;
   begin

      --  We have four cases: FP to FP, FP to Int, Int to FP, and Int to Int.
      --  In some cases, we do nothing and just return the input.  For the
      --  others, we set Subp to the function to build the appropriate IR.

      if Src_FP and then Dest_FP then
         if Src_Size = Dest_Size then
            return Value;
         else
            Subp := (if Is_Trunc then FP_Trunc'Access else FP_Ext'Access);
         end if;

      elsif Src_FP and then not Dest_FP then
         Subp := (if Dest_Uns then FP_To_UI'Access else FP_To_SI'Access);
      elsif not Src_FP and then Dest_FP then
         Subp := (if Src_Uns then UI_To_FP'Access else SI_To_FP'Access);
      else

         --  Remaining case is descrete to discrete

         if Src_Size = Dest_Size then
            if Src_Uns = Dest_Uns then
               Subp := Bit_Cast'Access;
            else
               return Value;
            end if;
         elsif Is_Trunc then
            Subp := Trunc'Access;
         else
            Subp := (if Src_Uns then Z_Ext'Access else S_Ext'Access);
         end if;
      end if;

      --  Here all that's left to do is generate the IR instruction.

      return Subp (Env.Bld, Value, Typ, "");

   end Convert_Scalar_Types;

   ----------------------------
   -- Convert_To_Scalar_Type --
   ----------------------------
   function Convert_To_Scalar_Type
     (Env         : Environ;
      Expr        : Value_T;
      T           : Entity_Id;
      Is_Unsigned : Boolean) return Value_T
   is
      type Cvtf is access function
        (Bld : Builder_T; Value : Value_T; Typ : Type_T; Name : String)
        return Value_T;

      In_Typ    : constant Type_T := Type_Of (Expr);
      In_Width  : constant Integer := Integer (Get_Int_Type_Width (In_Typ));
      Out_Width : constant Integer := Integer (UI_To_Int (Esize (T)));
      Subp      : Cvtf := null;
   begin
      if In_Width = Out_Width then
         return Expr;
      elsif In_Width > Out_Width then
         Subp := Trunc'Access;
      else
         Subp := (if Is_Unsigned then Z_Ext'Access else S_Ext'Access);
      end if;

      return Subp (Env.Bld, Expr, Create_Type (Env, T), "");
   end Convert_To_Scalar_Type;

   --------------------------------
   -- Build_Unchecked_Conversion --
   --------------------------------

   function Build_Unchecked_Conversion
     (Env                 : Environ;
      S_Type, D_Type      : Entity_Id;
      Expr                : Node_Id) return Value_T
   is
      type Opf is access function
        (Bld : Builder_T; Op : Value_T; Ty : Type_T; Name : String)
        return Value_T;

      Dest_Ty   : constant Type_T := Create_Type (Env, D_Type);
      Value     : constant Value_T := Emit_Expression (Env, Expr);
      Src_Type  : Entity_Id := S_Type;
      Dest_Type : Entity_Id := D_Type;
      Subp      : Opf := null;
   begin

      while Is_Array_Type (Src_Type)
        and then Present (Packed_Array_Impl_Type (Src_Type))
      loop
         Src_Type := Packed_Array_Impl_Type (Src_Type);
      end loop;

      while Is_Array_Type (Dest_Type)
        and then Present (Packed_Array_Impl_Type (Dest_Type))
      loop
         Dest_Type := Packed_Array_Impl_Type (Dest_Type);
      end loop;

      --  If the value is already of the desired LLVM type, we're done.

      if Type_Of (Value) = Dest_Ty then
         return Value;

      --  If converting pointer to pointer or pointer to/from integer, we
      --  just copy the bits using the appropriate instruction.
      elsif Is_Access_Type (Dest_Type) and then Is_Scalar_Type (Src_Type) then
         Subp := Int_To_Ptr'Access;
      elsif Is_Scalar_Type (Dest_Type) and then Is_Access_Type (Src_Type) then
         Subp := Ptr_To_Int'Access;
      elsif Is_Access_Type (Src_Type) and then Is_Access_Type (Dest_Type)
        and then not Is_Access_Unconstrained (Src_Type)
        and then not Is_Access_Unconstrained (Dest_Type)
      then
         Subp := Pointer_Cast'Access;

      --  If these are both integral types, we handle this as a normal
      --  conversion.  Unchecked conversion is only defined if the sizes
      --  are the same, which is handled above by checking for the same
      --  LLVM type, but the front-end generates it, meaning to do
      --  a normal conversion.

      elsif Is_Discrete_Or_Fixed_Point_Type (Dest_Type)
        and then Is_Discrete_Or_Fixed_Point_Type (Src_Type)
      then
         return Convert_To_Scalar_Type (Env, Value, Dest_Type,
                                        Is_Unsigned_Type (Src_Type));

      --  Otherwise, these must be cases where we have to convert by
      --  pointer punning.  If the source is a type of dynamic size, the
      --  value is already a pointer.  Otherwise, we have to make it a
      --  pointer.  ??? This code has a problem in that it calls Emit_LValue
      --  on an expression that's already been elaborated, but let's fix
      --  that double-elaboration issue later.

      else
         declare
            Addr           : constant Value_T :=
              (if Is_Dynamic_Size (Env, Src_Type) then Value
               else Emit_LValue (Env, Expr));
            Dest_Ptr       : constant Type_T := Pointer_Type (Dest_Ty, 0);
            Converted_Addr : constant Value_T :=
               Pointer_Cast (Env.Bld, Addr, Dest_Ptr, "unc-ptr-cvt");
         begin
            if Is_Dynamic_Size (Env, Dest_Type) then
               return Converted_Addr;
            else
               return Load_With_Type (Env, Dest_Type, Converted_Addr);
            end if;
         end;
      end if;

      --  If we get here, we should have set Subp to point to the function
      --  to call to do the conversion.

      return Subp (Env.Bld, Value, Dest_Ty, "unchecked-conv");
   end Build_Unchecked_Conversion;

   ------------------
   -- Emit_Min_Max --
   ------------------

   function Emit_Min_Max
     (Env         : Environ;
      Exprs       : List_Id;
      Compute_Max : Boolean) return Value_T
   is
      Expr_Type : constant Entity_Id := Full_Etype (First (Exprs));
      Left      : constant Value_T := Emit_Expression (Env, First (Exprs));
      Right     : constant Value_T := Emit_Expression (Env, Last (Exprs));
      Choose    : constant Value_T :=
          Emit_Comparison (Env, (if Compute_Max then N_Op_Gt else N_Op_Lt),
                           Expr_Type, First (Exprs), Left, Right);
   begin
      return Build_Select (Env.Bld, Choose, Left, Right,
                           (if Compute_Max then "max" else "min"));
   end Emit_Min_Max;

   --------------------
   -- Emit_Aggregate --
   --------------------

   function Emit_Array_Aggregate
     (Env           : Environ;
      Node          : Node_Id;
      Dims_Left     : Pos;
      Typ, Comp_Typ : Type_T) return Value_T
   is
      Result     : Value_T := Get_Undef (Typ);
      Cur_Expr   : Value_T;
      Cur_Index  : Integer := 0;
      Expr       : Node_Id;
   begin
      Expr := First (Expressions (Node));
      while Present (Expr) loop
         --  If this is a nested N_Aggregate and we have dimensions left
         --  in the outer array, use recursion to fill in the aggregate
         --  since we won't have the proper type for the inner aggregate.
         if Nkind (Expr) = N_Aggregate and then Dims_Left > 1 then
            Cur_Expr := Emit_Array_Aggregate
              (Env, Expr, Dims_Left - 1, Get_Element_Type (Typ), Comp_Typ);

         --  If the expression is a conversion to an unconstrained
         --  array type, skip it to avoid spilling to memory.

         elsif Nkind (Expr) = N_Type_Conversion
           and then Is_Array_Type (Full_Etype (Expr))
           and then not Is_Constrained (Full_Etype (Expr))
         then
            Cur_Expr := Emit_Expression (Env, Expression (Expr));
         else
            Cur_Expr := Emit_Expression (Env, Expr);
         end if;

         --  If this operand's type is a pointer and so is the element
         --  type, but they aren't the same, convert.
         if Type_Of (Cur_Expr) /= Comp_Typ
           and then Get_Type_Kind (Comp_Typ) = Pointer_Type_Kind
           and then Get_Type_Kind (Type_Of (Cur_Expr)) = Pointer_Type_Kind
         then
            Cur_Expr := Bit_Cast (Env.Bld, Cur_Expr, Comp_Typ, "");
         end if;

         Result := Insert_Value
           (Env.Bld, Result, Cur_Expr, unsigned (Cur_Index), "");
         Cur_Index := Cur_Index + 1;
         Expr := Next (Expr);
      end loop;

      return Result;
   end Emit_Array_Aggregate;

   ------------------------------
   -- Emit_Attribute_Reference --
   ------------------------------

   function Emit_Attribute_Reference
     (Env    : Environ;
      Node   : Node_Id;
      LValue : Boolean) return Value_T
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
            if LValue then
               return Emit_LValue (Env, Prefix (Node));
            else
               return Ptr_To_Int
                 (Env.Bld,
                  Emit_LValue
                    (Env, Prefix (Node)), Get_Address_Type, "attr-address");
            end if;

         when Attribute_Deref =>
            declare
               Expr : constant Node_Id := First (Expressions (Node));
               pragma Assert (Is_Descendant_Of_Address (Full_Etype (Expr)));

               Val : constant Value_T :=
                 Int_To_Ptr
                   (Env.Bld,
                    Emit_Expression (Env, Expr),
                    Create_Access_Type (Env, Full_Etype (Node)), "attr-deref");

            begin
               if LValue or else Is_Dynamic_Size (Env, Full_Etype (Node)) then
                  return Val;
               else
                  return Load (Env.Bld, Val, "attr-deref");
               end if;
            end;

         when Attribute_First
            | Attribute_Last
            | Attribute_Length =>

            declare
               Prefix_Type : constant Entity_Id := Full_Etype (Prefix (Node));
               Array_Descr : Value_T;
               Result      : Value_T;
               Dim         : constant Nat :=
                 (if Present (Expressions (Node)) then
                  UI_To_Int (Intval (First (Expressions (Node)))) - 1
                  else 0);

            begin
               if Is_Scalar_Type (Prefix_Type) then
                  if Attr = Attribute_First then
                     Result := Emit_Expression
                       (Env, Type_Low_Bound (Prefix_Type));
                  elsif Attr = Attribute_Last then
                     Result := Emit_Expression
                       (Env, Type_High_Bound (Prefix_Type));
                  else
                     Error_Msg_N ("unsupported attribute", Node);
                     Result :=
                       Get_Undef (Create_Type (Env, Full_Etype (Node)));
                  end if;

               elsif Is_Array_Type (Prefix_Type) then

                  --  If what we're taking the prefix of is a type, we can't
                  --  evaluate it as an expression.

                  if Is_Entity_Name (Prefix (Node))
                    and then Is_Type (Entity (Prefix (Node)))
                  then
                     Array_Descr := No_Value_T;
                  else
                     Array_Descr := Emit_LValue (Env, Prefix (Node));
                  end if;

                  if Attr = Attribute_Length then
                     Result :=
                       Get_Array_Length (Env, Prefix_Type, Dim, Array_Descr);
                  else
                     Result := Get_Array_Bound
                       (Env, Prefix_Type, Dim, Attr = Attribute_First,
                        Array_Descr);
                  end if;
               else
                  Error_Msg_N ("unsupported attribute", Node);
                  Result := Get_Undef (Create_Type (Env, Full_Etype (Node)));
               end if;

               return Convert_To_Scalar_Type
                 (Env, Result, Full_Etype (Node),
                  Is_Unsigned_Type (Full_Etype (Node)));
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
               Full_Etype (First (Expressions (Node))),
               Full_Etype (Node),
               First (Expressions (Node)));

         when Attribute_Succ
            | Attribute_Pred =>
            declare
               Exprs : constant List_Id := Expressions (Node);
               pragma Assert (List_Length (Exprs) = 1);

               Base : constant Value_T := Emit_Expression (Env, First (Exprs));
               T    : constant Type_T := Type_Of (Base);
               pragma Assert (Get_Type_Kind (T) = Integer_Type_Kind);

               One  : constant Value_T :=
                 Const_Int (T, 1, Sign_Extend => False);

            begin
               return
                 (if Attr = Attribute_Succ
                  then NSW_Add (Env.Bld, Base, One, "attr-succ")
                  else NSW_Sub (Env.Bld, Base, One, "attr-pred"));
            end;

         when Attribute_Machine =>
            --  ??? For now return the prefix itself. Would need to force a
            --  store in some cases.

            return Emit_Expression (Env, First (Expressions (Node)));

         when Attribute_Alignment =>
            declare
               Typ : constant Node_Id := Full_Etype (Node);
               Pre : constant Node_Id := Full_Etype (Prefix (Node));
            begin
               return Const_Int
                 (Create_Type (Env, Typ),
                  unsigned_long_long (Get_Type_Alignment
                   (Env, Create_Type (Env, Pre))),
                  Sign_Extend => False);
            end;

         when Attribute_Size =>
            declare

               Typ        : constant Node_Id := Full_Etype (Prefix (Node));
               LLVM_Typ   : constant Type_T := Create_Type (Env, Typ);
               Result_Typ : constant Node_Id := Full_Etype (Node);
               Const_8    : constant Value_T :=
                 Const_Int (Env.Size_Type, 8, False);
               For_Type   : constant Boolean :=
                 (Is_Entity_Name (Prefix (Node))
                    and then Is_Type (Entity (Prefix (Node))));
               Value      : Value_T := No_Value_T;

            begin
               if not For_Type then
                  Value := Emit_LValue (Env, Prefix (Node));
               end if;

               return
                 Convert_To_Scalar_Type
                 (Env,
                  NSW_Mul (Env.Bld,
                           Get_Type_Size (Env, LLVM_Typ, Typ, Value, For_Type),
                           Const_8,
                           ""),
                  Result_Typ, Is_Unsigned => False);
            end;

         when others =>
            Error_Msg_N
              ("unsupported attribute: `" &
               Attribute_Id'Image (Attr) & "`", Node);
            return Get_Undef (Create_Type (Env, Full_Etype (Node)));
      end case;
   end Emit_Attribute_Reference;

   ---------------------
   -- Emit_Comparison --
   ---------------------

   function Emit_Comparison
     (Env      : Environ;
      Kind     : Node_Kind;
      Op_Type  : Entity_Id;
      LHS, RHS : Node_Id) return Value_T
   is
      Operation    : constant Pred_Mapping := Get_Preds (Kind);
      Operand_Type : Entity_Id := Op_Type;
      function Subp_Ptr (Node : Node_Id) return Value_T is
        (if Nkind (Node) = N_Null
         then Const_Null (Pointer_Type (Int_Ty (8), 0))
         else Load
           (Env.Bld,
            Struct_GEP
              (Env.Bld, Emit_LValue (Env, Node), 0, "subp-addr"),
            ""));
      --  Return the subprogram pointer associated with Node

   begin

      while Is_Array_Type (Operand_Type)
        and then Present (Packed_Array_Impl_Type (Operand_Type))
      loop
         Operand_Type := Packed_Array_Impl_Type (Operand_Type);
      end loop;

      --  LLVM treats pointers as integers regarding comparison

      if Ekind (Operand_Type) = E_Anonymous_Access_Subprogram_Type then
         --  ??? It's unclear why there's special handling here that's
         --  not present in Gigi.
         return I_Cmp
           (Env.Bld,
            Operation.Unsigned,
            Subp_Ptr (LHS),
            Subp_Ptr (RHS),
            "");

      elsif Is_Floating_Point_Type (Operand_Type)
        or else Is_Discrete_Or_Fixed_Point_Type (Operand_Type)
        or else Is_Access_Type (Operand_Type)
      then
         return Emit_Comparison (Env, Kind, Operand_Type, LHS,
                                 Emit_Expression (Env, LHS),
                                 Emit_Expression (Env, RHS));

      elsif Is_Record_Type (Operand_Type) then
         Error_Msg_N ("unsupported record comparison", LHS);
         return Get_Undef (Int_Ty (1));

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
              Const_Int (Bool_Type, 0, False);
            True_Val     : constant Value_T :=
              Const_Int (Bool_Type, 1, False);

            LHS_Descr    : constant Value_T := Emit_LValue (Env, LHS);
            LHS_Type     : constant Entity_Id := Full_Etype (LHS);
            RHS_Descr    : constant Value_T := Emit_LValue (Env, RHS);
            RHS_Type     : constant Entity_Id := Full_Etype (RHS);

            Left_Length  : constant Value_T :=
              Get_Array_Length (Env, LHS_Type, 0, LHS_Descr);
            Right_Length : constant Value_T :=
              Get_Array_Length (Env, RHS_Type, 0, RHS_Descr);
            Null_Length  : constant Value_T :=
              Const_Null (Type_Of (Left_Length));
            Same_Length  : constant Value_T := I_Cmp
              (Env.Bld, Int_NE, Left_Length,
               Right_Length, "test-same-length");

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
            Results (1) := (if Kind = N_Op_Eq then False_Val else True_Val);

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
            Results (2) := (if Kind = N_Op_Eq then True_Val else False_Val);

            --  If we jump from here to BB_Merge, we are returning True

            Position_Builder_At_End (Env.Bld, Basic_Blocks (3));

            declare
               Left        : constant Value_T :=
                 Array_Data (Env, LHS_Descr, LHS_Type);
               Right       : constant Value_T :=
                 Array_Data (Env, RHS_Descr, RHS_Type);

               Void_Ptr_Type : constant Type_T := Pointer_Type (Int_Ty (8), 0);
               Comp_Type     : constant Entity_Id :=
                 Get_Fullest_View (Component_Type (Full_Etype (LHS)));
               LLVM_Comp_Typ : constant Type_T := Create_Type (Env, Comp_Type);
               Size          : constant Value_T :=
                 NSW_Mul
                   (Env.Bld,
                    Z_Ext (Env.Bld, Left_Length, Env.Size_Type, ""),
                    Get_Type_Size (Env, LLVM_Comp_Typ, Comp_Type, No_Value_T),
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
         return Get_Undef (Int_Ty (1));
      end if;
   end Emit_Comparison;

   function Emit_Comparison
     (Env          : Environ;
      Kind         : Node_Kind;
      Operand_Type : Entity_Id;
      Node         : Node_Id;
      LHS, RHS     : Value_T) return Value_T
   is
      Operation    : constant Pred_Mapping := Get_Preds (Kind);
   begin
      --  If these are fat pointer, they are equal iff their addresses are
      --  equal.  It's not possible for the addresses to be equal and not
      --  the bounds. We can't make a recursive call here or we'll try to do
      --  it again that time.

      if Is_Access_Unconstrained (Operand_Type) then
         return I_Cmp
           (Env.Bld, Operation.Unsigned,
            Array_Data (Env, LHS, Designated_Type (Operand_Type)),
            Array_Data (Env, RHS, Designated_Type (Operand_Type)),
            "");

      elsif Is_Floating_Point_Type (Operand_Type) then
         return F_Cmp
           (Env.Bld, Operation.Real, LHS, RHS, "");

      elsif Is_Discrete_Or_Fixed_Point_Type (Operand_Type)
        or else Is_Access_Type (Operand_Type)
      then
         return I_Cmp
           (Env.Bld,
            (if Is_Unsigned_Type (Operand_Type)
               or else Is_Access_Type (Operand_Type)
             then Operation.Unsigned
             else Operation.Signed),
            LHS, RHS, "");

      else
         Error_Msg_N
           ("unsupported operand type for comparison: `"
            & Entity_Kind'Image (Ekind (Operand_Type)) & "`", Node);
         return Get_Undef (Int_Ty (1));
      end if;
   end Emit_Comparison;

   ---------------
   -- Emit_Case --
   ---------------

   procedure Emit_Case (Env : Environ; Node : Node_Id) is

      function Count_Choices (Node : Node_Id) return Nat;
      --  Count the total number of choices in this case statement.

      -------------------
      -- Count_Choices --
      -------------------

      function Count_Choices (Node : Node_Id) return Nat is
         Num_Choices  : Nat := 0;
         Alt          : Node_Id;
         First_Choice : Node_Id;
      begin
         Alt := First (Alternatives (Node));
         while Present (Alt) loop

            --  We have a peculiarity in the "others" case of a case statement.
            --  The Alternative points to a list of choices of which the
            --  first choice is an N_Others_Choice.  So handle  that specially
            --  both here and when we compute our Choices below.

            First_Choice := First (Discrete_Choices (Alt));
            Num_Choices := Num_Choices +
              (if Nkind (First_Choice) = N_Others_Choice then
               List_Length (Others_Discrete_Choices (First_Choice))
               else List_Length (Discrete_Choices (Alt)));
            Alt := Next (Alt);
         end loop;

         return Num_Choices;
      end Count_Choices;

      --  We have data structures to record information about each choice
      --  and each alternative in the case statement.  For each choice, we
      --  record the bounds and costs.  The "if" cost is one if both bounds
      --  are the same, otherwise two.  The "switch" cost is the size of the
      --  range, if known and fits in an integer, otherwise a large number
      --  (we arbitrary use 1000).  For the alternative, we record the
      --  basic block in which we've emitted the relevant code, the basic
      --  block we'll use for the test (in the "if" case), the first and
      --  last choice, and the total costs for all the choices in this
      --  alternative.

      type One_Choice is record
         Low, High            : Uint;
         If_Cost, Switch_Cost : Nat;
      end record;

      type One_Alt is record
         BB                        : Basic_Block_T;
         First_Choice, Last_Choice : Nat;
         If_Cost, Switch_Cost      : Nat;
      end record;

      Num_Alts         : constant Nat := List_Length (Alternatives (Node));
      Alts             : array (1 .. Num_Alts) of One_Alt;
      Choices          : array (1 .. Count_Choices (Node)) of One_Choice;
      LHS              : constant Value_T :=
        Emit_Expression (Env, Expression (Node));
      LHS_Type         : constant Entity_Id := Full_Etype (Expression (Node));
      Typ              : constant Type_T := Create_Type (Env, LHS_Type);
      Start_BB         : constant Basic_Block_T := Get_Insert_Block (Env.Bld);
      Current_Alt      : Nat := 1;
      First_Choice     : Nat;
      Current_Choice   : Nat := 1;
      Alt, Choice      : Node_Id;
      Low, High        : Uint;
      If_Cost          : Nat;
      Switch_Cost      : Nat;
      BB               : Basic_Block_T;
      BB_End           : constant Basic_Block_T :=
        Create_Basic_Block (Env, "switch-end");
      Switch           : Value_T;

      procedure Swap_Highest_Cost (Is_Switch : Boolean);
      --  Move the highest-cost alternative to the last entry.  Is_Switch
      --  says whether we look at the switch cost or the if cost.

      procedure Swap_Highest_Cost (Is_Switch : Boolean) is
         Temp_Alt         : One_Alt;
         Worst_Alt        : Nat;
         Worst_Cost       : Nat;
         Our_Cost         : Nat;
      begin
         Worst_Alt := Alts'Last;
         Worst_Cost := 0;
         for I in Alts'Range loop
            Our_Cost := (if Is_Switch then Alts (I).Switch_Cost
                         else Alts (I).If_Cost);
            if Our_Cost > Worst_Cost then
               Worst_Cost := Our_Cost;
               Worst_Alt := I;
            end if;
         end loop;

         Temp_Alt := Alts (Alts'Last);
         Alts (Alts'Last) := Alts (Worst_Alt);
         Alts (Worst_Alt) := Temp_Alt;
      end Swap_Highest_Cost;

   begin

      --  First we scan all the alternatives and choices and fill in most
      --  of the data.  We emit the code for each alternative as part of
      --  that process.

      Alt := First (Alternatives (Node));
      while Present (Alt) loop
         First_Choice := Current_Choice;
         BB := Create_Basic_Block (Env, "case-alt");
         Position_Builder_At_End (Env.Bld, BB);
         Emit_List (Env, Statements (Alt));
         Discard (Build_Br (Env.Bld, BB_End));

         Choice := First (Discrete_Choices (Alt));
         if Nkind (Choice) = N_Others_Choice then
            Choice := First (Others_Discrete_Choices (Choice));
         end if;

         while Present (Choice) loop
            Decode_Range (Choice, Low, High);

            --  When we compute the cost, set the cost of a null range
            --  to zero.  If the if cost is 0 or 1, that's the switch cost too,
            --  but if either of the bounds aren't in Int, we can't use
            --  switch at all.

            If_Cost := (if Low > High then 0 elsif Low = High then 1 else 2);

            Switch_Cost := (if not UI_Is_In_Int_Range (Low)
                            or else not UI_Is_In_Int_Range (High)
                            then 1000
                            elsif If_Cost <= 1 then If_Cost
                            elsif Integer (UI_To_Int (Low)) /= Integer'First
                            and then Integer (UI_To_Int (High)) /= Integer'Last
                            and then UI_To_Int (High) - UI_To_Int (Low) < 1000
                            then UI_To_Int (High) - UI_To_Int (Low) + 1
                            else 1000);
            Choices (Current_Choice) := (Low => Low, High => High,
                                         If_Cost => If_Cost,
                                         Switch_Cost => Switch_Cost);
            Current_Choice := Current_Choice + 1;
            Choice := Next (Choice);
         end loop;

         If_Cost := 0;
         Switch_Cost := 0;

         --  Sum up the costs of all the choices in this alternative.

         for I in First_Choice .. Current_Choice - 1 loop
            If_Cost := If_Cost + Choices (I).If_Cost;
            Switch_Cost := Switch_Cost + Choices (I).Switch_Cost;
         end loop;

         Alts (Current_Alt) := (BB => BB, First_Choice => First_Choice,
                                Last_Choice => Current_Choice - 1,
                                If_Cost => If_Cost,
                                Switch_Cost => Switch_Cost);
         Current_Alt := Current_Alt + 1;
         Alt := Next (Alt);
      end loop;

      --  We have two strategies: we can use an LLVM switch instruction if
      --  there aren't too many choices.  If not, we use "if".  First we
      --  find the alternative with the largest switch cost and make that
      --  the "others" option.  Then we see if the total cost of the remaining
      --  alternatives is low enough (we use 100).  If so, use that approach.

      Swap_Highest_Cost (True);
      Position_Builder_At_End (Env.Bld, Start_BB);
      Switch_Cost := 0;
      for I in Alts'First .. Alts'Last - 1 loop
         Switch_Cost := Switch_Cost + Alts (I).Switch_Cost;
      end loop;

      if Switch_Cost < 100 then

         --  First we emit the actual "switch" statement, then we add
         --  the cases to it.  Here we collect all the basic blocks.

         declare
            BBs : array (Alts'Range) of Basic_Block_T;
         begin
            for I in BBs'Range loop
               BBs (I) := Alts (I).BB;
            end loop;

            Switch := Build_Switch (Env.Bld, LHS, BBs (BBs'Last), BBs'Length);
            for I in Alts'First .. Alts'Last - 1 loop
               for J in Alts (I).First_Choice .. Alts (I).Last_Choice loop
                  for K in UI_To_Int (Choices (J).Low) ..
                    UI_To_Int (Choices (J).High) loop
                     Add_Case (Switch,
                               Const_Int (Typ,
                                          unsigned_long_long (Integer (K)),
                                          Sign_Extend => True),
                               Alts (I).BB);
                  end loop;
               end loop;
            end loop;
         end;

      else

         --  Otherwise, we generate if/elsif/elsif/else.

         Swap_Highest_Cost (False);
         for I in Alts'First .. Alts'Last - 1 loop
            for J in Alts (I).First_Choice .. Alts (I).Last_Choice loop

               --  Only do something if this is not a null range.

               if Choices (J).If_Cost /= 0 then

                  --  If we're processing the very last choice, then
                  --  if the choice is not a match, we go to "others".
                  --  Otherwise, we go to a new basic block that's the
                  --  next choice.  Note that we can't simply test
                  --  against Choices'Last because we may have swapped
                  --  some other alternative with Alts'Last.

                  if I = Alts'Last - 1 and then J = Alts (I).Last_Choice then
                     BB := Alts (Alts'Last).BB;
                  else
                     BB := Create_Basic_Block (Env, "case-when");
                  end if;

                  Emit_If_Range (Env, Node, LHS_Type, LHS,
                                 Choices (J).Low, Choices (J).High,
                                 Alts (I).BB, BB);
                  Position_Builder_At_End (Env.Bld, BB);
               end if;
            end loop;
         end loop;
      end if;

      Position_Builder_At_End (Env.Bld, BB_End);
   end Emit_Case;

   -------------
   -- Emit_If --
   -------------

   procedure Emit_If (Env : Environ; Node : Node_Id) is

      --  Record information about each part of an "if" statement.
      type If_Ent is record
         Cond     : Node_Id;         --  Expression to test.
         Stmts    : List_Id;         --  Statements to emit if true.
         BB_True  : Basic_Block_T;   --  Basic block to branch for true.
         BB_False : Basic_Block_T;   --  Basic block to branch for false.
      end record;

      If_Parts     : array (0 .. List_Length (Elsif_Parts (Node))) of If_Ent;

      BB_End       : Basic_Block_T;
      If_Parts_Pos : Nat := 1;
      Elsif_Part   : Node_Id;

   begin

      --  First go through all the parts of the "if" statement recording
      --  the expressions and statements.
      If_Parts (0) := (Cond => Condition (Node),
                       Stmts => Then_Statements (Node),
                       BB_True => Create_Basic_Block (Env, "true"),
                       BB_False => Create_Basic_Block (Env, "false"));

      if Present (Elsif_Parts (Node)) then
         Elsif_Part := First (Elsif_Parts (Node));
         while Present (Elsif_Part) loop
            If_Parts (If_Parts_Pos) := (Cond => Condition (Elsif_Part),
                                        Stmts => Then_Statements (Elsif_Part),
                                        BB_True => Create_Basic_Block
                                          (Env, "true"),
                                       BB_False => Create_Basic_Block
                                         (Env, "false"));
            If_Parts_Pos := If_Parts_Pos + 1;
            Elsif_Part := Next (Elsif_Part);
         end loop;
      end if;

      --  When done, each part goes to the end of the statement.  If there's
      --  an "else" clause, it's a new basic block and the end; otherwise,
      --  it's the last False block.
      BB_End := (if Present (Else_Statements (Node))
                 then Create_Basic_Block (Env, "end")
                 else If_Parts (If_Parts_Pos - 1).BB_False);

      --  Now process each entry that we made: test the condition and branch;
      --  emit the statements in the appropriate block; branch to the end;
      --  and set up the block for the next test, the "else", or next
      --  statement.

      for Part of If_Parts loop
         Emit_If_Cond (Env, Part.Cond, Part.BB_True, Part.BB_False);
         Position_Builder_At_End (Env.Bld, Part.BB_True);
         Emit_List (Env, Part.Stmts);
         Discard (Build_Br (Env.Bld, BB_End));
         Position_Builder_At_End (Env.Bld, Part.BB_False);
      end loop;

      --  If there's an Else part, emit it and go into the "end" basic block.
      if Present (Else_Statements (Node)) then
         Emit_List (Env, Else_Statements (Node));
         Discard (Build_Br (Env.Bld, BB_End));
         Position_Builder_At_End (Env.Bld, BB_End);
      end if;

   end Emit_If;

   ------------------
   -- Emit_If_Cond --
   ------------------

   procedure Emit_If_Cond
     (Env               : Environ;
      Cond              : Node_Id;
      BB_True, BB_False : Basic_Block_T)
   is
      BB_New : Basic_Block_T;
   begin
      case Nkind (Cond) is

         --  Process operations that we can handle in terms of different branch
         --  mechanisms, such as short-circuit operators.

         when N_Op_Not =>
            Emit_If_Cond (Env, Right_Opnd (Cond), BB_False, BB_True);
            return;

         when N_And_Then | N_Or_Else =>

            --  Depending on the result of the the test of the left operand,
            --  we either go to a final basic block or to a new intermediate
            --  one where we test the right operand.

            BB_New := Create_Basic_Block (Env, "short-circuit");
            Emit_If_Cond (Env, Left_Opnd (Cond),
                          (if Nkind (Cond) = N_And_Then
                           then BB_New else BB_True),
                          (if Nkind (Cond) = N_And_Then
                           then BB_False else BB_New));
            Position_Builder_At_End (Env.Bld, BB_New);
            Emit_If_Cond (Env, Right_Opnd (Cond), BB_True, BB_False);
            return;

         when N_In | N_Not_In =>

            --  If we can decode the range into Uint's, we can just do
            --  simple comparisons.

            declare
               Low, High     : Uint;
            begin
               Decode_Range (Right_Opnd (Cond), Low, High);
               if Low /= No_Uint and then High /= No_Uint then
                  Emit_If_Range
                    (Env, Cond, Full_Etype (Left_Opnd (Cond)),
                     Emit_Expression (Env, Left_Opnd (Cond)), Low, High,
                     (if Nkind (Cond) = N_In then BB_True else BB_False),
                     (if Nkind (Cond) = N_In then BB_False else BB_True));
                  return;
               end if;
            end;

         when others =>
            null;

      end case;

      --  If we haven't handled it via one of the special cases above,
      --  just evaluate the expression and do the branch.

      Discard (Build_Cond_Br (Env.Bld, Emit_Expression (Env, Cond),
                              BB_True, BB_False));

   end Emit_If_Cond;

   -------------------
   -- Emit_If_Range --
   -------------------

   procedure Emit_If_Range
     (Env               : Environ;
      Node              : Node_Id;
      Operand_Type      : Entity_Id;
      LHS               : Value_T;
      Low, High         : Uint;
      BB_True, BB_False : Basic_Block_T)
   is
      Cond              : Value_T;
      Inner_BB          : Basic_Block_T;
      LLVM_Type         : constant Type_T := Create_Type (Env, Operand_Type);
   begin

      --  For discrete types (all we handle here), handle ranges by testing
      --  against the high and the low and branching as appropriate.  We
      --  must be sure to evaluate the LHS only once.  But first check for
      --  a range of size one since that's only one comparison.

      if Low = High then
         Cond := Emit_Comparison
           (Env, N_Op_Eq, Operand_Type, Node,
            LHS, Const_Int (LLVM_Type, Low));
         Discard (Build_Cond_Br (Env.Bld, Cond, BB_True, BB_False));
      else
         Inner_BB := Create_Basic_Block (Env, "range-test");
         Cond := Emit_Comparison (Env, N_Op_Ge, Operand_Type, Node,
                                  LHS, Const_Int (LLVM_Type, Low));
         Discard (Build_Cond_Br (Env.Bld, Cond, Inner_BB, BB_False));
         Position_Builder_At_End (Env.Bld, Inner_BB);
         Cond := Emit_Comparison (Env, N_Op_Le, Operand_Type, Node,
                                  LHS, Const_Int (LLVM_Type, High));
         Discard (Build_Cond_Br (Env.Bld, Cond, BB_True, BB_False));
      end if;
   end Emit_If_Range;

   ------------------------
   -- Emit_If_Expression --
   ------------------------

   function Emit_If_Expression
     (Env  : Environ;
      Node : Node_Id) return Value_T
   is
      Condition  : constant Node_Id := First (Expressions (Node));
      Then_Expr  : constant Node_Id := Next (Condition);
      Else_Expr  : constant Node_Id := Next (Then_Expr);

      BB_Then, BB_Else, BB_Next : Basic_Block_T;
      --  BB_Then is the basic block we jump to if the condition is true.
      --  BB_Else is the basic block we jump to if the condition is false.
      --  BB_Next is the BB we jump to after the IF is executed.

      Then_Value, Else_Value : Value_T;

   begin
      BB_Then := Create_Basic_Block (Env, "if-then");
      BB_Else := Create_Basic_Block (Env, "if-else");
      BB_Next := Create_Basic_Block (Env, "if-next");
      Discard
        (Build_Cond_Br
          (Env.Bld, Emit_Expression (Env, Condition), BB_Then, BB_Else));

      --  Emit code for the THEN part

      Position_Builder_At_End (Env.Bld, BB_Then);

      Then_Value := Emit_Expression (Env, Then_Expr);

      --  The THEN part may be composed of multiple basic blocks. We want
      --  to get the one that jumps to the merge point to get the PHI node
      --  predecessor.

      BB_Then := Get_Insert_Block (Env.Bld);

      Discard (Build_Br (Env.Bld, BB_Next));

      --  Emit code for the ELSE part

      Position_Builder_At_End (Env.Bld, BB_Else);

      Else_Value := Emit_Expression (Env, Else_Expr);
      Discard (Build_Br (Env.Bld, BB_Next));

      --  We want to get the basic blocks that jumps to the merge point: see
      --  above.

      BB_Else := Get_Insert_Block (Env.Bld);

      --  Then prepare the instruction builder for the next
      --  statements/expressions and return a merged expression if needed.

      Position_Builder_At_End (Env.Bld, BB_Next);

      --  ??? We can't use Phi if this is a composite type: Phi can only
      --  be used in LLVM for first-class types.

      declare
         Values : constant Value_Array (1 .. 2) := (Then_Value, Else_Value);
         BBs    : constant Basic_Block_Array (1 .. 2) := (BB_Then, BB_Else);
         Phi    : constant Value_T :=
           LLVM.Core.Phi (Env.Bld, Type_Of (Then_Value), "");
      begin
         Add_Incoming (Phi, Values'Address, BBs'Address, 2);
         return Phi;
      end;
   end Emit_If_Expression;

   ------------------
   -- Emit_Literal --
   ------------------

   function Emit_Literal (Env : Environ; Node : Node_Id) return Value_T is
   begin
      case Nkind (Node) is
         when N_Character_Literal =>
            return Const_Int
              (Create_Type (Env, Full_Etype (Node)),
               Char_Literal_Value (Node));

         when N_Integer_Literal =>
            return Const_Int
              (Create_Type (Env, Full_Etype (Node)), Intval (Node));

         when N_Real_Literal =>
            if Is_Fixed_Point_Type (Full_Etype (Node)) then
               return Const_Int
                 (Create_Type (Env, Full_Etype (Node)),
                  Corresponding_Integer_Value (Node));
            else
               declare
                  Real_Type           : constant Type_T :=
                    Create_Type (Env, Full_Etype (Node));
                  Val                 : Ureal := Realval (Node);
                  FP_Num, FP_Denom    : double;

               begin
                  if UR_Is_Zero (Val) then
                     return Const_Real (Real_Type, 0.0);
                  end if;

                  --  First convert the value to a machine number if it isn't
                  --  already. That will force the base to 2 for non-zero
                  --  values and simplify the rest of the logic.

                  if not Is_Machine_Number (Node) then
                     Val := Machine
                       (Implementation_Base_Type (Full_Etype (Node)),
                        Val, Round_Even, Node);
                  end if;

                  pragma Assert (Rbase (Val) = 2);

                  --  ??? This code is not necessarily the most efficient,
                  --  may not give full precision in all cases, and may not
                  --  handle denormalized constants, but should work in enough
                  --  cases for now.

                  FP_Num :=
                    double (UI_To_Long_Long_Integer (Numerator (Val)));
                  if UR_Is_Negative (Val) then
                     FP_Num := -FP_Num;
                  end if;

                  FP_Denom :=
                    2.0 ** (Integer (-UI_To_Int (Denominator (Val))));
                  return Const_Real (Real_Type, FP_Num * FP_Denom);
               end;
            end if;

         when N_String_Literal =>
            declare
               String       : constant String_Id := Strval (Node);
               Array_Type   : constant Type_T :=
                 Create_Type (Env, Full_Etype (Node));
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
                     Sign_Extend => False);
               end loop;

               return Const_Array (Element_Type, Elements'Address, Length);
            end;

         when others =>
            Error_Msg_N ("unhandled literal node", Node);
            return Get_Undef (Create_Type (Env, Full_Etype (Node)));

      end case;
   end Emit_Literal;

   ----------------
   -- Emit_Shift --
   ----------------

   function Emit_Shift
     (Env                 : Environ;
      Node                : Node_Id;
      LHS_Node, RHS_Node  : Node_Id) return Value_T
   is
      To_Left, Rotate, Arithmetic : Boolean := False;

      LHS       : constant Value_T := Emit_Expression (Env, LHS_Node);
      RHS       : constant Value_T := Emit_Expression (Env, RHS_Node);
      Operation : constant Node_Kind := Nkind (Node);
      Result    : Value_T := LHS;
      LHS_Type  : constant Type_T := Type_Of (LHS);
      N         : Value_T := S_Ext (Env.Bld, RHS, LHS_Type, "bits");
      LHS_Bits  : constant Value_T := Const_Int
        (LHS_Type,
         unsigned_long_long (Get_Int_Type_Width (LHS_Type)),
         Sign_Extend => False);

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
            return Get_Undef (Create_Type (Env, Full_Etype (Node)));
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
              NSW_Sub (Env.Bld, LHS_Bits, N, "lower-shift");
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
            then Shl (Env.Bld, LHS, N, "")
            else
              (if Arithmetic
               then A_Shr (Env.Bld, LHS, N, "")
               else L_Shr (Env.Bld, LHS, N, "")));

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

   ------------------
   -- Get_Label_BB --
   ------------------

   function Get_Label_BB (Env : Environ; E : Entity_Id) return Basic_Block_T is
      BB : Basic_Block_T := Get_Basic_Block (Env, E);
   begin
      if BB = No_BB_T then
         BB := Create_Basic_Block (Env, Get_Name (E));
         Set_Basic_Block (Env, E, BB);
      end if;

      return BB;
   end Get_Label_BB;

   -------------------------------
   -- Node_Enclosing_Subprogram --
   -------------------------------

   function Node_Enclosing_Subprogram (Node : Node_Id) return Node_Id is
      N : Node_Id := Node;
   begin
      while Present (N) loop
         if Nkind (N) = N_Subprogram_Body then
            return Defining_Unit_Name (Specification (N));
         end if;

         N := Atree.Parent (N);
      end loop;

      return N;
   end Node_Enclosing_Subprogram;

end GNATLLVM.Compile;
