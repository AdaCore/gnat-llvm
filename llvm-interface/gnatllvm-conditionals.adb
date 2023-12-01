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

with Exp_Util; use Exp_Util;
with Table;    use Table;

with GNATLLVM.Arrays;       use GNATLLVM.Arrays;
with GNATLLVM.Blocks;       use GNATLLVM.Blocks;
with GNATLLVM.Builtins;     use GNATLLVM.Builtins;
with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.DebugInfo;    use GNATLLVM.DebugInfo;
with GNATLLVM.Conversions;  use GNATLLVM.Conversions;
with GNATLLVM.Instructions; use GNATLLVM.Instructions;
with GNATLLVM.Subprograms;  use GNATLLVM.Subprograms;
with GNATLLVM.Utils;        use GNATLLVM.Utils;

package body GNATLLVM.Conditionals is

   ----------------------------
   -- Build_Short_Circuit_Op --
   ----------------------------

   function Build_Short_Circuit_Op
     (Left, Right : N_Subexpr_Id; Op : Node_Kind) return GL_Value
   is
      And_Op               : constant Boolean       :=
        Op in N_And_Then | N_Op_And;
      --  Whether this is an AND or OR operation

      LHS, RHS             : GL_Value;
      --  We start evaluating the LHS in the current block, but we need to
      --  record which block it completes in, since it may not be the
      --  same block.

      Block_Left_Expr_End  : Basic_Block_T;
      --  Block which contains the evaluation of the right part
      --  expression of the operator and its end.

      Block_Right_Expr     : constant Basic_Block_T :=
        Create_Basic_Block ("scl.right.expr");
      Block_Right_Expr_End : Basic_Block_T;
      --  Block containing the exit code (the phi that selects that value)

      Block_Exit           : constant Basic_Block_T :=
        Create_Basic_Block ("scl.exit");

   begin
      --  In the case of And, evaluate the right expression when Left is
      --  true. In the case of Or, evaluate it when Left is false.
      --  Supress constant folding here because we need to be sure we have
      --  the expected predecessors to the block with the Phi.

      LHS := Get (Emit (Left), Boolean_Data);
      Block_Left_Expr_End := Get_Insert_Block;
      Build_Cond_Br (LHS,
                     (if And_Op then Block_Right_Expr else Block_Exit),
                     (if And_Op then Block_Exit       else Block_Right_Expr),
                     Optimize => False);

      --  Emit code for the evaluation of the right part expression

      Position_Builder_At_End (Block_Right_Expr);
      RHS := Get (Emit (Right), Boolean_Data);

      Block_Right_Expr_End := Get_Insert_Block;
      Move_To_BB (Block_Exit);

      --  If we exited the entry block, it means that for AND, the result
      --  is false and for OR, it's true. Otherwise, the result is the right.

      return Build_Phi
        ((1 => (if And_Op then Const_False else Const_True), 2 => RHS),
         (1 => Block_Left_Expr_End, 2 => Block_Right_Expr_End));
   end Build_Short_Circuit_Op;

   ---------------------
   -- Emit_Comparison --
   ---------------------

   function Emit_Comparison
     (Kind : Node_Kind; LHS, RHS : N_Subexpr_Id) return GL_Value
   is
      Operation : constant Pred_Mapping := Get_Preds (Kind);
      GT        : constant GL_Type      := Full_GL_Type (LHS);
      BT        : constant GL_Type      := Base_GL_Type (GT);

   begin
      --  If we're just elaborating decls, return undef

      if Decls_Only then
         return Get (Get_Undef (Boolean_GL_Type), Boolean_Data);

      --  LLVM treats pointers as integers regarding comparison. But we first
      --  have to see if the pointer has an activation record. If so,
      --  we just compare the functions, not the activation record.

      elsif Is_Access_Subprogram_Type (GT)
        and then not Has_Foreign_Convention (GT)
        and then Can_Use_Internal_Rep (GT)
      then
         declare
            LHS_Val : constant GL_Value := Subp_Ptr (LHS);
            RHS_Val : constant GL_Value := Subp_Ptr (RHS);

         begin
            return I_Cmp (Operation.Unsigned, LHS_Val, RHS_Val);
         end;

      elsif Is_Elementary_Type (GT) then
         declare
            LHS_Val : constant GL_Value := Emit_Convert_Value (LHS, BT);
            RHS_Val : constant GL_Value := Emit_Convert_Value (RHS, BT);

         begin
            return Build_Elementary_Comparison (Kind, LHS_Val, RHS_Val);
         end;

      --  We'll see some simple record comparisons, typically if they're
      --  Equivalent_Types of, e.g., an E_Access_Protected_Subprogram_Type.

      elsif Is_Record_Type (GT) then
         declare
            --  Now we need to get the size of the record (in bytes) to do
            --  the memory comparison. Memcmp is defined as returning zero
            --  for a zero size, so we don't need to worry about testing
            --  for that case, but handle the case of a constant 0.

            LHS_Val : constant GL_Value := To_Primitive (Emit_LValue (LHS));
            RHS_Val : constant GL_Value := To_Primitive (Emit_LValue (RHS));
            Size    : constant GL_Value :=
              Compute_Size (Related_Type (LHS_Val), Related_Type (RHS_Val),
                            LHS_Val, RHS_Val);
            Memcmp  : constant GL_Value :=
              (if   Is_Const_Int_Value (Size, 0)
               then Const_Null (Integer_GL_Type)
               else Call (Get_Memory_Compare_Fn,
                          (1 => Pointer_Cast (LHS_Val, A_Char_GL_Type),
                           2 => Pointer_Cast (RHS_Val, A_Char_GL_Type),
                           3 => To_Bytes (Size))));
         begin
            return I_Cmp (Operation.Signed, Memcmp,
                          Const_Null (Integer_GL_Type));
         end;
      else
         pragma Assert (Is_Array_Type (GT)
                          and then Operation.Signed in Int_EQ | Int_NE);
         --  The front end expands record type comparisons and array
         --  comparisons for other than equality.

         --  We handle this case by creating two basic blocks and doing
         --  this as a test of the arrays, branching to each if true or false.
         --  We then have a merge block which has a Phi selecting true
         --  or false.

         declare
            False_Val    : constant GL_Value      := Const_False;
            True_Val     : constant GL_Value      := Const_True;
            BB_True      : constant Basic_Block_T := Create_Basic_Block;
            BB_False     : constant Basic_Block_T := Create_Basic_Block;
            BB_Merge     : constant Basic_Block_T :=
              Create_Basic_Block ("MERGE");
            Results      : constant GL_Value_Array (1 .. 2)    :=
              (1 => (if Kind = N_Op_Eq then True_Val  else False_Val),
               2 => (if Kind = N_Op_Eq then False_Val else True_Val));
            Basic_Blocks : constant Basic_Block_Array (1 .. 2) :=
              (1 => BB_True, 2 => BB_False);

         begin
            --  First emit the comparison and branch to one of the two
            --  blocks.

            Emit_Comparison_And_Branch (N_Op_Eq, LHS, RHS, BB_True, BB_False);

            --  Now have each block branch to the merge point and create the
            --  Phi at the merge point.

            Position_Builder_At_End (BB_True);
            Build_Br (BB_Merge);
            Position_Builder_At_End (BB_False);
            Move_To_BB (BB_Merge);
            return Build_Phi (Results, Basic_Blocks);
         end;

      end if;
   end Emit_Comparison;

   --------------------------------
   -- Emit_Comparison_And_Branch --
   --------------------------------

   procedure Emit_Comparison_And_Branch
     (Kind              : Node_Kind;
      LHS, RHS          : N_Subexpr_Id;
      BB_True, BB_False : Basic_Block_T)
   is
      Cond : GL_Value;

   begin
      --  If we're just elaborating decls, elaborate both types and
      --  we're done.

      if Decls_Only then
         Discard (Full_GL_Type (LHS));
         Discard (Full_GL_Type (RHS));
         return;

      --  Do the array case here, where we have labels, to simplify the
      --  logic and take advantage of the reality that almost all array
      --  comparisons are part of "if" statements.

      elsif  Is_Array_Type (Full_Etype (LHS)) then
         pragma Assert (Kind in N_Op_Eq | N_Op_Ne);
         pragma Assert (Number_Dimensions (Full_Etype (LHS)) =
                          Number_Dimensions (Full_Etype (RHS)));

         declare
            Last_Dim       : constant Nat           :=
              Number_Dimensions (Full_Etype (LHS)) - 1;
            LHS_Complexity : constant Nat           :=
              Get_Array_Size_Complexity (Full_Etype (LHS));
            RHS_Complexity : constant Nat           :=
              Get_Array_Size_Complexity (Full_Etype (LHS));
            Our_LHS        : constant N_Subexpr_Id  :=
              (if LHS_Complexity > RHS_Complexity then LHS else RHS);
            --  To simplify the code below, we arrange things so that the
            --  array with the most complex size is on the LHS.

            Our_RHS        : constant N_Subexpr_Id  :=
              (if LHS_Complexity > RHS_Complexity then RHS else LHS);
            BB_T           : constant Basic_Block_T :=
              (if Kind = N_Op_Eq then BB_True else BB_False);
            BB_F           : constant Basic_Block_T :=
              (if Kind = N_Op_Eq then BB_False else BB_True);
            LHS_Val        : constant GL_Value      :=
              To_Primitive (Emit_LValue (Our_LHS));
            RHS_Val        : constant GL_Value      :=
              To_Primitive (Emit_LValue (Our_RHS));
            BB_Next        : Basic_Block_T;
            LHS_Lengths    : GL_Value_Array (0 .. Last_Dim);
            RHS_Lengths    : GL_Value_Array (0 .. Last_Dim);

         begin
            --  There's an obscure case where if both arrays have a
            --  dimension that's zero (whether or not it's the same
            --  dimension in both), the arrays compare true. This is
            --  tested in C45264A. If this is a single-dimensional array,
            --  this falls through from the normal computation, but for
            --  multi-dimensional arrays, we have to actually do the test.

            --  Start by getting all the lengths

            for Dim in 0 .. Last_Dim loop
               LHS_Lengths (Dim) :=
                 Get_Array_Length (Full_Etype (Our_LHS), Dim, LHS_Val);
               RHS_Lengths (Dim) :=
                 Get_Array_Length (Full_Etype (Our_RHS), Dim, RHS_Val);
            end loop;

            if Last_Dim /= 0 then

               --  RHS is the least complex. So check its dimensions.
               --  If any are zero, we need to check LHS. If none are zero
               --  (and hopefully we'll know this at compile-time), we
               --  don't need to check LHS and can go to the next test.

               declare
                  BB_RHS_Has_Zero_Dim : constant Basic_Block_T :=
                    Create_Basic_Block ("rhs.has.0.dim");
                  BB_Continue         : constant Basic_Block_T :=
                    Create_Basic_Block ("normal.tests");

               begin
                  for Dim in 0 .. Last_Dim loop
                     BB_Next :=
                       (if Dim = Last_Dim then BB_Continue
                        else Create_Basic_Block);
                     Cond := Build_Elementary_Comparison
                       (N_Op_Eq, RHS_Lengths (Dim),
                        Const_Null (RHS_Lengths (Dim)));
                     Build_Cond_Br (Cond, BB_RHS_Has_Zero_Dim, BB_Next);
                     Position_Builder_At_End (BB_Next);
                  end loop;

                  --  Now go to where we know that RHS has a zero dimension
                  --  and see if LHS does as well.

                  Position_Builder_At_End (BB_RHS_Has_Zero_Dim);
                  for Dim in 0 .. Last_Dim loop
                     BB_Next :=
                       (if Dim = Last_Dim then BB_Continue
                        else Create_Basic_Block);
                     Cond    := Build_Elementary_Comparison
                       (N_Op_Eq, LHS_Lengths (Dim),
                        Const_Null (LHS_Lengths (Dim)));
                     Build_Cond_Br (Cond, BB_T, BB_Next);
                     Position_Builder_At_End (BB_Next);
                  end loop;

                  Position_Builder_At_End (BB_Continue);
               end;
            end if;

            --  For each dimension, see if the lengths of the two arrays
            --  are different. If so, the comparison is false.

            --  We need to be careful with types here: LHS and RHS are
            --  the actual array types, but, because we called Emit_LValue,
            --  LHS_Val and RHS_Val are actually references to the array,
            --  not the array.

            for Dim in 0 .. Number_Dimensions (Full_Etype (LHS)) - 1 loop
               BB_Next := Create_Basic_Block;
               Cond    := Build_Elementary_Comparison
                 (N_Op_Eq, LHS_Lengths (Dim), RHS_Lengths (Dim));
               Build_Cond_Br (Cond, BB_Next, BB_F);
               Position_Builder_At_End (BB_Next);
            end loop;

            declare

               --  Now we need to get the size of the array (in bytes)
               --  to do the memory comparison. Memcmp is defined as
               --  returning zero for a zero size, so we don't need to worry
               --  about testing for that case, but do check for constant 0.

               Size   : constant GL_Value :=
                 Compute_Size (Related_Type (LHS_Val), Related_Type (RHS_Val),
                               LHS_Val, RHS_Val);
               Memcmp : constant GL_Value :=
                 (if   Is_Const_Int_Value (Size, 0)
                  then Const_Null (Integer_GL_Type)
                  else Call (Get_Memory_Compare_Fn,
                             (1 => Pointer_Cast (Get (LHS_Val, Reference),
                                                 A_Char_GL_Type),
                              2 => Pointer_Cast (Get (RHS_Val, Reference),
                                                 A_Char_GL_Type),
                              3 => To_Bytes (Size))));
               Cond   : constant GL_Value :=
                 I_Cmp (Int_EQ, Memcmp, Const_Null (Integer_GL_Type));

            begin
               Build_Cond_Br (Cond, BB_T, BB_F);
            end;
         end;

      --  And now we have the other cases. We do have to be careful in how
      --  the tests work that we don't have infinite mutual recursion.

      else
         Cond := Emit_Comparison (Kind, LHS, RHS);
         Build_Cond_Br (Cond, BB_True, BB_False);
      end if;
   end Emit_Comparison_And_Branch;

   ---------------------------------
   -- Build_Elementary_Comparison --
   ---------------------------------

   function Build_Elementary_Comparison
     (Kind               : Node_Kind;
      Orig_LHS, Orig_RHS : GL_Value) return GL_Value
   is
      Operation    : constant Pred_Mapping := Get_Preds (Kind);
      LHS_GT       : constant GL_Type      := Related_Type (Orig_LHS);
      RHS_GT       : constant GL_Type      := Related_Type (Orig_RHS);
      LHS          : GL_Value              := Orig_LHS;
      RHS          : GL_Value              := Orig_RHS;

   begin
      --  If a scalar type (meaning both must be), convert each operand to
      --  its base type.

      if Is_Scalar_Type (LHS) then
         LHS := Convert (LHS, Base_GL_Type (LHS));
         RHS := Convert (RHS, Base_GL_Type (RHS));
      end if;

      --  If we're comparing two access types, first get the values as
      --  references to the designated types, then as a single-word
      --  reference. To be a valid comparison, they must be the same LLVM
      --  type at that point.

      if Is_Access_Type (LHS) then
         LHS := Get (From_Access (LHS), Reference_For_Integer);
         RHS := Get (From_Access (RHS), Reference_For_Integer);

         --  Now we have simple pointers, but they may not be the same LLVM
         --  type. If they aren't, convert the RHS to the type of the LHS.

         if Type_Of (LHS) /= Type_Of (RHS) then
            RHS := Pointer_Cast (RHS, LHS);
         end if;

         return I_Cmp (Operation.Unsigned, LHS, RHS);

      elsif Is_Floating_Point_Type (LHS) then
         return F_Cmp (Operation.Real, LHS, RHS);
      else
         --  The only case left is integer or normal access type.

         pragma Assert (Is_Discrete_Or_Fixed_Point_Type (LHS)
                          or else Is_Access_Type (LHS));

         --  At this point, if LHS is an access type, then RHS is too and
         --  we know the aren't pointers to unconstrained arrays. It's
         --  possible that the two pointer types aren't the same, however.
         --  So in that case, convert one to the pointer of the other.

         if Is_Access_Type (LHS) and then Type_Of (RHS) /= Type_Of (LHS) then
            RHS := Pointer_Cast (RHS, LHS);
         end if;

         --  Now just do the normal comparison, but be sure to get the
         --  signedness from the original type, not the base type, unless
         --  the two inputs are of different types.

         return I_Cmp
           ((if   (Is_Unsigned_Type (Orig_LHS) and then LHS_GT = RHS_GT)
                  or else Is_Access_Type (LHS)
             then Operation.Unsigned else Operation.Signed),
            LHS, RHS);

      end if;
   end Build_Elementary_Comparison;

   ---------------------
   -- Emit_And_Or_Xor --
   ---------------------

   function Emit_And_Or_Xor
     (Kind : Node_Kind; LHS_Node, RHS_Node : N_Subexpr_Id) return GL_Value
   is
      BT  : constant GL_Type := Base_GL_Type (Full_GL_Type (LHS_Node));
      LHS : GL_Value         := Emit (LHS_Node);
      RHS : GL_Value         := Emit (RHS_Node);

   begin
      --  If both are Boolean_Data, we can compute our result in that
      --  relationship. Otherwise, force to Data.

      if Relationship (LHS) /= Boolean_Data
        or else Relationship (RHS) /= Boolean_Data
      then
         LHS := Convert (Get (LHS, Data), BT);
         RHS := Convert (Get (RHS, Data), BT);
      end if;

      if Kind = N_Op_And then
         return Build_And (LHS, RHS);
      elsif Kind = N_Op_Or then
         return Build_Or (LHS, RHS);
      else  --  Kind = N_Op_Xor
         return Build_Xor (LHS, RHS);
      end if;
   end Emit_And_Or_Xor;

   -------------------------
   -- Emit_Case_Statement --
   -------------------------

   procedure Emit_Case_Statement (N : N_Case_Statement_Id) is

      Alts        : constant List_Id       := Alternatives (N);
      Start_BB    : constant Basic_Block_T := Get_Insert_Block;
      BB_End      : constant Basic_Block_T := Create_Basic_Block;
      Alt         : Opt_N_Alternative_Id   := First_Non_Pragma (Alts);
      Current_Alt : Nat                                 := 1;
      BBs         : Basic_Block_Array (1 .. List_Length_Non_Pragma (Alts));

   begin
      --  First emit the code for each alternative and add its BB

      while Present (Alt) loop
         BBs (Current_Alt) := Create_Basic_Block;
         Position_Builder_At_End (BBs (Current_Alt));
         Emit (Statements (Alt));
         Build_Br (BB_End);
         Current_Alt := Current_Alt + 1;
         Next_Non_Pragma (Alt);
      end loop;

      --  Now go back into our block, generate the statements to make the
      --  choice, and position at the exit point of the statement.

      Position_Builder_At_End (Start_BB);
      Emit_Case_Code (Alts, Emit_Expression (Expression (N)), BBs);
      Position_Builder_At_End (BB_End);
   end Emit_Case_Statement;

   --------------------
   -- Emit_Case_Code --
   --------------------

   procedure Emit_Case_Code
     (In_Alts : List_Id; LHS : GL_Value; In_BBs : Basic_Block_Array)
   is
      function Count_Choices (Alts : List_Id) return Nat;
      --  Count the total number of choices in this case part

      procedure Swap_Highest_Cost (Is_Switch : Boolean);
      --  Move the highest-cost alternative to the last entry. Is_Switch
      --  says whether we look at the switch cost or the if cost.

      -------------------
      -- Count_Choices --
      -------------------

      function Count_Choices (Alts : List_Id) return Nat is
         Alt          : Opt_N_Alternative_Id := First_Non_Pragma (Alts);
         First_Choice : Opt_N_Is_Case_Choice_Id;

      begin
         return Num_Choices : Nat := 0 do
            while Present (Alt) loop

               --  We have a peculiarity in the "others" case of a case
               --  statement. The Alternative points to a list of choices
               --  of which the first choice is an N_Others_Choice. So
               --  handle that specially both here and when we compute our
               --  Choices below.

               First_Choice := First (Discrete_Choices (Alt));
               Num_Choices  := Num_Choices +
                 (if   Nkind (First_Choice) = N_Others_Choice
                  then List_Length (Others_Discrete_Choices (First_Choice))
                  else List_Length (Discrete_Choices (Alt)));
               Next_Non_Pragma (Alt);
            end loop;
         end return;

      end Count_Choices;

      --  We have data structures to record information about each choice
      --  and each alternative in the case statement. For each choice, we
      --  record the bounds and costs. The "if" cost is one if both bounds
      --  are the same, otherwise two. The "switch" cost is the size of the
      --  range, if known and fits in an integer, otherwise a large number
      --  (we arbitrary use 1000). For the alternative, we record the basic
      --  block in which we've emitted the relevant code, the basic block
      --  we'll use for the test (in the "if" case), the first and last
      --  choice, and the total costs for all the choices in this
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

      Max_Cost       : constant Nat      := 10_000;
      Num_Alts       : constant Nat      := List_Length_Non_Pragma (In_Alts);
      Typ            : constant Type_T   := Type_Of (Related_Type (LHS));
      First_Choice   : Nat               := 1;
      Current_Alt    : Nat               := 1;
      Current_Choice : Nat               := 1;
      Alts           : array (1 .. Num_Alts) of One_Alt;
      Choices        : array (1 .. Count_Choices (In_Alts)) of One_Choice;
      BB             : Basic_Block_T;
      Alt            : Opt_N_Alternative_Id;
      Choice         : Opt_N_Is_Case_Choice_Id;
      Low, High      : Uint;
      If_Cost        : Nat;
      Switch_Cost    : Nat;
      Switch         : Value_T;

      -----------------------
      -- Swap_Highest_Cost --
      -----------------------

      procedure Swap_Highest_Cost (Is_Switch : Boolean) is
         Temp_Alt   : One_Alt;
         Worst_Alt  : Nat;
         Worst_Cost : Nat;
         Our_Cost   : Nat;

      begin
         Worst_Alt  := Alts'Last;
         Worst_Cost := 0;

         for J in Alts'Range loop
            Our_Cost := (if   Is_Switch then Alts (J).Switch_Cost
                         else Alts (J).If_Cost);

            if Our_Cost > Worst_Cost then
               Worst_Cost := Our_Cost;
               Worst_Alt  := J;
            end if;
         end loop;

         Temp_Alt         := Alts (Alts'Last);
         Alts (Alts'Last) := Alts (Worst_Alt);
         Alts (Worst_Alt) := Temp_Alt;
      end Swap_Highest_Cost;

   begin
      --  If we're just elaborating decls, we may have junk for choices,
      --  so don't do anything.

      if Decls_Only then
         return;
      end if;

      --  First we scan all the alternatives and choices and fill in most
      --  of the data.

      Alt := First_Non_Pragma (In_Alts);
      while Present (Alt) loop
         First_Choice := Current_Choice;
         Choice       := First (Discrete_Choices (Alt));

         if Nkind (Choice) = N_Others_Choice then
            Choice    := First (Others_Discrete_Choices (Choice));
         end if;

         while Present (Choice) loop
            Decode_Range (Choice, Low, High);

            --  When we compute the cost, set the cost of a null range
            --  to zero. If the if cost is 0 or 1, that's the switch cost too,
            --  but if either of the bounds aren't in Int, we can't use
            --  switch at all.

            If_Cost := (if Low > High then 0 elsif Low = High then 1 else 2);
            Switch_Cost := (if    not UI_Is_In_Int_Range (Low)
                                  or else not UI_Is_In_Int_Range (High)
                            then  Max_Cost
                            elsif If_Cost <= 1 then If_Cost
                            else  Range_Length (Low, High, Max_Cost));

            Choices (Current_Choice) := (Low => Low, High => High,
                                         If_Cost => If_Cost,
                                         Switch_Cost => Switch_Cost);
            Current_Choice := Current_Choice + 1;
            Next (Choice);
         end loop;

         If_Cost     := 0;
         Switch_Cost := 0;

         --  Sum up the costs of all the choices in this alternative

         for J in First_Choice .. Current_Choice - 1 loop
            If_Cost := If_Cost + Choices (J).If_Cost;
            Switch_Cost := Switch_Cost + Choices (J).Switch_Cost;
         end loop;

         --  If this alternative has an N_Others_Choice, there is a
         --  possibility it may not have any Others_Discrete_Choices (e.g.,
         --  if it was added by the front end). If so, it means we don't
         --  have the list of its choices, so it must remain the default.

         if Nkind (First (Discrete_Choices (Alt))) = N_Others_Choice
           and then No (Others_Discrete_Choices
                          (First (Discrete_Choices (Alt))))
         then
            If_Cost     := Max_Cost * Max_Cost;
            Switch_Cost := Max_Cost * Max_Cost;
         end if;

         Alts (Current_Alt) := (BB           => In_BBs (Current_Alt),
                                First_Choice => First_Choice,
                                Last_Choice  => Current_Choice - 1,
                                If_Cost      => If_Cost,
                                Switch_Cost  => Switch_Cost);
         Current_Alt := Current_Alt + 1;
         Next_Non_Pragma (Alt);
      end loop;

      --  We have two strategies: we can use an LLVM switch instruction if
      --  there aren't too many choices. If not, we use "if". First we
      --  find the alternative with the largest switch cost and make that
      --  the "others" option. Then we see if the total cost of the remaining
      --  alternatives is low enough (we use 100). If so, use that approach.

      Swap_Highest_Cost (True);
      Switch_Cost := 0;

      for J in Alts'First .. Alts'Last - 1 loop
         Switch_Cost := Switch_Cost + Alts (J).Switch_Cost;
      end loop;

      if Switch_Cost < 100 then

         --  First we emit the actual "switch" statement, then we add
         --  the cases to it. Here we collect all the basic blocks.

         declare
            BBs : Basic_Block_Array (Alts'Range);

         begin
            for J in BBs'Range loop
               BBs (J) := Alts (J).BB;
            end loop;

            Switch := Build_Switch (LHS, BBs (BBs'Last), BBs'Length);

            for J in Alts'First .. Alts'Last - 1 loop
               for K in Alts (J).First_Choice .. Alts (J).Last_Choice loop
                  for L in +Choices (K).Low .. +Choices (K).High loop
                     Add_Case (Switch,
                               Const_Int (Typ, ULL (L), Sign_Extend => True),
                               Alts (J).BB);
                  end loop;
               end loop;
            end loop;
         end;

      else
         --  Otherwise, we generate if/elsif/elsif/else

         Swap_Highest_Cost (False);
         for J in Alts'First .. Alts'Last - 1 loop
            for K in Alts (J).First_Choice .. Alts (J).Last_Choice loop

               --  Only do something if this is not a null range

               if Choices (K).If_Cost /= 0 then

                  --  If we're processing the very last choice, then
                  --  if the choice is not a match, we go to "others".
                  --  Otherwise, we go to a new basic block that's the
                  --  next choice. Note that we can't simply test
                  --  against Choices'Last because we may have swapped
                  --  some other alternative with Alts'Last.

                  if J = Alts'Last - 1 and then K = Alts (J).Last_Choice then
                     BB := Alts (Alts'Last).BB;
                  else
                     BB := Create_Basic_Block;
                  end if;

                  Build_If_Range (LHS, Choices (K).Low, Choices (K).High,
                                  Alts (J).BB, BB);
                  Position_Builder_At_End (BB);
               end if;
            end loop;
         end loop;
      end if;
   end Emit_Case_Code;

   -------------
   -- Emit_If --
   -------------

   procedure Emit_If (N : N_If_Statement_Id) is

      --  Record information about each part of an "if" statement

      type If_Ent is record
         Cond     : N_Subexpr_Id;
         --  Expression to test

         Stmts    : List_Id;
         --  Statements to emit if true

         BB_True  : Basic_Block_T;
         --  Basic block to branch for true

         BB_False : Basic_Block_T;
         --  Basic block to branch for false
      end record;

      Elseif_Count : constant Nat :=
        (if   Present (Elsif_Parts (N)) then List_Length (Elsif_Parts (N))
         else 0);
      If_Parts_Pos : Nat          := 1;
      If_Parts     : array (0 .. Elseif_Count) of If_Ent;
      BB_End       : Basic_Block_T;
      Elsif_Part   : Opt_N_Elsif_Part_Id;

   begin
      --  First go through all the parts of the "if" statement recording
      --  the expressions and statements.

      If_Parts (0) := (Cond     => Condition (N),
                       Stmts    => Then_Statements (N),
                       BB_True  => Create_Basic_Block,
                       BB_False => Create_Basic_Block);

      if Present (Elsif_Parts (N)) then
         Elsif_Part := First (Elsif_Parts (N));
         while Present (Elsif_Part) loop
            If_Parts (If_Parts_Pos) :=
              (Cond     => Condition (Elsif_Part),
               Stmts    => Then_Statements (Elsif_Part),
               BB_True  => Create_Basic_Block,
               BB_False => Create_Basic_Block);
            If_Parts_Pos := If_Parts_Pos + 1;
            Next (Elsif_Part);
         end loop;
      end if;

      --  When done, each part goes to the end of the statement. If there's
      --  an "else" clause, it's a new basic block and the end; otherwise,
      --  it's the last False block.

      BB_End := (if   Present (Else_Statements (N)) then Create_Basic_Block
                 else If_Parts (If_Parts_Pos - 1).BB_False);

      --  Now process each entry that we made: test the condition and branch;
      --  emit the statements in the appropriate block; branch to the end;
      --  and set up the block for the next test, the "else", or next
      --  statement.

      for Part of If_Parts loop
         Emit_If_Cond (Part.Cond, Part.BB_True, Part.BB_False);
         Position_Builder_At_End (Part.BB_True);
         Emit (Part.Stmts);
         Build_Br (BB_End);
         Position_Builder_At_End (Part.BB_False);
      end loop;

      --  If there's an Else part, emit it and go into the "end" basic block

      if Present (Else_Statements (N)) then
         Emit (Else_Statements (N));
         Move_To_BB (BB_End);
      end if;

   end Emit_If;

   ---------------------------
   -- Is_Simple_Conditional --
   ---------------------------

   function Is_Simple_Conditional (N : N_Subexpr_Id) return Boolean is
   begin
      case Nkind (N) is
         when N_Op_Compare =>
            return Is_Elementary_Type (Full_Etype (Left_Opnd (N)));

         when N_Op_And | N_Op_Or | N_And_Then | N_Or_Else =>
            return (Is_Simple_Conditional (Left_Opnd (N))
                      and then Is_Simple_Conditional (Right_Opnd (N)));

         when N_Op_Xor | N_Op_Not =>
            return Is_Simple_Conditional (Right_Opnd (N));

         when N_In | N_Not_In =>

            --  ??? These could be done using the trick that maps signed
            --  range comparisons to unsigned comparisons, but not worth
            --  the trouble.

            return False;

         when others =>
            return True;
      end case;
   end Is_Simple_Conditional;

   ------------------
   -- Emit_If_Cond --
   ------------------

   procedure Emit_If_Cond (N : N_Subexpr_Id; BB_True, BB_False : Basic_Block_T)
   is
      And_Op : constant Boolean := Nkind (N) in N_And_Then | N_Op_And;
      BB_New : Basic_Block_T;
      Result : GL_Value;

   begin
      case Nkind (N) is

         --  Process operations that we can handle in terms of different branch
         --  mechanisms, such as short-circuit operators.

         when N_Expression_With_Actions => Expression_With_Actions : declare

            Has_All : Boolean;
            Expr    : constant Opt_N_Subexpr_Id :=
              Simple_Value_Action (N, Has_All);

         begin
            --  If this is just defining the value that is to be its result
            --  with no reference, just use that as the condition that we
            --  test. Otherwise, emit the actions and then test.

            if Present (Expr) and then not Has_All then
               Emit_If_Cond (Expr, BB_True, BB_False);
            else
               Emit (Actions (N));
               Emit_If_Cond (Expression (N), BB_True, BB_False);
            end if;

            return;

         end Expression_With_Actions;

         when N_Op_Not =>
            Emit_If_Cond (Right_Opnd (N), BB_False, BB_True);
            return;

         when N_And_Then | N_Or_Else | N_Op_And | N_Op_Or =>

            --  If this is not a short-circuit form, we can only do this
            --  as a short-circuit if there are no side-effects.

            if Nkind (N) in N_And_Then | N_Or_Else
              or else (Safe_For_Short_Circuit (Left_Opnd (N))
                         and then Safe_For_Short_Circuit (Right_Opnd (N)))
            then
               --  Depending on the result of the test of the left operand,
               --  we either go to a final basic block or to a new
               --  intermediate one where we test the right operand.

               BB_New := Create_Basic_Block ("short.circuit");
               Emit_If_Cond (Left_Opnd (N),
                             (if And_Op then BB_New else BB_True),
                             (if And_Op then BB_False else BB_New));
               Position_Builder_At_End (BB_New);
               Emit_If_Cond (Right_Opnd (N), BB_True, BB_False);
               return;
            end if;

         when N_Op_Compare =>
            Emit_Comparison_And_Branch (Nkind (N), Left_Opnd (N),
                                        Right_Opnd (N), BB_True, BB_False);
            return;

         when N_In | N_Not_In => In_Not_In : declare

            Low, High : Uint;

         begin
            --  If we can decode the range into Uint's, we can just do
            --  simple comparisons.
            --
            --  If we're just elaborating decls, this may not be
            --  complete at this point, so don't try to do anything.

            if Decls_Only then
               return;
            end if;

            pragma Assert (No (Alternatives (N)));
            Decode_Range (Right_Opnd (N), Low, High);

            if Present (Low) and then Present (High) then
               Build_If_Range
                 (Emit_Expression (Left_Opnd (N)), Low, High,
                  (if Nkind (N) = N_In then BB_True  else BB_False),
                    (if Nkind (N) = N_In then BB_False else BB_True));
               return;
            end if;
         end In_Not_In;

         when others =>
            null;

      end case;

      --  If we haven't handled it via one of the special cases above, just
      --  evaluate the expression and do the branch, depending on whether
      --  the result if zero or nonzero, unless we already have an i1
      --  (Boolean_Data relationship). We must have a boolean type here.

      pragma Assert (Is_Boolean_Type (Full_Etype (N)));
      Result := Emit (N);

      if Relationship (Result) /= Boolean_Data then
         Result := To_Primitive (Get (Result, Data));
         Result := I_Cmp (Int_NE, Result, Const_Null (Result));
      end if;

      Build_Cond_Br (Result, BB_True, BB_False);

   end Emit_If_Cond;

   --------------------
   -- Build_If_Range --
   --------------------

   procedure Build_If_Range
     (LHS               : GL_Value;
      Low, High         : Uint;
      BB_True, BB_False : Basic_Block_T)
   is
      LHS_BT   : constant GL_Type := Base_GL_Type (LHS);
      LHS_Base : GL_Value;
      Cond     : GL_Value;
      Inner_BB : Basic_Block_T;

   begin
      --  For discrete types (all we handle here), handle ranges by testing
      --  against the high and the low and branching as appropriate. We
      --  must be sure to evaluate the LHS only once. But first check for
      --  a range of size one since that's only one comparison. If we are
      --  comparing against a range, be sure to do the comparison in the
      --  base type in case the subtype is unsigned and the base type isn't.

      if Low = High then
         Cond := Build_Elementary_Comparison
           (N_Op_Eq, LHS, Const_Int (LHS, Low));
         Build_Cond_Br (Cond, BB_True, BB_False);
      else
         Inner_BB := Create_Basic_Block ("range.test");
         LHS_Base := Convert (LHS, LHS_BT);
         Cond     := Build_Elementary_Comparison (N_Op_Ge, LHS_Base,
                                                  Const_Int (LHS_BT, Low));
         Build_Cond_Br (Cond, Inner_BB, BB_False);
         Position_Builder_At_End (Inner_BB);
         Cond := Build_Elementary_Comparison (N_Op_Le, LHS_Base,
                                              Const_Int (LHS_BT, High));
         Build_Cond_Br (Cond, BB_True, BB_False);
      end if;
   end Build_If_Range;

   ------------------------
   -- Emit_If_Expression --
   ------------------------

   function Emit_If_Expression
     (N : N_If_Expression_Id; LHS : GL_Value) return GL_Value
   is
      function Need_Ref_To_Convert
        (From_GT, To_GT : GL_Type; V : GL_Value) return Boolean
        with Pre  => Present (From_GT) and then Present (To_GT);
      --  Return True iff we need to use a reference to convert

      --  There can be a nest if N_If_Expression nodes that correspond
      --  to a single (if ... then ... elsif ... then ... else) statement.
      --  We record information about each executable part, with the last
      --  one being the final 'else'.

      type I_E_Part is record
         Condition : Opt_N_Subexpr_Id;
         --  Condition to test, or Empty for the else part

         Expr      : N_Subexpr_Id;
         --  Expression to evaluate for this part

         BB        : Basic_Block_T;
         --  Basic block containing the code so far for this part

         Value     : GL_Value;
         --  The latest representation of the value computed by this part
      end record;

      package Parts is new Table.Table
        (Table_Component_Type => I_E_Part,
         Table_Index_Type     => Nat,
         Table_Low_Bound      => 1,
         Table_Initial        => 3,
         Table_Increment      => 1,
         Table_Name           => "IE_Table");

      Expr          : N_Subexpr_Id           := N;
      Expr_GT       : constant GL_Type       := Full_GL_Type (N);
      Elementary    : constant Boolean       := Is_Elementary_Type (Expr_GT);
      Phi_BB        : constant Basic_Block_T := Create_Basic_Block ("i.e.phi");
      BB            : Basic_Block_T          := Get_Insert_Block;
      Phi_GT        : GL_Type                := No_GL_Type;
      All_Ref       : Boolean                := True;
      Some_Ref      : Boolean                := False;
      Need_Convert  : Boolean                := False;
      Fat_R         : GL_Relationship        := Invalid;
      All_R         : GL_Relationship        := Object;
      All_As_Data_R : GL_Relationship        := Object;
      Phi_R         : GL_Relationship;
      Need_Ref      : Boolean;
      Prefer_Ref    : Boolean;
      Use_Ref       : Boolean;

      -------------------------
      -- Need_Ref_To_Convert --
      -------------------------

      function Need_Ref_To_Convert
        (From_GT, To_GT : GL_Type; V : GL_Value) return Boolean is
      begin
         --  If the two types are the same or if they are elementary (we know
         --  that if one is, both must be), we don't need to use a reference.

         if From_GT = To_GT or else Elementary then
            return False;

         --  If we can convert constant data, we don't need a reference

         elsif Present (V)
           and then Can_Convert_Aggregate_Constant (V, To_GT)
         then
            return False;

         --  Otherwise we do

         else
            return True;
         end if;
      end Need_Ref_To_Convert;

   begin  --  Start of processing for Emit_If_Expression

      --  Start by building the table of parts. If we don't have any nested
      --  N_If_Expression, there are two parts: one for 'then' and one for
      --  'else'.

      loop
         Expr := First (Expressions (Expr));
         Parts.Append ((Expr, Next (Expr), BB, No_GL_Value));
         Expr := Next (Next (Expr));
         BB   := Create_Basic_Block;
         exit when Nkind (Expr) /= N_If_Expression;
      end loop;

      Parts.Append ((Empty, Expr, BB, No_GL_Value));

      --  Now do initial processing for each part while computing what our
      --  target type and relationship is.

      for J in 1 .. Parts.Last loop
         declare
            IEP     : I_E_Part renames Parts.Table (J);
            Next_BB : Basic_Block_T;
            GT      : GL_Type;
            R       : GL_Relationship;
            Data_R  : GL_Relationship;

         begin
            --  Start by generating the conditional branch and working on each
            --  expression in its own BB.

            Position_Builder_At_End (IEP.BB);

            if Present (IEP.Condition) then
               Next_BB := Create_Basic_Block;
               Emit_If_Cond (IEP.Condition, Next_BB, Parts.Table (J + 1).BB);
               Position_Builder_At_End (Next_BB);
            end if;

            IEP.Value := Emit (IEP.Expr, LHS => LHS);
            IEP.BB    := Get_Insert_Block;

            --  Get the type and relationship of the result of the expression
            --  and compute some things from them that we'll use below
            --  to see what form each part should be left in.

            GT            := Related_Type (IEP.Value);
            R             := Relationship (IEP.Value);
            Data_R        := (if Is_Reference (R) then Deref (R) else R);
            Phi_GT        := (if No (Phi_GT) then GT
                              elsif Phi_GT /= GT then Expr_GT else GT);
            All_Ref       := All_Ref  and then Is_Reference (R);
            Some_Ref      := Some_Ref or else  Is_Reference (R);
            Need_Convert  := Need_Convert or else GT /= Expr_GT;

            --  See if all relationships are the same. We've initialized
            --  to Object since that can't be an actual relationship.

            All_R         := (if    All_R = Object then R
                             elsif All_R = R then All_R else Invalid);
            All_As_Data_R := (if    All_As_Data_R = Object then Data_R
                              elsif All_As_Data_R = Data_R
                              then All_As_Data_R else Invalid);

            if R in Fat_Pointer | Fat_Reference_To_Subprogram then
               Fat_R      := R;
            end if;

         end;
      end loop;

      --  The front end guarantees that the types of the parts and the
      --  result are very similar types. However, they may not be
      --  identical and we need identical types for the Phi. If the types
      --  are the same, we can use that one and later convert to Expr_GT.
      --  Otherwise, we may as well convert both to Expr_GT instead of
      --  finding some intermediate type, especially because all three are
      --  usually the same. If the only conversion is after the Phi, we
      --  can use data up to then if all are data, but would prefer
      --  reference over data if we have one if each since we need
      --  reference for that conversion.

      --  We also need to have the Relationship be the same on all parts
      --  and that's more complex since there are a lot of cases.
      --
      --  Normally, data is preferred. But there are some cases where we
      --  can't use data. If we have to convert types, the types are
      --  composite and either we can't statically convert to the type or
      --  we need to convert a non-constant value, we can only do the
      --  conversion by pointer punning, so we need a reference.

      Need_Ref   := (for some J in 1 .. Parts.Last =>
                     Need_Ref_To_Convert (Related_Type (Parts.Table (J).Value),
                                          Phi_GT, Parts.Table (J).Value));
      Prefer_Ref := Need_Ref_To_Convert (Phi_GT, Expr_GT, No_GL_Value);

      --  We use a reference if we need to use a reference, if all parts
      --  are a reference, or if we prefer a reference and at least one
      --  part is a reference. However, we must use data if we have an
      --  elementary type and need to do a conversion.

      if Elementary and then Need_Convert then
         Use_Ref := False;
      else
         Use_Ref := Need_Ref or else All_Ref
           or else (Prefer_Ref and then Some_Ref);
      end if;

      --  Next choose the exact relationship for both the data and
      --  reference cases.

      if Use_Ref then

         --  If GT is an unconstrained array, use a fat pointer since the
         --  bounds may be different between the parts.

         if Is_Unconstrained_Array (Expr_GT) then
            Phi_R := Fat_Pointer;

         --  If all are the same and a reference, that's what we use

         elsif Is_Reference (All_R) then
            Phi_R := All_R;

         --  If any is a fat pointer of some type, that's our choice

         elsif Fat_R /= Invalid then
            Phi_R := Fat_R;

         --  Otherwise use Reference

         else
            Phi_R := Reference;
         end if;

      --  Otherwise, we have some type of data

      else
         --  If all data references are the same, use that one. Otherwise,
         --  use Data.

         Phi_R := (if All_As_Data_R /= Invalid then All_As_Data_R else Data);
      end if;

      --  Now we generate similar code for alll parts, to obtain a result
      --  with relationship Phi_R to Phi_GT.

      for J in 1 .. Parts.Last loop
         declare
            IEP    : I_E_Part renames Parts.Table (J);
            GT     : constant GL_Type := Related_Type (IEP.Value);
            Result : GL_Value         := IEP.Value;

         begin
            Position_Builder_At_End (IEP.BB);

            --  If we have data and need a reference, or vice versa, get it
            --  as the corresponding data or reference (making the minimal
            --  change).

            if Is_Data (Result) and then Is_Reference (Phi_R) then

               --  If we're going to be converting to a wider GT, do the
               --  conversion as data and then take a reference. Although
               --  this may cause us to store more data than we need, the
               --  alternative will generate invalid LLVM IR since we may
               --  end up reading outside of allocated memory.

               if not Is_Nonnative_Type (Phi_GT)
                 and then GT_Size (Phi_GT) > GT_Size (GT)
               then
                  Result := Convert_GT (Result, Phi_GT);
               end if;

               Result := Get (Result, Any_Reference);
            elsif Is_Data (Phi_R) and then Is_Double_Reference (Result) then
               Result := Get (Result, Deref (Deref (Relationship (Result))));
            elsif Is_Data (Phi_R) and then Is_Reference (Result) then
               Result := Get (Result, Deref (Relationship (Result)));
            end if;

            --  Now convert. First see if this is being converted to the same
            --  native LLVM type. If so, just change the GT.

            if not Is_Nonnative_Type (GT)
              and then not Is_Nonnative_Type (Phi_GT)
              and then Type_Of (GT) = Type_Of (Phi_GT)
            then
               Result := G_Is (Result, Phi_GT);

               --  Next see if this is a constant aggregate

            elsif Can_Convert_Aggregate_Constant (Result, Phi_GT) then
               Result := Convert_Aggregate_Constant (Result, Phi_GT);

               --  If elementary type, do that the conversion

            elsif Elementary then
               Result := Convert (Result, Phi_GT);

               --  Otherwise, we must have a reference. So convert that.

            else
               Result := Convert_Ref (Result, Phi_GT);
            end if;

            --  Finally, get it with the desired relationship and branch to
            --  the Phi.

            IEP.Value := Get (Result, Phi_R);
            IEP.BB    := Get_Insert_Block;
            Build_Br (Phi_BB);
         end;
      end loop;

      declare
         Result : GL_Value;
         Values : GL_Value_Array (1 .. Parts.Last);
         BBs    : Basic_Block_Array (1 .. Parts.Last);

      begin
         for J in 1 .. Parts.Last loop
            Values (J) := Parts.Table (J).Value;
            BBs    (J) := Parts.Table (J).BB;
         end loop;

         Position_Builder_At_End (Phi_BB);
         Result := Build_Phi (Values, BBs);

         --  In the elementary case, convert to the result type, since we
         --  may not already have done this.

         return (if    Related_Type (Result) = Expr_GT then Result
                 elsif Is_Data (Result) then Convert (Result, Expr_GT)
                 else  Convert_Ref (Result, Expr_GT));
      end;
   end Emit_If_Expression;

   ------------------
   -- Emit_Min_Max --
   ------------------

   function Emit_Min_Max
     (Exprs : List_Id; Compute_Max : Boolean) return GL_Value
   is
      LHS_N      : constant N_Subexpr_Id := First (Exprs);
      BT         : constant GL_Type      :=
        Base_GL_Type (Full_GL_Type (LHS_N));
      LHS        : constant GL_Value     := Emit_Convert_Value (LHS_N, BT);
      RHS        : constant GL_Value     :=
        Emit_Convert_Value (Last (Exprs), BT);
      Choose     : constant GL_Value     :=
        Build_Elementary_Comparison
        ((if Compute_Max then N_Op_Gt else N_Op_Lt), LHS, RHS);
      RHS_No_Nan : constant GL_Value     :=
        (if   Is_Floating_Point_Type (RHS) then F_Cmp (Real_OEQ, RHS, RHS)
         else Const_True);

   begin
      --  If the comparison is True, the result is the LHS. But if the
      --  comparison is False, the result RHS is RHS isn't a Nan and
      --  otherwise LHS.

      return Build_Select (Choose, LHS, Build_Select (RHS_No_Nan, RHS, LHS));
   end Emit_Min_Max;

   ----------------------------
   -- Safe_For_Short_Circuit --
   ----------------------------

   function Safe_For_Short_Circuit (N : N_Subexpr_Id) return Boolean is
      function Process (N : Node_Id) return Traverse_Result;
      --  Process one node in search of something that could cause an
      --  exception.

      -------------
      -- Process --
      -------------

      function Process (N : Node_Id) return Traverse_Result is
      begin
         if Nkind (N) in N_Op_Divide            |
                         N_Explicit_Dereference |
                         N_Indexed_Component
         then
            return Abandon;
         else
            return OK;
         end if;
      end Process;

      function Search_For_Exception is new Traverse_Func (Process);

   begin
      return not Decls_Only and then Side_Effect_Free (N)
        and then Search_For_Exception (N) /= Abandon;
   end Safe_For_Short_Circuit;

   -------------------------
   -- Emit_Loop_Statement --
   -------------------------

   procedure Emit_Loop_Statement (N : N_Loop_Statement_Id) is
      procedure Emit_Loop_Body;
      --  Emit the body of the loop

      Loop_Identifier : constant Opt_E_Loop_Id             :=
        (if Present (Identifier (N)) then Entity (Identifier (N)) else Empty);
      Iter_Scheme     : constant Opt_N_Iteration_Scheme_Id :=
        Iteration_Scheme (N);
      Is_Mere_Loop    : constant Boolean                   := No (Iter_Scheme);
      Is_For_Loop     : constant Boolean                   :=
        not Is_Mere_Loop
        and then Present (Loop_Parameter_Specification (Iter_Scheme));
      BB_Exit         : constant Basic_Block_T             :=
        Create_Basic_Block;
      BB_Start        : constant Basic_Block_T             :=
        (if   Is_For_Loop then Create_Basic_Block
         else Enter_Block_With_Node (Empty));

      --------------------
      -- Emit_Loop_Body --
      --------------------

      procedure Emit_Loop_Body is
      begin
         --  Record where the exit point for this loop is, create a block
         --  to handle any per-iteration variables, write the loop body
         --  itself, including a branch to the iteration point, and ????

         Push_Loop (Loop_Identifier, BB_Exit);
         Push_Block;
         Emit (Statements (N));
         Set_Debug_Pos_At_Node (N);
         Pop_Block;
         Pop_Loop;
      end Emit_Loop_Body;

   begin
      --  Handle each case separarely. First we have the case of no
      --  iteration scheme, where the only ways to exit the loop are
      --  explicit.

      if Is_Mere_Loop then
         Emit_Loop_Body;
         Build_Br (BB_Start);

      --  Next is the case where we have a condition, but not iteration
      --  variable. In that case, we start the loop with a test of that
      --  condition.

      elsif not Is_For_Loop then
         declare
            BB_Stmts : constant Basic_Block_T := Create_Basic_Block;

         begin
            if not Decls_Only then
               Emit_If_Cond (Condition (Iter_Scheme), BB_Stmts, BB_Exit);
               Position_Builder_At_End (BB_Stmts);
               Emit_Loop_Body;
               Build_Br (BB_Start);
            end if;
         end;

      --  The remaining case is a FOR loop. We have an initialization
      --  section, the body of the loop, an adjustment of the iterator, and
      --  a comparison.

      else
         --  We use the subtype of the loop parameter only to establish the
         --  bounds of the loop. It's more efficient to do the computation in
         --  the base type than to potentially have conversions to and from
         --  the subtype.

         declare
            Spec       : constant N_Loop_Parameter_Specification_Id :=
              Loop_Parameter_Specification (Iter_Scheme);
            E          : constant E_Loop_Parameter_Id               :=
              Defining_Identifier (Spec);
            Reversed   : constant Boolean                           :=
              Reverse_Present (Spec);
            Var_GT     : constant GL_Type                           :=
              Full_GL_Type (E);
            Var_BT     : constant GL_Type                           :=
              Base_GL_Type (Var_GT);
            Uns_BT     : constant Boolean                           :=
              Is_Unsigned_Type (Var_BT);
            One        : constant GL_Value                          :=
              Const_Int (Var_BT, Uint_1);
            SRange     : constant N_Has_Bounds_Id                   :=
              Simplify_Range (Scalar_Range (Var_GT));
            Low        : constant GL_Value                          :=
              Emit_Convert_Value (Low_Bound (SRange), Var_BT);
            High       : constant GL_Value                          :=
              Emit_Convert_Value (High_Bound (SRange), Var_BT);
            Start      : constant GL_Value                          :=
              (if Reversed then High else Low);
            Last       : constant GL_Value                          :=
              (if Reversed then Low else High);
            Loop_Var   : constant GL_Value                          :=
              Allocate_For_Type (Var_BT, N => E, E => E);
            Prev       : GL_Value;

         begin
            --  If we use an inequality comparison against the last element
            --  in the range each iteration of the loop, that comparison
            --  will always succeed if that element is a bound of the
            --  subtype. We can replace that comparison with a equality
            --  comparison if we add a test that verifies that the loop
            --  will be executed at least once. But once we do that, we can
            --  move the test to the end of the loop, which is a more
            --  canonical form. We need not do this test if we can prove
            --  that the loop will always be executed, but we only check
            --  for the trivial case of constants. If we determine that the
            --  loop is never executed, we don't do anything more.

            if Is_Constant (Low) and then Is_Constant (High)
              and then High < Low
            then
               Delete_Basic_Block (BB_Start);
               Delete_Basic_Block (BB_Exit);
               return;

            --  Otherwise, if either isn't a constant, add the test

            elsif not Is_Constant (Low) or else not Is_Constant (High) then
               declare
                  BB_Init : constant Basic_Block_T := Create_Basic_Block;

               begin
                  Build_Cond_Br
                    (I_Cmp ((if Uns_BT then Int_ULT else Int_SLT), High, Low),
                      BB_Exit, BB_Init);
                  Position_Builder_At_End (BB_Init);
               end;
            end if;

            --  Initialization block: initialize the loop variable

            Set_Value                        (E, Loop_Var);
            Create_Local_Variable_Debug_Data (E, Loop_Var);
            C_Set_Entity                     (Loop_Var, E);
            Store (Start, Loop_Var);
            Move_To_BB (BB_Start);

            --  Now the loop body

            Emit_Loop_Body;

            --  Increment (or decrement, as requested) the loop variable
            --  and test for the end condition.

            if not Are_In_Dead_Code then
               Prev := Get (Loop_Var, Data);
               Store ((if Reversed then Sub (Prev, One) else Add (Prev, One)),
                      Loop_Var);
               Build_Cond_Br (I_Cmp (Int_NE, Prev, Last), BB_Start, BB_Exit);
            end if;
         end;
      end if;

      Position_Builder_At_End (BB_Exit);

   end Emit_Loop_Statement;

end GNATLLVM.Conditionals;
