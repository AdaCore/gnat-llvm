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

with Exp_Util; use Exp_Util;
with Stand;    use Stand;

with LLVM.Core;  use LLVM.Core;

with GNATLLVM.Arrays;      use GNATLLVM.Arrays;
with GNATLLVM.Compile;     use GNATLLVM.Compile;
with GNATLLVM.Subprograms; use GNATLLVM.Subprograms;
with GNATLLVM.Utils;       use GNATLLVM.Utils;

package body GNATLLVM.Conditionals is

   ----------------------------
   -- Build_Short_Circuit_Op --
   ----------------------------

   function Build_Short_Circuit_Op
     (Left, Right : Node_Id; Op : Node_Kind) return GL_Value
   is
      And_Op               : constant Boolean := Op in N_And_Then | N_Op_And;
      --  Whether this is an AND or OR operation

      LHS, RHS             : GL_Value;
      --  We start evaluating the LHS in the current block, but we need to
      --  record which block it completes in, since it may not be the
      --  same block.

      Block_Left_Expr_End  : Basic_Block_T;
      --  Block which contains the evaluation of the right part
      --  expression of the operator and its end.

      Block_Right_Expr     : constant Basic_Block_T :=
        Create_Basic_Block ("scl-right-expr");
      Block_Right_Expr_End : Basic_Block_T;
      --  Block containing the exit code (the phi that selects that value)

      Block_Exit           : constant Basic_Block_T :=
        Create_Basic_Block ("scl-exit");

   begin
      --  In the case of And, evaluate the right expression when Left is
      --  true. In the case of Or, evaluate it when Left is false.

      LHS := Emit_Expression (Left);
      Block_Left_Expr_End := Get_Insert_Block;

      Build_Cond_Br (I_Cmp (Int_NE, LHS, Const_Null (LHS)),
                     (if And_Op then Block_Right_Expr else Block_Exit),
                     (if And_Op then Block_Exit else Block_Right_Expr));

      --  Emit code for the evaluation of the right part expression

      Position_Builder_At_End (Block_Right_Expr);
      RHS := Emit_Expression (Right);

      Block_Right_Expr_End := Get_Insert_Block;
      Move_To_BB (Block_Exit);

      --  If we exited the entry block, it means that for AND, the result
      --  is false and for OR, it's true.  Otherwise, the result is the right.

      return Build_Phi
        ((1 => Const_Int (RHS, (if And_Op then Uint_0 else Uint_1)), 2 => RHS),
         (1 => Block_Left_Expr_End, 2 => Block_Right_Expr_End));
   end Build_Short_Circuit_Op;

   ---------------------
   -- Emit_Comparison --
   ---------------------

   function Emit_Comparison
     (Kind : Node_Kind; LHS, RHS : Node_Id) return GL_Value
   is
      Operation : constant Pred_Mapping := Get_Preds (Kind);
      TE        : constant Entity_Id    := Full_Etype (LHS);
      BT        : constant Entity_Id    := Full_Base_Type (TE);

   begin
      --  LLVM treats pointers as integers regarding comparison.  But we first
      --  have to see if the pointer has an activation record.  If so,
      --  we just compare the functions, not the activation record.

      if Is_Access_Subprogram_Type (TE)
        and then not Has_Foreign_Convention (TE)
      then
         return I_Cmp
           (Operation.Unsigned, Subp_Ptr (LHS), Subp_Ptr (RHS));

      elsif Is_Elementary_Type (TE) then
         return Emit_Elementary_Comparison
           (Kind, Emit_Convert_Value (LHS, BT), Emit_Convert_Value (RHS, BT));

      --  We'll see some simple record comparisons, typically if they're
      --  Equivalent_Types of, e.g., an E_Access_Protected_Subprogram_Type.

      elsif Is_Record_Type (TE) then
         declare
            --  Now we need to get the size of the record (in bytes) to do
            --  the memory comparison.  Memcmp is defined as returning zero
            --  for a zero size, so we don't need to worry about testing
            --  for that case.

            LHS_Val : constant GL_Value := Emit_LValue (LHS);
            RHS_Val : constant GL_Value := Emit_LValue (RHS);
            Size    : constant GL_Value :=
              Compute_Size (Full_Designated_Type (LHS_Val),
                            Full_Designated_Type (RHS_Val),
                            LHS_Val, RHS_Val);
            Memcmp  : constant GL_Value :=
              Call (Get_Memory_Compare_Fn, Standard_Integer,
                    (1 => Pointer_Cast (LHS_Val, Standard_A_Char),
                     2 => Pointer_Cast (RHS_Val, Standard_A_Char),
                     3 => Size));
         begin
            return I_Cmp (Operation.Signed, Memcmp,
                          Const_Null (Standard_Integer));
         end;
      else
         pragma Assert (Is_Array_Type (TE)
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
              Create_Basic_Block ("merge");
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
      LHS, RHS          : Node_Id;
      BB_True, BB_False : Basic_Block_T)
   is
      Cond : GL_Value;

   begin
      --  Do the array case here, where we have labels, to simplify the
      --  logic and take advantage of the reality that almost all array
      --  comparisons are part of "if" statements.

      if  Is_Array_Type (Full_Etype (LHS)) then
         pragma Assert (Kind in N_Op_Eq | N_Op_Ne);
         pragma Assert (Number_Dimensions (Full_Etype (LHS)) =
                          Number_Dimensions (Full_Etype (RHS)));

         declare
            Last_Dim       : constant Nat     :=
              Number_Dimensions (Full_Etype (LHS)) - 1;
            LHS_Complexity : constant Nat     :=
              Get_Array_Size_Complexity (Full_Etype (LHS));
            RHS_Complexity : constant Nat     :=
              Get_Array_Size_Complexity (Full_Etype (LHS));
            Our_LHS        : constant Node_Id :=
              (if LHS_Complexity > RHS_Complexity then LHS else RHS);
            --  To simplify the code below, we arrange things so that the
            --  array with the most complex size is on the LHS.

            Our_RHS        : constant Node_Id  :=
              (if LHS_Complexity > RHS_Complexity then RHS else LHS);
            BB_T           : constant Basic_Block_T :=
              (if Kind = N_Op_Eq then BB_True else BB_False);
            BB_F           : constant Basic_Block_T :=
              (if Kind = N_Op_Eq then BB_False else BB_True);
            LHS_Val        : constant GL_Value := Emit_LValue (Our_LHS);
            RHS_Val        : constant GL_Value := Emit_LValue (Our_RHS);
            BB_Next        : Basic_Block_T;
            LHS_Lengths    : GL_Value_Array (0 .. Last_Dim);
            RHS_Lengths    : GL_Value_Array (0 .. Last_Dim);

         begin
            --  There's an obscure case where if both arrays have a
            --  dimension that's zero (whether or not it's the same
            --  dimension in both), the arrays compare true.  This is
            --  tested in C45264A.  If this is a single-dimensional array,
            --  this falls through from the normal computation, but for
            --  multi-dimensional arrays, we have to actually do the test.

            --  Start by getting all the lengths

            for Dim in 0 .. Last_Dim loop
               LHS_Lengths (Dim) :=
                 Get_Array_Length (Full_Etype (Our_LHS), Dim, LHS_Val);
               RHS_Lengths (Dim) :=
                 Get_Array_Length (Full_Etype (Our_RHS), Dim, RHS_Val);
            end loop;

            if Last_Dim /= 1 then

               --  RHS is the least complex.  So check its dimensions.
               --  If any are zero, we need to check LHS.  If none are zero
               --  (and hopefully we'll know this at compile-time), we
               --  don't need to check LHS and can go to the next test.

               declare
                  BB_RHS_Has_Zero_Dim : constant Basic_Block_T :=
                    Create_Basic_Block ("rhs-has-0-dim");
                  BB_Continue         : constant Basic_Block_T :=
                    Create_Basic_Block ("normal-tests");
               begin
                  for Dim in 0 .. Last_Dim loop
                     BB_Next :=
                       (if Dim = Last_Dim then BB_Continue
                        else Create_Basic_Block);
                     Cond := Emit_Elementary_Comparison
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
                     Cond    := Emit_Elementary_Comparison
                       (N_Op_Eq, LHS_Lengths (Dim),
                        Const_Null (LHS_Lengths (Dim)));
                     Build_Cond_Br (Cond, BB_T, BB_Next);
                     Position_Builder_At_End (BB_Next);
                  end loop;

                  Position_Builder_At_End (BB_Continue);
               end;
            end if;

            --  For each dimension, see if the lengths of the two arrays
            --  are different.  If so, the comparison is false.

            --  We need to be careful with types here: LHS and RHS are
            --  the actual array types, but, because we called Emit_LValue,
            --  LHS_Val and RHS_Val are actually references to the array,
            --  not the array.

            for Dim in 0 .. Number_Dimensions (Full_Etype (LHS)) - 1 loop
               BB_Next := Create_Basic_Block;
               Cond    := Emit_Elementary_Comparison
                 (N_Op_Eq, LHS_Lengths (Dim), RHS_Lengths (Dim));
               Build_Cond_Br (Cond, BB_Next, BB_F);
               Position_Builder_At_End (BB_Next);
            end loop;

            declare

               --  Now we need to get the size of the array (in bytes)
               --  to do the memory comparison.  Memcmp is defined as
               --  returning zero for a zero size, so we don't need to worry
               --  about testing for that case.

               Size   : constant GL_Value :=
                 Compute_Size (Full_Designated_Type (LHS_Val),
                               Full_Designated_Type (RHS_Val),
                               LHS_Val, RHS_Val);
               Memcmp : constant GL_Value := Call
                 (Get_Memory_Compare_Fn, Standard_Integer,
                  (1 => Pointer_Cast (Get (LHS_Val, Reference),
                                      Standard_A_Char),
                   2 => Pointer_Cast (Get (RHS_Val, Reference),
                                      Standard_A_Char),
                   3 => Size));
               Cond   : constant GL_Value :=
                 I_Cmp (Int_EQ, Memcmp, Const_Null (Standard_Integer));

            begin
               Build_Cond_Br (Cond, BB_T, BB_F);
            end;
         end;

      --  And now we have the other cases.  We do have to be careful in how
      --  the tests work that we don't have infinite mutual recursion.

      else
         Cond := Emit_Comparison (Kind, LHS, RHS);
         Build_Cond_Br (Cond, BB_True, BB_False);
      end if;
   end Emit_Comparison_And_Branch;

   --------------------------------
   -- Emit_Elementary_Comparison --
   --------------------------------

   function Emit_Elementary_Comparison
     (Kind               : Node_Kind;
      Orig_LHS, Orig_RHS : GL_Value) return GL_Value
   is
      Operation    : constant Pred_Mapping := Get_Preds (Kind);
      LHS          : GL_Value := Orig_LHS;
      RHS          : GL_Value := Orig_RHS;

   begin
      --  If a scalar type (meaning both must be), convert each operand to
      --  its base type.

      if Is_Scalar_Type (LHS) then
         LHS := Convert (LHS, Full_Base_Type (LHS));
         RHS := Convert (RHS, Full_Base_Type (RHS));
      end if;

      --  If we're comparing two access types, first get the values as
      --  references to the designated types, then as a single-word
      --  reference.  To be a valid comparion, they must be the same LLVM
      --  type at that point.

      if Is_Access_Type (LHS) then

         LHS := Get (From_Access (LHS), Reference_For_Integer);
         RHS := Get (From_Access (RHS), Reference_For_Integer);

         --  Now we have simple pointers, but they may not be the same LLVM
         --  type.  If they aren't, convert the RHS to the type of the LHS.

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
         --  we know the aren't pointers to unconstrained arrays.  It's
         --  possible that the two pointer types aren't the same, however.
         --  So in that case, convert one to the pointer of the other.

         if Is_Access_Type (LHS) and then Type_Of (RHS) /= Type_Of (LHS) then
            RHS := Pointer_Cast (RHS, LHS);
         end if;

         --  Now just do the normal comparison, but be sure to get the
         --  signedness from the original type, not the base type.

         return I_Cmp
           ((if Is_Unsigned_Type (Orig_LHS) or else Is_Access_Type (LHS)
             then Operation.Unsigned else Operation.Signed),
            LHS, RHS);

      end if;
   end Emit_Elementary_Comparison;

   ---------------------
   -- Emit_And_Or_Xor --
   ---------------------

   function Emit_And_Or_Xor
     (Kind : Node_Kind; LHS_Node, RHS_Node : Node_Id) return GL_Value
   is
      BT  : constant Entity_Id := Full_Base_Type (Full_Etype (LHS_Node));
      LHS : GL_Value           := Emit (LHS_Node);
      RHS : GL_Value           := Emit (RHS_Node);

   begin
      --  If both are Boolean_Data, we can compute our result in that
      --  relationship.  Otherwise, force to Data.

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

   procedure Emit_Case_Statement (N : Node_Id) is

      Alts        : constant List_Id       := Alternatives (N);
      Start_BB    : constant Basic_Block_T := Get_Insert_Block;
      BB_End      : constant Basic_Block_T := Create_Basic_Block ("case-end");
      Alt         : Node_Id                := First_Non_Pragma (Alts);
      Current_Alt : Nat                    := 1;
      BBs         : Basic_Block_Array (1 .. List_Length_Non_Pragma (Alts));

   begin
      --  First emit the code for each alternative and add its BB

      while Present (Alt) loop
         BBs (Current_Alt) := Create_Basic_Block ("case-alt");
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
      --  Move the highest-cost alternative to the last entry.  Is_Switch
      --  says whether we look at the switch cost or the if cost.

      -------------------
      -- Count_Choices --
      -------------------

      function Count_Choices (Alts : List_Id) return Nat is
         Alt          : Node_Id := First_Non_Pragma (Alts);
         First_Choice : Node_Id;

      begin
         return Num_Choices : Nat := 0 do
            while Present (Alt) loop

               --  We have a peculiarity in the "others" case of a case
               --  statement.  The Alternative points to a list of choices
               --  of which the first choice is an N_Others_Choice.  So
               --  handle that specially both here and when we compute our
               --  Choices below.

               First_Choice := First (Discrete_Choices (Alt));
               Num_Choices  := Num_Choices +
                 (if Nkind (First_Choice) = N_Others_Choice
                  then List_Length (Others_Discrete_Choices (First_Choice))
                  else List_Length (Discrete_Choices (Alt)));
                  Next_Non_Pragma (Alt);
            end loop;
         end return;

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

      Max_Cost       : constant Nat    := 10_000;
      Num_Alts       : constant Nat    := List_Length_Non_Pragma (In_Alts);
      Typ            : constant Type_T := Create_Type (Full_Etype (LHS));
      Current_Alt    : Nat             := 1;
      Current_Choice : Nat             := 1;
      Alts           : array (1 .. Num_Alts) of One_Alt;
      Choices        : array (1 .. Count_Choices (In_Alts)) of One_Choice;
      BB             : Basic_Block_T;
      First_Choice   : Nat;
      Alt, Choice    : Node_Id;
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
            Our_Cost := (if Is_Switch then Alts (J).Switch_Cost
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
      --  First we scan all the alternatives and choices and fill in most
      --  of the data.

      Alt := First_Non_Pragma (In_Alts);
      while Present (Alt) loop
         First_Choice := Current_Choice;
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
         --  if it was added by the front end).  If so, it means we don't
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
      --  there aren't too many choices.  If not, we use "if".  First we
      --  find the alternative with the largest switch cost and make that
      --  the "others" option.  Then we see if the total cost of the remaining
      --  alternatives is low enough (we use 100).  If so, use that approach.

      Swap_Highest_Cost (True);
      Switch_Cost := 0;

      for J in Alts'First .. Alts'Last - 1 loop
         Switch_Cost := Switch_Cost + Alts (J).Switch_Cost;
      end loop;

      if Switch_Cost < 100 then

         --  First we emit the actual "switch" statement, then we add
         --  the cases to it.  Here we collect all the basic blocks.

         declare
            BBs : Basic_Block_Array (Alts'Range);
         begin
            for J in BBs'Range loop
               BBs (J) := Alts (J).BB;
            end loop;

            Switch := Build_Switch (LHS, BBs (BBs'Last), BBs'Length);

            for J in Alts'First .. Alts'Last - 1 loop
               for K in Alts (J).First_Choice .. Alts (J).Last_Choice loop
                  for L in UI_To_Int (Choices (K).Low) ..
                    UI_To_Int (Choices (K).High) loop
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
                  --  next choice.  Note that we can't simply test
                  --  against Choices'Last because we may have swapped
                  --  some other alternative with Alts'Last.

                  if J = Alts'Last - 1 and then K = Alts (J).Last_Choice then
                     BB := Alts (Alts'Last).BB;
                  else
                     BB := Create_Basic_Block ("case-when");
                  end if;

                  Emit_If_Range (LHS, Choices (K).Low, Choices (K).High,
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

   procedure Emit_If (N : Node_Id) is

      --  Record information about each part of an "if" statement

      type If_Ent is record
         Cond     : Node_Id;         --  Expression to test.
         Stmts    : List_Id;         --  Statements to emit if true.
         BB_True  : Basic_Block_T;   --  Basic block to branch for true.
         BB_False : Basic_Block_T;   --  Basic block to branch for false.
      end record;

      If_Parts_Pos : Nat := 1;
      If_Parts     : array (0 .. List_Length (Elsif_Parts (N))) of If_Ent;
      BB_End       : Basic_Block_T;
      Elsif_Part   : Node_Id;

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
            If_Parts (If_Parts_Pos)
              := (Cond     => Condition (Elsif_Part),
                  Stmts    => Then_Statements (Elsif_Part),
                  BB_True  => Create_Basic_Block,
                  BB_False => Create_Basic_Block);
            If_Parts_Pos := If_Parts_Pos + 1;
            Next (Elsif_Part);
         end loop;
      end if;

      --  When done, each part goes to the end of the statement.  If there's
      --  an "else" clause, it's a new basic block and the end; otherwise,
      --  it's the last False block.

      BB_End := (if Present (Else_Statements (N)) then Create_Basic_Block
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

   function Is_Simple_Conditional (N : Node_Id) return Boolean is
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

            --  These could be done using the trick that maps signed
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

   procedure Emit_If_Cond (N : Node_Id; BB_True, BB_False : Basic_Block_T)
   is
      And_Op : constant Boolean := Nkind_In (N, N_And_Then, N_Op_And);
      BB_New : Basic_Block_T;
      Result : GL_Value;

   begin
      case Nkind (N) is

         --  Process operations that we can handle in terms of different branch
         --  mechanisms, such as short-circuit operators.

         when N_Expression_With_Actions =>
            Emit (Actions (N));
            Emit_If_Cond (Expression (N), BB_True, BB_False);
            return;

         when N_Op_Not =>
            Emit_If_Cond (Right_Opnd (N), BB_False, BB_True);
            return;

         when N_And_Then | N_Or_Else | N_Op_And | N_Op_Or =>

            --  If this is not a short-circuit form, we can only do this
            --  as a short-circuit if there are no side-effects.

            if Nkind_In (N, N_And_Then, N_Or_Else)
              or else (Safe_For_Short_Circuit (Left_Opnd (N))
                         and then Safe_For_Short_Circuit (Right_Opnd (N)))
            then
               --  Depending on the result of the test of the left operand,
               --  we either go to a final basic block or to a new
               --  intermediate one where we test the right operand.

               BB_New := Create_Basic_Block ("short-circuit");
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

         when N_In | N_Not_In =>

            --  If we can decode the range into Uint's, we can just do
            --  simple comparisons.

            declare
               Low, High : Uint;

            begin
               Decode_Range (Right_Opnd (N), Low, High);
               if Low /= No_Uint and then High /= No_Uint then
                  Emit_If_Range
                    (Emit_Expression (Left_Opnd (N)), Low, High,
                     (if Nkind (N) = N_In then BB_True else BB_False),
                     (if Nkind (N) = N_In then BB_False else BB_True));
                  return;
               end if;
            end;

         when others =>
            null;

      end case;

      --  If we haven't handled it via one of the special cases above, just
      --  evaluate the expression and do the branch, depending on whether
      --  the result if zero or nonzero, unless we already have an i1
      --  (Boolean_Data relationship).  We must have a boolean type here.

      pragma Assert (Is_Boolean_Type (Etype (N)));
      Result := Emit (N);
      if Relationship (Result) /= Boolean_Data then
         Result := Get (Result, Data);
         Result := I_Cmp (Int_NE, Result, Const_Null (Result));
      end if;

      Build_Cond_Br (Result, BB_True, BB_False);

   end Emit_If_Cond;

   -------------------
   -- Emit_If_Range --
   -------------------

   procedure Emit_If_Range
     (LHS               : GL_Value;
      Low, High         : Uint;
      BB_True, BB_False : Basic_Block_T)
   is
      LHS_BT   : constant Entity_Id := Full_Base_Type (LHS);
      LHS_Base : GL_Value;
      Cond     : GL_Value;
      Inner_BB : Basic_Block_T;

   begin
      --  For discrete types (all we handle here), handle ranges by testing
      --  against the high and the low and branching as appropriate.  We
      --  must be sure to evaluate the LHS only once.  But first check for
      --  a range of size one since that's only one comparison.  If we are
      --  comparing against a range, be sure to do the comparison in the
      --  base type in case the subtype is unsigned and the base type isn't.

      if Low = High then
         Cond := Emit_Elementary_Comparison
           (N_Op_Eq, LHS, Const_Int (LHS, Low));
         Build_Cond_Br (Cond, BB_True, BB_False);
      else
         Inner_BB := Create_Basic_Block ("range-test");
         LHS_Base := Convert (LHS, LHS_BT);
         Cond     := Emit_Elementary_Comparison (N_Op_Ge, LHS_Base,
                                                 Const_Int (LHS_BT, Low));
         Build_Cond_Br (Cond, Inner_BB, BB_False);
         Position_Builder_At_End (Inner_BB);
         Cond := Emit_Elementary_Comparison (N_Op_Le, LHS_Base,
                                             Const_Int (LHS_BT, High));
         Build_Cond_Br (Cond, BB_True, BB_False);
      end if;
   end Emit_If_Range;

   ------------------------
   -- Emit_If_Expression --
   ------------------------

   function Emit_If_Expression (N : Node_Id) return GL_Value
   is
      TE         : constant Entity_Id     := Full_Etype (N);
      Condition  : constant Node_Id       := First (Expressions (N));
      Then_Expr  : constant Node_Id       := Next (Condition);
      Else_Expr  : constant Node_Id       := Next (Then_Expr);
      Elementary : constant Boolean       := Is_Elementary_Type (TE);
      Next_BB    : constant Basic_Block_T := Create_Basic_Block ("if-next");
      Then_BB    : Basic_Block_T          := Create_Basic_Block ("if-then");
      Else_BB    : Basic_Block_T          := Create_Basic_Block ("if-else");
      Then_Value : GL_Value;
      Else_Value : GL_Value;
      Then_Type  : Entity_Id;
      Else_Type  : Entity_Id;
      R          : GL_Relationship;
      Result     : GL_Value;

   begin
      --  Start by generating the conditional branch and working on each
      --  expression in its own BB.

      Emit_If_Cond (Condition, Then_BB, Else_BB);
      Position_Builder_At_End (Then_BB);
      Then_Value := Emit (Then_Expr);
      Then_Type  := Related_Type (Then_Value);
      Then_BB    := Get_Insert_Block;
      Position_Builder_At_End (Else_BB);
      Else_Value := Emit (Else_Expr);
      Else_Type  := Related_Type (Else_Value);
      Else_BB    := Get_Insert_Block;

      --  We need to be sure that the operands are either both data or both
      --  references and that they are the same LLVM type for the Phi
      --  below.  The front end assures they are the same base type, but we
      --  do have potential issues if they're both different record or
      --  arrays subtypes.
      --
      --  If these are elementary types, ensure both are data and, if they
      --  aren't already the same type as each other, convert both to the
      --  result type.

      if Elementary then
         Position_Builder_At_End (Then_BB);
         Then_Value := Get (Then_Value, Data);
         if Type_Of (Then_Value) /= Type_Of (Else_Value) then
            Then_Value := Convert (Then_Value, TE);
         end if;
         Then_BB    := Get_Insert_Block;

         Position_Builder_At_End (Else_BB);
         Else_Value := Get (Else_Value, Data);
         if Type_Of (Then_Value) /= Type_Of (Else_Value) then
            Else_Value := Convert (Else_Value, TE);
         end if;
         Else_BB    := Get_Insert_Block;

      --  In the composite case, we may have one side as a reference
      --  and the other as data.  We need to make them the same.  In
      --  general, it's cheaper to change reference to data than vice
      --  versa, but if the types are different, we have to have both as
      --  references because that's the way we convert composite types.

      elsif Is_Reference (Then_Value) /= Is_Reference (Else_Value)
        or else Then_Type /= Else_Type
      then
         R := (if Related_Type (Then_Value) = Related_Type (Else_Value)
                 then Data else Any_Reference);
         Position_Builder_At_End (Then_BB);
         Then_Value := Get (Then_Value, R);
         if R = Any_Reference then
            Then_Value := Convert_Ref (Then_Value, TE);
         end if;
         Then_BB    := Get_Insert_Block;

         Position_Builder_At_End (Else_BB);
         Else_Value := Get (Else_Value, R);
         if R = Any_Reference then
            Else_Value := Convert_Ref (Else_Value, TE);
         end if;
         Else_BB    := Get_Insert_Block;
      end if;

      --  Both sides then branch to the Phi block and we emit the Phi,
      --  converting to either data or reference, depending on the type.
      --  In the elementary case, convert to the result type, since we
      --  may not already have done this.

      Position_Builder_At_End (Then_BB);
      Build_Br (Next_BB);
      Position_Builder_At_End (Else_BB);
      Move_To_BB (Next_BB);

      Result := Build_Phi ((1 => Then_Value, 2 => Else_Value),
                           (1 => Then_BB,    2 => Else_BB));
      if Elementary then
         return Convert (Result, TE);
      else
         return Get (Result, Object);
      end if;

   end Emit_If_Expression;

   ------------------
   -- Emit_Min_Max --
   ------------------

   function Emit_Min_Max
     (Exprs : List_Id; Compute_Max : Boolean) return GL_Value
   is
      LHS_N      : constant Node_Id   := First (Exprs);
      BT         : constant Entity_Id := Full_Base_Type (Full_Etype (LHS_N));
      LHS        : constant GL_Value  := Emit_Convert_Value (LHS_N, BT);
      RHS        : constant GL_Value  := Emit_Convert_Value (Last (Exprs), BT);
      Choose     : constant GL_Value  :=
        Emit_Elementary_Comparison
        ((if Compute_Max then N_Op_Gt else N_Op_Lt), LHS, RHS);
      RHS_No_Nan : constant GL_Value :=
        (if   Is_Floating_Point_Type (RHS) then F_Cmp (Real_OEQ, RHS, RHS)
         else Const_True);

   begin
      --  If the comparison is True, the result is the LHS.  But if the
      --  comparison is False, the result RHS is RHS isn't a Nan and
      --  otherwise LHS.

      return Build_Select (Choose, LHS, Build_Select (RHS_No_Nan, RHS, LHS));
   end Emit_Min_Max;

   ----------------------------
   -- Safe_For_Short_Circuit --
   ----------------------------

   function Safe_For_Short_Circuit (N : Node_Id) return Boolean is
      function Process (N : Node_Id) return Traverse_Result;
      --  Process one node in search of something that could cause an
      --  exception.

      -------------
      -- Process --
      -------------

      function Process (N : Node_Id) return Traverse_Result is
      begin
         return (if   Nkind_In (N, N_Op_Divide, N_Explicit_Dereference,
                                N_Indexed_Component)
                 then Abandon else OK);
      end Process;

      function Search_For_Exception is new Traverse_Func (Process);

   begin
      return Side_Effect_Free (N) and then Search_For_Exception (N) /= Abandon;
   end Safe_For_Short_Circuit;

end GNATLLVM.Conditionals;
