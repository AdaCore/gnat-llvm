------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2022, AdaCore                     --
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

with Ada.Containers.Generic_Sort;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Sets;

with Debug;  use Debug;
with Output; use Output;
with Table;

with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

with CCG.Instructions; use CCG.Instructions;
with CCG.Subprograms;  use CCG.Subprograms;
with CCG.Target;       use CCG.Target;
with CCG.Utils;        use CCG.Utils;

package body CCG.Flow is

   --  First is how we record a line of C to be output, which contains the
   --  string to output and the instruction it comes from (for debug data).

   type Line_Data is record
      Text : Str;
      --  The string containing the line to be written

      Inst : Value_T;
      --  The instruction corresponding to the line (for debug data)

   end record;

   package Lines is new Table.Table
     (Table_Component_Type => Line_Data,
      Table_Index_Type     => Line_Idx,
      Table_Low_Bound      => Line_Idx_Low_Bound,
      Table_Initial        => 10,
      Table_Increment      => 50,
      Table_Name           => "Cases");

   --  Next is a table containing pairs of switch statement values and
   --  targets. We record the value as an LLVM integer so we can use it
   --  for a stable sort and as a Str so we can output the proper value in
   --  C, including any handling of unsigned.

   type Case_Data is record
      Value  : Value_T;
      --  Value corresponding to this case or No_Value_T for "default"

      Expr   : Str;
      --  String for this case node or No_Str if this is for "default"

      Target : Flow_Idx;
      --  The Flow corresponding to this case node, if any. No flow means
      --  that the target of this node is the same as that of the next node
      --  (which may also not be Present).

      Inst   : Value_T;
      --  First instruction at target, for debug info
   end record;

   package Cases is new Table.Table
     (Table_Component_Type => Case_Data,
      Table_Index_Type     => Case_Idx,
      Table_Low_Bound      => Case_Idx_Low_Bound,
      Table_Initial        => 10,
      Table_Increment      => 50,
      Table_Name           => "Cases");

   --  Next is a table containing pairs of if/then tests and targets.

   type If_Data is record
      Test   : Str;
      --  Expression corresponding to the test, if Present. If not, this
      --  represents an "else".

      Inst   : Value_T;
      --  Instruction that does test (for debug info)

      Target : Flow_Idx;
      --  Destination if this test is true (or not Present)

   end record;

   package Ifs is new Table.Table
     (Table_Component_Type => If_Data,
      Table_Index_Type     => If_Idx,
      Table_Low_Bound      => If_Idx_Low_Bound,
      Table_Initial        => 100,
      Table_Increment      => 200,
      Table_Name           => "Ifs");

   --  Finally, we have the table that encodes the flows themselves

   type Flow_Data is record
      BB           : Basic_Block_T;
      --  Block corresponding to this flow, if not a return flow

      First_Line   : Line_Idx;
      --  First line that's part of this flow, if any

      Last_Line    : Line_Idx;
      --  Last line that's part of this flow, if any

      Use_Count    : Nat;
      --  Number of times this flow is referenced by another flow (always one
      --  for the entry block).

      Next         : Flow_Idx;
      --  Next flow executed after this one has completed, if any

      Is_Return    : Boolean;
      --  This is set for the unique return flow

      Return_Value : Str;
      --  If a return flow, the value to return, if any

      First_If     : If_Idx;
      Last_If      : If_Idx;
      --  First and last if/then/elseif/else parts, if any

      Case_Expr    : Str;
      --  Expression for switch statement, if any

      First_Case   : Case_Idx;
      Last_Case    : Case_Idx;
      --  First and last of cases for a switch statement, if any

   end record;

   package Flows is new Table.Table
     (Table_Component_Type => Flow_Data,
      Table_Index_Type     => Flow_Idx,
      Table_Low_Bound      => Flow_Idx_Low_Bound,
      Table_Initial        => 100,
      Table_Increment      => 200,
      Table_Name           => "Flows");

   --  We merge returns that have the same return value (or those that
   --  have no return value) and use a map to allow us to do that.

   package Return_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Value_T,
      Element_Type    => Flow_Idx,
      Hash            => Hash,
      Equivalent_Keys => "=");
   Return_Map : Return_Maps.Map;

   --  The unique Flow that indicates a return

   Current_Flow : Flow_Idx := Empty_Flow_Idx;
   --  The flow that we're currently building

   function New_Line (S : Str; V : Value_T) return Line_Idx
     with Pre => Present (S) and then Present (V);
   --  Create a new Line entry with the specified values

   function New_Case (V : Value_T; S : Str) return Case_Idx;
   --  Create a new case element for V and S, if any

   function New_If (S : Str; Inst : Value_T) return If_Idx
     with Pre  => Is_A_Instruction (Inst),
          Post => Present (New_If'Result);
   --  Create a new "if" piece for the specified value and instruction

   type Process_Subprogram is access procedure (Idx : Flow_Idx);
   procedure Process_Flows (Idx : Flow_Idx; Subprog : Process_Subprogram)
     with Pre => Present (Idx);
   --  Call Subprog once for each flow nested inside Idx in a depth-first
   --  fashion.

   function Case_Before (L, R : Case_Idx) return Boolean
     with Pre => Present (L) and then Present (R);
   --  Return whether L is before R in the sort order for case values

   procedure Swap_Cases (L, R : Case_Idx)
     with Pre => Present (L) and then Present (R);
   --  Swap the contents of L and R in the case table

   procedure Case_Sort is new Ada.Containers.Generic_Sort
     (Index_Type => Case_Idx, Before => Case_Before, Swap => Swap_Cases);
   --  Sort a list of Case parts

   function Final_Target (Idx : Flow_Idx) return Flow_Idx;
   --  Follow Idx through flows that just jump to another flow

   procedure Simplify_Final_Target (Idx : Flow_Idx)
     with Pre => Present (Idx);
   --  Simplify all targets in Idx to their ultimate destinations

   procedure Simplify_One_Flow_Cases (Idx : Flow_Idx)
     with Pre => Present (Idx);
   --  Sort a and simplify case parts in Idx

   function Effective_Flow (Idx : Flow_Idx) return Flow_Idx
     with Pre => Present (Idx), Post => Present (Effective_Flow'Result);
   --  Return the flow corresponding to Idx, which means following a
   --  any flows where there's a chain of Next links.

   procedure Try_Merge_Ifs (Idx : Flow_Idx)
     with Pre => Present (Idx);
   --  See if Idx has if parts that allow merging the destination of the
   --  "else" into another if parts and do the merge if so.

   ----------
   -- Text --
   ----------

   function Text (Idx : Line_Idx) return Str is
     (Lines.Table (Idx).Text);

   ----------
   -- Inst --
   ----------

   function Inst (Idx : Line_Idx) return Value_T is
     (Lines.Table (Idx).Inst);

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Idx : Line_Idx; S : Str) is
   begin
      Lines.Table (Idx).Text := S;
   end Set_Text;

   --------------
   -- Set_Inst --
   --------------

   procedure Set_Inst (Idx : Line_Idx; V : Value_T) is
   begin
      Lines.Table (Idx).Inst := V;
   end Set_Inst;

   -----------
   -- Value --
   -----------

   function Value (Idx : Case_Idx) return Value_T is
     (Cases.Table (Idx).Value);

   ----------
   -- Expr --
   ----------

   function Expr (Idx : Case_Idx) return Str is
     (Cases.Table (Idx).Expr);

   ------------
   -- Target --
   ------------

   function Target (Idx : Case_Idx) return Flow_Idx is
     (Cases.Table (Idx).Target);

   ----------
   -- Inst --
   ----------

   function Inst (Idx : Case_Idx) return Value_T is
     (Cases.Table (Idx).Inst);

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Idx : Case_Idx; V : Value_T) is
   begin
      Cases.Table (Idx).Value := V;
   end Set_Value;

   --------------
   -- Set_Expr --
   --------------

   procedure Set_Expr (Idx : Case_Idx; S : Str) is
   begin
      Cases.Table (Idx).Expr := S;
   end Set_Expr;

   ----------------
   -- Set_Target --
   ----------------

   procedure Set_Target (Idx : Case_Idx; Fidx : Flow_Idx) is
   begin
      Remove_Use (Target (Idx));
      Add_Use (Fidx);
      Cases.Table (Idx).Target := Fidx;
   end Set_Target;

   --------------
   -- Set_Inst --
   --------------

   procedure Set_Inst (Idx : Case_Idx; V : Value_T) is
   begin
      Cases.Table (Idx).Inst := V;
   end Set_Inst;

   ----------
   -- Test --
   ----------

   function Test (Idx : If_Idx) return Str is
     (Ifs.Table (Idx).Test);

   ----------
   -- Inst --
   ----------

   function Inst (Idx : If_Idx) return Value_T is
     (Ifs.Table (Idx).Inst);

   ------------
   -- Target --
   ------------

   function Target (Idx : If_Idx) return Flow_Idx is
     (Ifs.Table (Idx).Target);

   --------------
   -- Set_Test --
   --------------

   procedure Set_Test (Idx : If_Idx; S : Str) is
   begin
      Ifs.Table (Idx).Test := S;
   end Set_Test;

   --------------
   -- Set_Inst --
   --------------

   procedure Set_Inst (Idx : If_Idx; V : Value_T) is
   begin
      Ifs.Table (Idx).Inst := V;
   end Set_Inst;

   ----------------
   -- Set_Target --
   ----------------

   procedure Set_Target (Idx : If_Idx; Fidx : Flow_Idx) is
   begin
      Remove_Use (Target (Idx));
      Add_Use (Fidx);
      Ifs.Table (Idx).Target := Fidx;
   end Set_Target;

   --------
   -- BB --
   --------

   function BB (Idx : Flow_Idx) return Basic_Block_T is
     (Flows.Table (Idx).BB);

   ----------------
   -- First_Line --
   ----------------

   function First_Line (Idx : Flow_Idx) return Line_Idx is
     (Flows.Table (Idx).First_Line);

   ---------------
   -- Last_Stmt --
   ---------------

   function Last_Line (Idx : Flow_Idx) return Line_Idx is
     (Flows.Table (Idx).Last_Line);

   ---------------
   -- Use_Count --
   ---------------

   function Use_Count (Idx : Flow_Idx)  return Nat is
     (Flows.Table (Idx).Use_Count);

   ----------
   -- Next --
   ----------

   function Next (Idx : Flow_Idx) return Flow_Idx is
     (Flows.Table (Idx).Next);

   ---------------
   -- Is_Return --
   ---------------

   function Is_Return (Idx : Flow_Idx) return Boolean is
     (Flows.Table (Idx).Is_Return);

   ------------------
   -- Return_Value --
   ------------------

   function Return_Value (Idx : Flow_Idx) return Str is
     (Flows.Table (Idx).Return_Value);

   --------------
   -- First_If --
   --------------

   function First_If (Idx : Flow_Idx) return If_Idx is
     (Flows.Table (Idx).First_If);

   -------------
   -- Last_If --
   -------------

   function Last_If (Idx : Flow_Idx) return If_Idx is
     (Flows.Table (Idx).Last_If);

   ---------------
   -- Case_Expr --
   ---------------

   function Case_Expr (Idx : Flow_Idx)  return Str is
     (Flows.Table (Idx).Case_Expr);

   ----------------
   -- First_Case --
   ----------------

   function First_Case (Idx : Flow_Idx) return Case_Idx is
      (Flows.Table (Idx).First_Case);

   ----------------
   -- Last_Case --
   ----------------

   function Last_Case (Idx : Flow_Idx) return Case_Idx is
      (Flows.Table (Idx).Last_Case);

   ------------
   -- Set_BB --
   ------------

   procedure Set_BB (Idx : Flow_Idx; B : Basic_Block_T) is
   begin
      Flows.Table (Idx).BB := B;
   end Set_BB;

   --------------------
   -- Set_First_Line --
   --------------------

   procedure Set_First_Line (Idx : Flow_Idx; Lidx : Line_Idx) is
   begin
      Flows.Table (Idx).First_Line := Lidx;
   end Set_First_Line;

   -------------------
   -- Set_Last_Stmt --
   -------------------

   procedure Set_Last_Line (Idx : Flow_Idx; Lidx : Line_Idx) is
   begin
      Flows.Table (Idx).Last_Line := Lidx;
   end Set_Last_Line;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Idx : Flow_Idx) is
   begin
      if Present (Idx) then
         Flows.Table (Idx).Use_Count := Flows.Table (Idx).Use_Count + 1;
      end if;

   end Add_Use;

   ----------------
   -- Remove_Use --
   ----------------

   procedure Remove_Use (Idx : Flow_Idx) is
   begin
      if Present (Idx) then
         Flows.Table (Idx).Use_Count := Flows.Table (Idx).Use_Count - 1;
      end if;

   end Remove_Use;

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next (Idx, Nidx : Flow_Idx) is
   begin
      Remove_Use (Next (Idx));
      Add_Use (Nidx);
      Flows.Table (Idx).Next := Nidx;
   end Set_Next;

   -------------------
   -- Set_Is_Return --
   -------------------

   procedure Set_Is_Return (Idx : Flow_Idx; B : Boolean := True) is
   begin
      Flows.Table (Idx).Is_Return := B;
   end Set_Is_Return;

   ----------------------
   -- Set_Return_Value --
   ----------------------

   procedure Set_Return_Value (Idx : Flow_Idx; S : Str) is
   begin
      Flows.Table (Idx).Return_Value := S;
   end Set_Return_Value;

   ------------------
   -- Set_First_If --
   ------------------

   procedure Set_First_If (Idx : Flow_Idx; Iidx : If_Idx) is
   begin
      Flows.Table (Idx).First_If := Iidx;
   end Set_First_If;

   -----------------
   -- Set_Last_If --
   -----------------

   procedure Set_Last_If (Idx : Flow_Idx; Iidx : If_Idx) is
   begin
      Flows.Table (Idx).Last_If := Iidx;
   end Set_Last_If;

   -------------------
   -- Set_Case_Expr --
   -------------------

   procedure Set_Case_Expr (Idx : Flow_Idx; S : Str) is
   begin
      Flows.Table (Idx).Case_Expr := S;
   end Set_Case_Expr;

   --------------------
   -- Set_First_Case --
   --------------------

   procedure Set_First_Case (Idx : Flow_Idx; Cidx : Case_Idx) is
   begin
      Flows.Table (Idx).First_Case := Cidx;
   end Set_First_Case;

   -------------------
   -- Set_Last_Case --
   -------------------

   procedure Set_Last_Case (Idx : Flow_Idx; Cidx : Case_Idx) is
   begin
      Flows.Table (Idx).Last_Case := Cidx;
   end Set_Last_Case;

   --------------
   -- Add_Line --
   --------------

   procedure Add_Line (S : Str; V : Value_T) is
      Idx : Line_Idx;

   begin
      --  If we've been given an instruction corresponding to this
      --  statement and it has side-effects, first flush any pending
      --  assignments.

      if Present (V) and then Has_Side_Effects (V) then
         Process_Pending_Values;
      end if;

      --  Then add this line to the current flow, make sure that it's
      --  consecutive to a previous line if any.

      Idx := New_Line (S, V);
      if No (First_Line (Current_Flow)) then
         Set_First_Line (Current_Flow, Idx);
      else
         pragma Assert (Idx = Last_Line (Current_Flow) + 1);
      end if;

      Set_Last_Line (Current_Flow, Idx);
   end Add_Line;

   --------------
   -- New_Line --
   --------------

   function New_Line (S : Str; V : Value_T) return Line_Idx is
   begin
      Lines.Append ((Text => S, Inst => V));
      return Lines.Last;
   end New_Line;

   --------------
   -- New_Case --
   --------------

   function New_Case (V : Value_T; S : Str) return Case_Idx is
   begin
      Cases.Append ((Value  => V,
                     Expr   => S,
                     Target => Empty_Flow_Idx,
                     Inst   => No_Value_T));
      return Cases.Last;
   end New_Case;

   ------------
   -- New_If --
   ------------

   function New_If (S : Str; Inst : Value_T) return If_Idx is
   begin
      Ifs.Append ((Test => S, Inst => Inst, Target => Empty_Flow_Idx));
      return Ifs.Last;
   end New_If;

   ------------------------
   -- Get_Or_Create_Flow --
   ------------------------

   function Get_Or_Create_Flow (V : Value_T) return Flow_Idx is
     (Get_Or_Create_Flow (Value_As_Basic_Block (V)));

   ------------------------
   -- Get_Or_Create_Flow --
   ------------------------

   function Get_Or_Create_Flow (B : Basic_Block_T) return Flow_Idx is
      T   : constant Value_T  := Get_Basic_Block_Terminator (B);
      Idx : Flow_Idx          := Get_Flow (B);
      V   : Value_T           := Get_First_Instruction (B);

   begin
      --  If we already made a flow for this block, return it.

      if Present (Idx) then
         return Idx;
      end if;

      --  Otherwise, make a new flow and set it as the flow for this block
      --  and as the current flow.

      Flows.Append ((Is_Return    => False,
                     Return_Value => No_Str,
                     BB           => B,
                     First_Line   => Empty_Line_Idx,
                     Last_Line    => Empty_Line_Idx,
                     Use_Count    => 0,
                     Next         => Empty_Flow_Idx,
                     First_If     => Empty_If_Idx,
                     Last_If      => Empty_If_Idx,
                     Case_Expr    => No_Str,
                     First_Case   => Empty_Case_Idx,
                     Last_Case    => Empty_Case_Idx));
      Idx := Flows.Last;
      Set_Flow (B, Idx);
      Current_Flow := Idx;

      --  Add all instructions to the flow

      while V /= T loop
         Process_Instruction (V);
         V := Get_Next_Instruction (V);
      end loop;

      --  Finally, process the various types of terminators. We need to write
      --  pending values after we've built the strings for any expressions
      --  in ther terminator.

      case Get_Opcode (T) is
         when Op_Ret =>
            declare
               use Return_Maps;
               Retval   : Value_T := No_Value_T;
               Ret_Idx  : Flow_Idx;
               Position : Cursor;

            begin
               --  If this function returns and it returns a value,
               --  we'll need to make a return flow coresponding to that
               --  value. Otherwise, we use a flow with no value.

               if not Does_Not_Return (Curr_Func)
                 and then Get_Num_Operands (T) = 1
               then
                  Retval := Get_Operand0 (T);
                  Maybe_Decl (Retval);

                  --  If we're returning an array, declare a value (we can
                  --  use T even though its LLVM type is void) of the
                  --  struct type corresponding to Retval's type, assign Retval
                  --  into its only field (which will be done with a
                  --  memmove), and return that value.

                  if Get_Type_Kind (Retval) = Array_Type_Kind then
                     Output_Decl (TP ("#T1_R #2", Retval, T), V => T);
                     Write_Copy (T & ".F", Retval, Type_Of (Retval));
                     Retval := T;
                  end if;
               end if;

               --  If we already have a return flow for this value, use
               --  it. Otherwise, make one and record it.

               Position := Find (Return_Map, Retval);
               if Has_Element (Position) then
                  Ret_Idx := Element (Position);
               else
                  Flows.Append ((Is_Return    => True,
                                 Return_Value =>
                                   (if   Present (Retval) then +Retval
                                    else No_Str),
                                 BB           => No_BB_T,
                                 First_Line   => Empty_Line_Idx,
                                 Last_Line    => Empty_Line_Idx,
                                 Use_Count    => 0,
                                 Next         => Empty_Flow_Idx,
                                 First_If     => Empty_If_Idx,
                                 Last_If      => Empty_If_Idx,
                                 Case_Expr    => No_Str,
                                 First_Case   => Empty_Case_Idx,
                                 Last_Case    => Empty_Case_Idx));
                  Ret_Idx := Flows.Last;
                  Insert (Return_Map, Retval, Ret_Idx);
               end if;

               Set_Next (Idx, Ret_Idx);
               Process_Pending_Values;
            end;

         when Op_Br =>

            --  For a conditional branch, create two if entries, one for
            --  the true and false branch. But make those flows in a second
            --  pass to be sure that our entries are consecutive.

            if Is_Conditional (T) then
               declare
                  Test  : constant Value_T := Get_Operand0 (T);
                  Iidx1 : If_Idx;
                  Iidx2 : If_Idx;

               begin
                  Iidx1 := New_If (+Test, T);
                  Iidx2 := New_If (No_Str, T);
                  Maybe_Decl   (Test);
                  Process_Pending_Values;
                  Set_First_If (Idx, Iidx1);
                  Set_Last_If  (Idx, Iidx2);
                  Set_Target   (Iidx1, Get_Or_Create_Flow (Get_Operand2 (T)));
                  Set_Target   (Iidx2, Get_Or_Create_Flow (Get_Operand1 (T)));
               end;
            else
               Set_Next (Idx, Get_Or_Create_Flow (Get_Operand0 (T)));
               Process_Pending_Values;
            end if;

         when Op_Switch =>

            declare
               Val       : constant Value_T                := Get_Operand0 (T);
               POO       : constant Process_Operand_Option :=
                 (if Get_Is_Unsigned (Val) then POO_Unsigned else POO_Signed);
               Last_Case : constant Nat                    :=
                 Get_Num_Operands (T) / 2 - 1;
               Result    : Str                             :=
                 Process_Operand (Val, POO);
               Cidx      : Case_Idx                        :=
                 New_Case (No_Value_T, No_Str);

            begin
               Maybe_Decl (Val);

               --  If Val is narrower than int, we must force it to its size

               if Get_Scalar_Bit_Size (Val) < Int_Size then
                  Result := TP ("(#T1) ", Val) & (Result + Unary);
               end if;

               --  Process pending values and set the case expression and
               --  the first case data.

               Process_Pending_Values;
               Set_Case_Expr  (Idx, Result);
               Set_First_Case (Idx, Cidx);

               --  Now make case data for each alternative

               for J in 1 .. Last_Case loop
                  declare
                     Value : constant Value_T := Get_Operand (T, J * 2);

                  begin
                     Cidx := New_Case (Value, Process_Operand (Value, POO));
                  end;
               end loop;

               --  Finally, set the last case and all the targets of the
               --  default and alternative choices.

               Set_Last_Case (Idx, Cidx);
               Cidx := First_Case (Idx);
               Set_Target (Cidx, Get_Or_Create_Flow (Get_Operand1 (T)));
               Set_Inst   (Cidx, Get_First_Instruction (BB (Target (Cidx))));
               for J in 1 .. Last_Case loop
                  Cidx := Cidx + 1;
                  Set_Target
                    (Cidx, Get_Or_Create_Flow (Get_Operand (T, J * 2 + 1)));
                  Set_Inst (Cidx, Get_First_Instruction (BB (Target (Cidx))));
               end loop;
            end;

         when others =>
            pragma Assert (False);
      end case;

      --  Finally, return the new Flow we constructed

      return Idx;
   end Get_Or_Create_Flow;

   -----------------
   -- Case_Before --
   -----------------

   function Case_Before (L, R : Case_Idx) return Boolean is
      L_Inst : constant Value_T              := Inst (L);
      R_Inst : constant Value_T              := Inst (R);
      L_Line : constant Physical_Line_Number := Get_Debug_Loc_Line (L_Inst);
      R_Line : constant Physical_Line_Number := Get_Debug_Loc_Line (R_Inst);

   begin
      --  The two filenames should be the same, but just in case, use
      --  that as a sort criteria too.

      if Get_Debug_Loc_Filename (L_Inst) < Get_Debug_Loc_Filename (R_Inst) then
         return True;
      elsif Get_Debug_Loc_Filename (L_Inst) /= Get_Debug_Loc_Filename (R_Inst)
      then
         return False;

      --  Next, compare the line numbers of the target

      elsif L_Line < R_Line then
         return True;
      elsif L_Line /= R_Line then
         return False;

      --  If one is "default", put that last

      elsif No (Value (L)) then
         return False;
      elsif No (Value (R)) then
         return True;

      --  Otherwise, put the values in order

      else
         return Equals_Int (Const_I_Cmp (Int_SLE, Value (L), Value (R)), 1);
      end if;
   end Case_Before;

   ----------------
   -- Swap_Cases --
   ----------------

   procedure Swap_Cases (L, R : Case_Idx) is
      Temp : constant Case_Data := Cases.Table (L);

   begin
      Cases.Table (L) := Cases.Table (R);
      Cases.Table (R) := Temp;
   end Swap_Cases;

   -------------------
   -- Process_Flows --
   -------------------

   procedure Process_Flows (Idx : Flow_Idx; Subprog : Process_Subprogram) is
      procedure Process_One_Flow (Idx : Flow_Idx);
      --  Do one recursive call is Idx is Present and wasn't processed

      procedure Process_Flow_Graph (Idx : Flow_Idx)
        with Pre => Present (Idx);
      --  Proces all flows nested in Idx

      package Processed_Flows is new Ada.Containers.Ordered_Sets
        (Element_Type => Flow_Idx,
         "<"          => "<",
         "="          => "=");
      use Processed_Flows;
      Processed : Set;

      ----------------------
      -- Process_One_Flow --
      ----------------------

      procedure Process_One_Flow (Idx : Flow_Idx) is
      begin
         if No (Idx) or else Contains (Processed, Idx) then
            return;
         else
            Insert (Processed, Idx);
            Process_Flow_Graph (Idx);
         end if;
      end Process_One_Flow;

      ------------------------
      -- Process_Flow_Graph --
      ------------------------

      procedure Process_Flow_Graph (Idx : Flow_Idx) is
      begin
         Process_One_Flow (Next (Idx));
         if Present (First_If (Idx)) then
            for Iidx in First_If (Idx) .. Last_If (Idx) loop
               Process_One_Flow (Target (Iidx));
            end loop;
         end if;

         if Present (Case_Expr (Idx)) then
            for Cidx in First_Case (Idx) .. Last_Case (Idx) loop
               Process_One_Flow (Target (Cidx));
            end loop;
         end if;

         Subprog.all (Idx);
      end Process_Flow_Graph;

   begin  --  Start of processing for Process_Flow

      Process_Flow_Graph (Idx);
   end Process_Flows;

   ------------------
   -- Final_Target --
   ------------------

   function Final_Target (Idx : Flow_Idx) return Flow_Idx is
      Max_Count : constant := 10;
      Count     : Nat      := 0;
      --  To protect against infinite loops in the source, set a maximum
      --  count for how many flows we'll follow.

   begin
      return Final : Flow_Idx := Idx do
         while Present (Final) and then not Is_Return (Final)
           and then No (First_Line (Final)) and then No (First_If (Final))
           and then No (Case_Expr (Final)) and then Count < Max_Count
         loop
            Final := Next (Final);
            Count := Count + 1;
         end loop;
      end return;
   end Final_Target;

   ---------------------------
   -- Simplify_Final_Target --
   ---------------------------

   procedure Simplify_Final_Target (Idx : Flow_Idx) is
   begin
      --  Replace all targets in Idx with their final targets

      Set_Next (Idx, Final_Target (Next (Idx)));
      if Present (First_If (Idx)) then
         for Iidx in First_If (Idx) .. Last_If (Idx) loop
            Set_Target (Iidx, Final_Target (Target (Iidx)));
         end loop;
      end if;

      if Present (Case_Expr (Idx)) then
         for Cidx in First_Case (Idx) .. Last_Case (Idx) loop
            Set_Target (Cidx, Final_Target (Target (Cidx)));
         end loop;
      end if;
   end Simplify_Final_Target;

   -------------------------
   -- Sort_One_Flow_Cases --
   -------------------------

   procedure Simplify_One_Flow_Cases (Idx : Flow_Idx) is
   begin
      --  If we have case parts, first sort them

      if Present (Case_Expr (Idx)) then
         Case_Sort (First_Case (Idx), Last_Case (Idx));

         --  Now loop through them and remove the target for a case part
         --  if it's the same as the target of the next part. Note that we
         --  can't sort the cases once this has been done.

         for Cidx in First_Case (Idx) .. Last_Case (Idx) - 1 loop
            if Target (Cidx) = Target (Cidx + 1) then
               Set_Target (Cidx, Empty_Flow_Idx);
            end if;
         end loop;
      end if;

   end Simplify_One_Flow_Cases;

   --------------------
   -- Effective_Flow --
   -------------------

   function Effective_Flow (Idx : Flow_Idx) return Flow_Idx is
      Max_Count : constant := 10;
      Count     : Integer  := 0;

   begin
      --  Follow the Next chain, but avoid infinite loops

      return Ret_Idx : Flow_Idx := Idx do
         while Count < Max_Count and then Present (Next (Ret_Idx)) loop
            Ret_Idx := Next (Ret_Idx);
            Count   := Count + 1;
         end loop;
      end return;
   end Effective_Flow;

   -------------------
   -- Try_Merge_Ifs --
   -------------------

   procedure Try_Merge_Ifs (Idx : Flow_Idx) is
      New_First   : constant If_Idx := Ifs.Last + 1;
      Then_Target : Flow_Idx;
      Else_Target : Flow_Idx;

   begin
      --  If this doesn't have "if" parts, we can't do anything with it

      if No (First_If (Idx)) then
         return;
      end if;

      --  Otherwise, get our "then" and "else" targets. We can do the
      --  merge if the latter has only one use, has no code, has only
      --  two "if" parts, and the destination of the "then" part is
      --  either our "then" target or one or both are return flows.

      Then_Target := Target (First_If (Idx));
      Else_Target := Target (Last_If (Idx));
      if Use_Count (Else_Target) = 1 and then No (First_Line (Else_Target))
        and then Present (First_If (Else_Target))
        and then First_If (Else_Target) + 1 = Last_If (Else_Target)
        and then (Is_Return (Effective_Flow (Then_Target))
                    or else Is_Return (Effective_Flow
                                         (Target (First_If (Else_Target))))
                    or else Effective_Flow (Then_Target) =
                              Effective_Flow (Target (First_If (Else_Target))))
      then
         --  Copy all but the last of our "if" targets, then the two from
         --  our "else" target.

         for Iidx in First_If (Idx) .. Last_If (Idx) - 1 loop
            Ifs.Append (Ifs.Table (Iidx));
         end loop;

         for Iidx in First_If (Else_Target) .. Last_If (Else_Target) loop
            Ifs.Append (Ifs.Table (Iidx));
         end loop;

         --  Update our first and last indexes and make a recursive call
         --  in case we can merge more parts into this.

         Set_First_If (Idx, New_First);
         Set_Last_If  (Idx, Ifs.Last);
         Try_Merge_Ifs (Idx);
      end if;
   end Try_Merge_Ifs;

   -------------------
   -- Simplify_Flow --
   -------------------

   procedure Simplify_Flow (Idx : Flow_Idx) is
   begin
      Process_Flows (Idx, Simplify_Final_Target'Access);
      Process_Flows (Idx, Simplify_One_Flow_Cases'Access);
      Process_Flows (Idx, Try_Merge_Ifs'Access);
   end Simplify_Flow;

   -----------------
   -- Output_Flow --
   -----------------

   procedure Output_Flow (Idx : Flow_Idx) is
      procedure Output_Flow_Target
        (Idx : Flow_Idx; V : Value_T; BS : Block_Style; Depth : Nat)
        with Pre => Present (Idx) and then Present (V);
      --  Write the line(s) needed to go to the flow denoted by Idx from
      --  the instruction V. BS is the style of this block, if any

      procedure Output_One_Flow
        (Idx : Flow_Idx; Depth : Nat := 0; Write_Label : Boolean := True);
      --  Output the flow for Idx, if Present, and all nested flows,
      --  possibly omitting the starting label. We track the nesting depth to
      --  make sure it doesn't get too deep.

      package Output_Flows is new Ada.Containers.Ordered_Sets
        (Element_Type => Flow_Idx,
         "<"          => "<",
         "="          => "=");
      use Output_Flows;
      To_Output : Set;
      Output    : Set;

      ------------------------
      -- Output_Flow_Target --
      ------------------------

      procedure Output_Flow_Target
        (Idx : Flow_Idx; V : Value_T; BS : Block_Style; Depth : Nat) is
      begin
         Start_Output_Block (BS);

         --  If this represents a return, write a return

         if Is_Return (Idx) then
            if Present (Return_Value (Idx)) then
               Output_Stmt ("return " & Return_Value (Idx), V => V);
            elsif not Does_Not_Return (Curr_Func) then
               Output_Stmt ("return", V => V);
            end if;

            --  If this is a flow with only one use (so it must be this one),
            --  output that flow directly unless we're already too deep

         elsif Use_Count (Idx) = 1 and then Depth < Max_Depth then
            Output_One_Flow (Idx, Write_Label => False, Depth => Depth);

         --  Otherwise, write a goto and mark it for output

         else
            Output_Stmt ("goto " & BB (Idx), V => V);
            if not Is_Return (Idx) and then not Contains (Output, Idx)
              and then not Contains (To_Output, Idx)
            then
               Insert (To_Output, Idx);
            end if;
         end if;

         End_Stmt_Block (BS);
      end Output_Flow_Target;

      ---------------------
      -- Output_One_Flow --
      ---------------------

      procedure Output_One_Flow
        (Idx : Flow_Idx; Depth : Nat := 0; Write_Label : Boolean := True)
      is
         T : Value_T;

      begin
         Insert (Output, Idx);
         T := Get_Basic_Block_Terminator (BB (Idx));

         --  Write the block's label, if requested

         if Write_Label then
            Output_Stmt (BB (Idx) & ":",
                         Semicolon   => False,
                         Indent_Type => Left,
                         V           => Get_First_Instruction (BB (Idx)));
         end if;

         --  Now process lines in the flow, if any

         if Present (First_Line (Idx)) then
            for Lidx in First_Line (Idx) .. Last_Line (Idx) loop
               Output_Stmt (Text (Lidx), V => Inst (Lidx));
            end loop;
         end if;

         --  Next process any "if" parts in the flow

         if Present (First_If (Idx)) then
            for Iidx in First_If (Idx) .. Last_If (Idx) loop
               if Present (Test (Iidx)) then
                  Output_Stmt ((if Iidx = First_If (Idx) then "" else "else ")
                                & "if (" & Test (Iidx) & ")",
                               V         => Inst (Iidx),
                               Semicolon => False);
               else
                  Output_Stmt ("else", V => Inst (Iidx), Semicolon => False);
               end if;

               Output_Flow_Target (Target (Iidx), Inst (Iidx),
                                   BS    => If_Part,
                                   Depth => Depth + 1);
            end loop;
         end if;

         --  Now any Case parts

         if Present (Case_Expr (Idx)) then
            Output_Stmt ("switch (" & Case_Expr (Idx) & ")",
                         V         => T,
                         Semicolon => False);
            Start_Output_Block (Switch);
            for Cidx in First_Case (Idx) .. Last_Case (Idx) loop
               if Present (Value (Cidx)) then
                  Output_Stmt ("case " & Expr (Cidx) & ":",
                               Semicolon   => False,
                               Indent_Type => Under_Brace);
               else
                  Output_Stmt ("default:",
                               Semicolon   => False,
                               Indent_Type => Under_Brace);
               end if;

               if Present (Target (Cidx)) then
                  Output_Flow_Target (Target (Cidx), T,
                                      BS    => None,
                                      Depth => Depth + 1);
               end if;
            end loop;

            End_Stmt_Block (Switch);
         end if;

         --  The final thing to output is any jump at the end

         if Present (Next (Idx)) then
            Output_Flow_Target (Next (Idx), T, BS => None, Depth => Depth);
         end if;

         --  And separate flows

         Output_Stmt ("", Semicolon => False);
      end Output_One_Flow;

   begin  -- Start of processing for Output_Flow

      --  Output the top-level flow

      Output_One_Flow (Idx, Depth => 0, Write_Label => False);

      --  Now loop while there are still flows to output

      while not Is_Empty (To_Output) loop
         declare
            Output_Idx : constant Flow_Idx := First_Element (To_Output);

         begin
            Output_One_Flow (Output_Idx, Depth => 0);
            Delete (To_Output, Output_Idx);
         end;
      end loop;

   end Output_Flow;

   ---------------
   -- Dump_Flow --
   ---------------

   procedure Dump_Flow (J : Pos; Dump_All : Boolean) is
      procedure Write_Flow_Idx (Idx : Flow_Idx)
        with Pre => Present (Idx);
      procedure Dump_One_Flow (Idx : Flow_Idx)
        with Pre => Present (Idx);
      package Dump_Flows is new Ada.Containers.Ordered_Sets
        (Element_Type => Flow_Idx,
         "<"          => "<",
         "="          => "=");
      use Dump_Flows;
      To_Dump : Set;
      Dumped  : Set;
      LB      : constant Pos      := Pos (Flow_Idx_Low_Bound);
      Idx     : constant Flow_Idx := Flow_Idx ((if J < LB then J + LB else J));
      --  To simplify its use, this can be called either with the actual
      --  Flow_Idx value or a smaller integer which represents the low-order
      --  digits of the value.

      --------------------
      -- Write_Flow_Idx --
      --------------------

      procedure Write_Flow_Idx (Idx : Flow_Idx) is
      begin
         Write_Int (Pos (Idx));
         if Is_Return (Idx) then
            Write_Str (" (return");
            if Present (Return_Value (Idx)) then
               Write_Str (" " & Return_Value (Idx));
            end if;

            Write_Str (")");
         end if;

         --  If we haven't already dumped this flow and haven't
         --  already indicated we need to, show that we may need to

         if not Contains (Dumped, Idx)
           and then not Contains (To_Dump, Idx)
         then
            Insert (To_Dump, Idx);
         end if;
      end Write_Flow_Idx;

      -------------------
      -- Dump_One_Flow --
      -------------------

      procedure Dump_One_Flow (Idx : Flow_Idx) is
      begin
         if Is_Return (Idx) then
            Write_Str ("Flow " & Pos (Idx) & " has " & Use_Count (Idx));
         else
            Write_Str  ("Flow " & Pos (Idx) & " (" & BB (Idx) &
                          ") has " & Use_Count (Idx));
         end if;

         if Use_Count (Idx) = 1 then
            Write_Str (" use");
         else
            Write_Str (" uses");
         end if;

         if Is_Return (Idx) then
            Write_Str (" return");
            if Present (Return_Value (Idx)) then
               Write_Str (" " & Return_Value (Idx));
            end if;
         elsif Present (Next (Idx)) then
            Write_Str (" next ");
            Write_Flow_Idx (Next (Idx));
         end if;

         if Present (First_Line (Idx)) or else Present (First_If (Idx))
           or else Present (Case_Expr (Idx))
         then
            Write_Str (":");
         end if;

         Write_Eol;

         if Present (First_Line (Idx)) then
            for Lidx in First_Line (Idx) .. Last_Line (Idx) loop
               Write_Str ("    " & Text (Lidx), Eol => True);
            end loop;
         end if;

         if Present (First_If (Idx)) then
            for Iidx in First_If (Idx) .. Last_If (Idx) loop
               if Present (Test (Iidx)) then
                  Write_Str ("    if (" & Test (Iidx) & ") then ");
               else
                  Write_Str ("    else ");
               end if;

               Write_Flow_Idx (Target (Iidx));
               Write_Eol;
            end loop;
         end if;

         if Present (Case_Expr (Idx)) then
            Write_Str ("  switch (" & Case_Expr (Idx) & ")");
            Write_Eol;
            for Cidx in First_Case (Idx) .. Last_Case (Idx) loop
               if Present (Value (Cidx)) then
                  Write_Str ("    " & Value (Cidx) & ":");
               else
                  Write_Str ("    default:");
               end if;

               if Present (Target (Cidx)) then
                  Write_Str (" goto ");
                  Write_Flow_Idx (Target (Cidx));
               end if;

               Write_Eol;
            end loop;
         end if;
      end Dump_One_Flow;

   begin  --  Start of processing for Dump_Flow
      Push_Output;
      Set_Standard_Error;

      --  Dump the flow we're asked to dump. If we're to dump nested flow,
      --  keep dumping until all are done.

      Insert (Dumped, Idx);
      Dump_One_Flow (Idx);
      if Dump_All then
         while not Is_Empty (To_Dump) loop
            declare
               Dump_Idx : constant Flow_Idx := First_Element (To_Dump);

            begin
               --  We don't dump return flows since we see them when they are
               --  referenced. But pretend that we did.

               if not Is_Return (Dump_Idx) then
                  Dump_One_Flow (Dump_Idx);
               end if;

               Delete (To_Dump, Dump_Idx);
               Insert (Dumped, Dump_Idx);
            end;
         end loop;
      end if;

      Pop_Output;
   end Dump_Flow;

   ---------------------
   -- Maybe_Dump_Flow --
   ---------------------

   procedure Maybe_Dump_Flow (Idx : Flow_Idx; V : Value_T; Desc : String) is
   begin
      if Debug_Flag_Underscore_U then
         Push_Output;
         Set_Standard_Error;
         Write_Eol;
         Write_Str (Desc & " flows for " & V, Eol => True);
         Pop_Output;
         Dump_Flow (Pos (Idx), True);
      end if;
   end Maybe_Dump_Flow;

begin
   --  Ensure we have an empty entry in the tables

   Lines.Increment_Last;
   Cases.Increment_Last;
   Ifs.Increment_Last;
   Flows.Increment_Last;

end CCG.Flow;
