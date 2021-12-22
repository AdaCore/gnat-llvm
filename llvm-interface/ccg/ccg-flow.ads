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

with LLVM.Core; use LLVM.Core;

with CCG.Environment; use CCG.Environment;
with CCG.Helper;      use CCG.Helper;
with CCG.Output;      use CCG.Output;
with CCG.Strs;        use CCG.Strs;

package CCG.Flow is

   --  This package contains the processing for Flows. We define a Flow
   --  as a piece of C code corresponding to a control structure in a
   --  subprogram. This can be piece of straight-line code that continues
   --  to another Flow, an if/then/elseif/else block, a switch statement,
   --  or a loop (not handled yet).
   --
   --  We could create a discriminated variant record to record a Flow,
   --  but it's simpler to use three tables to represent this information.
   --  We use one table to contain information about one part of an "if",
   --  a second to contains information about one case of a switch
   --  statement, and the final table to represent a Flow itself.

   Line_Idx_Low_Bound  : constant := 600_000_000;
   Line_Idx_High_Bound : constant := 699_999_999;
   type Line_Idx is range Line_Idx_Low_Bound .. Line_Idx_High_Bound;
   Empty_Line_Idx      : constant Line_Idx := Line_Idx_Low_Bound;

   Case_Idx_Low_Bound  : constant := 700_000_000;
   Case_Idx_High_Bound : constant := 799_999_999;
   type Case_Idx is range Case_Idx_Low_Bound .. Case_Idx_High_Bound;
   Empty_Case_Idx      : constant Case_Idx := Case_Idx_Low_Bound;

   If_Idx_Low_Bound    : constant := 800_000_000;
   If_Idx_High_Bound   : constant := 899_999_999;
   type If_Idx is range If_Idx_Low_Bound .. If_Idx_High_Bound;
   Empty_If_Idx        : constant If_Idx := If_Idx_Low_Bound;

   function Present (Idx : Line_Idx) return Boolean is (Idx /= Empty_Line_Idx);
   function Present (Idx : Case_Idx) return Boolean is (Idx /= Empty_Case_Idx);
   function Present (Idx : If_Idx)   return Boolean is (Idx /= Empty_If_Idx);

   function No (Idx : Line_Idx) return Boolean is (Idx = Empty_Line_Idx);
   function No (Idx : Case_Idx) return Boolean is (Idx = Empty_Case_Idx);
   function No (Idx : If_Idx)   return Boolean is (Idx = Empty_If_Idx);

   --  Getters and setters for a Line node

   function Text (Idx : Line_Idx)   return Str
     with Pre => Present (Idx), Post => Present (Text'Result);
   --  The string containing the line to be written

   function Inst (Idx : Line_Idx)   return Value_T
     with Pre => Present (Idx), Post => Is_A_Instruction (Inst'Result);
      --  The instruction corresponding to the line (for debug data)

   procedure Set_Text (Idx : Line_Idx; S : Str)
     with Pre  => Present (Idx) and then Present (S),
          Post => Text (Idx) = S, Inline;

   procedure Set_Inst (Idx : Line_Idx; V : Value_T)
     with Pre  => Present (Idx) and then Is_A_Instruction (V),
          Post => Inst (Idx) = V, Inline;

   --  Getters and setters for a Case node

   function Value (Idx : Case_Idx)  return Value_T
     with Pre => Present (Idx);
   --  Return the integer value for this case node or No_Value_T if this
   --  is for "default".

   function Target (Idx : Case_Idx) return Flow_Idx
     with Pre => Present (Idx);
   --  Return the Flow corresponding to this case node

   procedure Set_Value (Idx : Case_Idx; V : Value_T)
     with Pre  => Present (Idx) and then Present (V),
          Post => Value (Idx) = V, Inline;

   procedure Set_Target (Idx : Case_Idx; Fidx : Flow_Idx)
     with Pre  => Present (Idx) and then Present (Fidx),
          Post => Target (Idx) = Fidx, Inline;

   --  Getters and setters for an If node

   function Test (Idx : If_Idx)   return Value_T
     with Pre => Present (Idx);
   --  Return the expression corresponding to the test, if Present. If
   --  not, this represents an "else".

   function Inst (Idx : If_Idx)   return Value_T
     with Pre  => Present (Idx),
          Post => No (Inst'Result) or else Is_A_Instruction (Inst'Result);
   --  Instruction that does test (for debug info), if any

   function Target (Idx : If_Idx) return Flow_Idx
     with Pre => Present (Idx);
   --  Destination if this test is true (or not Present)

   procedure Set_Test (Idx : If_Idx; V : Value_T)
     with Pre  => Present (Idx) and then Present (V),
          Post => Test (Idx) = V, Inline;

   procedure Set_Inst (Idx : If_Idx; V : Value_T)
     with Pre  => Present (Idx) and then Is_A_Instruction (V),
          Post => Inst (Idx) = V, Inline;

   procedure Set_Target (Idx : If_Idx; Fidx : Flow_Idx)
     with Pre  => Present (Idx) and then Present (Fidx),
          Post => Target (Idx) = Fidx, Inline;

   --  Getters and setters for a Flow

   function BB (Idx : Flow_Idx)           return Basic_Block_T
     with Pre => not Is_Return (Idx), Post => Present (BB'Result);
   --  Block corresponding to this flow, if not a return flow

   function First_Line (Idx : Flow_Idx)   return Line_Idx
     with Pre => Present (Idx);
   --  First line that's part of this flow, if any

   function Last_Line (Idx : Flow_Idx)    return Line_Idx
     with Pre => Present (Idx);
   --  Last line that's part of this flow, if any

   function Use_Count (Idx : Flow_Idx)    return Nat
     with Pre => Present (Idx);
   --  Number of times this flow is referenced by another flow (always one
   --  for the entry block).

   function Next (Idx : Flow_Idx)         return Flow_Idx
     with Pre => Present (Idx);
   --  Next flow executed after this one has completed, if any

   function Is_Return (Idx : Flow_Idx)    return Boolean
     with Pre => Present (Idx);
   --  True for a return flow

   function Return_Value (Idx : Flow_Idx) return Value_T
     with Pre => Is_Return (Idx) and then Present (Idx);
   --  If a return flow, the value to return, if any

   function First_If (Idx : Flow_Idx)     return If_Idx
     with Pre => Present (Idx);
   function Last_If (Idx : Flow_Idx)      return If_Idx
     with Pre => Present (Idx);
   --  First and last if/then/elseif/else parts, if any

   function Case_Expr (Idx : Flow_Idx)    return Str
     with Pre => Present (Idx);
   --  Expression for switch statement, if any

   function First_Case (Idx : Flow_Idx)   return Case_Idx
     with Pre => Present (Idx);
   function Last_Case (Idx : Flow_Idx)    return Case_Idx
     with Pre => Present (Idx);
   --  First and last of cases for a switch statement, if any

   procedure Set_BB (Idx : Flow_Idx; B : Basic_Block_T)
     with Pre  => Present (Idx) and then Present (B),
          Post => BB (Idx) = B, Inline;

   procedure Set_First_Line (Idx : Flow_Idx; Lidx : Line_Idx)
     with Pre  => Present (Idx) and then Present (Lidx),
          Post => First_Line (Idx) = Lidx, Inline;
   procedure Set_Last_Line (Idx : Flow_Idx; Lidx : Line_Idx)
     with Pre  => Present (Idx) and then Present (Lidx),
          Post => Last_Line (Idx) = Lidx, Inline;

   procedure Set_Next (Idx, Nidx : Flow_Idx)
     with Pre  => Present (Idx), Post => Next (Idx) = Nidx, Inline;

   procedure Set_Is_Return (Idx : Flow_Idx; B : Boolean := True)
     with Pre  => Present (Idx), Post => Is_Return (Idx) = B, Inline;

   procedure Set_Return_Value (Idx : Flow_Idx; V : Value_T)
     with Pre  => Present (Idx) and then Is_Return (Idx) and then Present (V),
          Post => Return_Value (Idx) = V, Inline;

   procedure Set_First_If (Idx : Flow_Idx; Iidx : If_Idx)
     with Pre  => Present (Idx) and then Present (Iidx),
          Post => First_If (Idx) = Iidx, Inline;
   procedure Set_Last_If (Idx : Flow_Idx; Iidx : If_Idx)
     with Pre  => Present (Idx) and then Present (Iidx),
          Post => Last_If (Idx) = Iidx, Inline;

   procedure Set_Case_Expr (Idx : Flow_Idx; S : Str)
     with Pre  => Present (Idx) and then Present (S),
          Post => Case_Expr (Idx) = S, Inline;

   procedure Set_First_Case (Idx : Flow_Idx; Cidx : Case_Idx)
     with Pre  => Present (Idx) and then Present (Cidx),
          Post => First_Case (Idx) = Cidx, Inline;
   procedure Set_Last_Case (Idx : Flow_Idx; Cidx : Case_Idx)
     with Pre  => Present (Idx) and then Present (Cidx),
          Post => Last_Case (Idx) = Cidx, Inline;

   function Get_Or_Create_Flow (V : Value_T) return Flow_Idx
     with Pre  => Is_A_Basic_Block (V),
          Post => Present (Get_Or_Create_Flow'Result);
   function Get_Or_Create_Flow (BB : Basic_Block_T) return Flow_Idx
     with Pre  => Present (BB),
          Post => Present (Get_Or_Create_Flow'Result)
                  and then Get_Flow (BB) = Get_Or_Create_Flow'Result;
   --  Get (and create if needed) a Flow for a block

   procedure Add_Line (S : Str; V : Value_T)
     with Pre => Present (S) and then Present (V);
   --  Add a line and corresponding instruction to the current flow

   pragma Annotate (Xcov, Exempt_On, "Debug helper");

   procedure Dump_Flow (J : Pos; Dump_All : Boolean)
     with Export, External_Name => "dfl";
   --  Dump a flow to stderr. To simplify its use, this can be called
   --  either with the actual Flow_Idx value or a smaller integer which
   --  represents the low-order digits of the value.

   pragma Annotate (Xcov, Exempt_Off, "Debug helper");

end CCG.Flow;
