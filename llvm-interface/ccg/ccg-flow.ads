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

with CCG.Helper; use CCG.Helper;
with CCG.Output; use CCG.Output;
with CCG.Strs;   use CCG.Strs;

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

   Case_Idx_Low_Bound  : constant := 600_000_000;
   Case_Idx_High_Bound : constant := 699_999_999;
   type Case_Idx is range Case_Idx_Low_Bound .. Case_Idx_High_Bound;
   Empty_Case_Idx      : constant Case_Idx := Case_Idx_Low_Bound;

   If_Idx_Low_Bound    : constant := 700_000_000;
   If_Idx_High_Bound   : constant := 799_999_999;
   type If_Idx is range If_Idx_Low_Bound .. If_Idx_High_Bound;
   Empty_If_Idx        : constant If_Idx := If_Idx_Low_Bound;

   function Present (Idx : Case_Idx) return Boolean is (Idx /= Empty_Case_Idx);
   function Present (Idx : If_Idx)   return Boolean is (Idx /= Empty_If_Idx);

   function No (Idx : Case_Idx) return Boolean is (Idx = Empty_Case_Idx);
   function No (Idx : If_Idx)   return Boolean is (Idx = Empty_If_Idx);

   --  Getters and setters for a Case node

   function Value (Idx : Case_Idx)  return Value_T
     with Pre => Present (Idx);
   --  Return the integer value for this case node or No_Value_T if this
   --  is for "default".

   function Target (Idx : Case_Idx) return Flow_Idx
     with Pre => Present (Idx), Post => Present (Target'Result);
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
     with Pre => Present (Idx), Post => Present (Target'Result);
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

   function First_Stmt (Idx : Flow_Idx) return Stmt_Idx
     with Pre => Present (Idx);
   --  First statement that's part of this flow, if any

   function Last_Stmt (Idx : Flow_Idx)  return Stmt_Idx
     with Pre => Present (Idx);
   --  Last statement that's part of this flow, if any

   function Next (Idx : Flow_Idx)       return Flow_Idx
     with Pre => Present (Idx);
   --  Next flow executed after this one has completed, if any

   function Is_Return (Idx : Flow_Idx)  return Boolean
     with Pre => Present (Idx);
   --  True for the unique return flow

   function First_If (Idx : Flow_Idx)   return If_Idx
     with Pre => Present (Idx);
   function Last_If (Idx : Flow_Idx)    return If_Idx
     with Pre => Present (Idx);
   --  First and last if/then/elseif/else parts, if any

   function Case_Expr (Idx : Flow_Idx)  return Value_T
     with Pre => Present (Idx);
   --  Expression for switch statement, if any

   function First_Case (Idx : Flow_Idx) return Case_Idx
     with Pre => Present (Idx);
   function Last_Case (Idx : Flow_Idx)  return Case_Idx
     with Pre => Present (Idx);
   --  First and last of cases for a switch statement, if any

   procedure Set_First_Stmt (Idx : Flow_Idx; S : Stmt_Idx)
     with Pre  => Present (Idx) and then Present (S),
          Post => First_Stmt (Idx) = S, Inline;

   procedure Set_Last_Stmt (Idx : Flow_Idx; S : Stmt_Idx)
     with Pre  => Present (Idx) and then Present (S),
          Post => Last_Stmt (Idx) = S, Inline;

   procedure Set_Next (Idx, Nidx : Flow_Idx)
     with Pre  => Present (Idx) and then Present (Nidx),
          Post => Next (Idx) = Nidx, Inline;

   procedure Set_Is_Return (Idx : Flow_Idx; B : Boolean := True)
     with Pre  => Present (Idx), Post => Is_Return (Idx) = B, Inline;

   procedure Set_First_If (Idx : Flow_Idx; Iidx : If_Idx)
     with Pre  => Present (Idx) and then Present (Iidx),
          Post => First_If (Idx) = Iidx, Inline;

   procedure Set_Last_If (Idx : Flow_Idx; Iidx : If_Idx)
     with Pre  => Present (Idx) and then Present (Iidx),
          Post => Last_If (Idx) = Iidx, Inline;

   procedure Set_Case_Expr (Idx : Flow_Idx; V : Value_T)
     with Pre  => Present (Idx) and then Present (V),
          Post => Case_Expr (Idx) = V, Inline;

   procedure Set_First_Case (Idx : Flow_Idx; Cidx : Case_Idx)
     with Pre  => Present (Idx) and then Present (Cidx),
          Post => First_Case (Idx) = Cidx, Inline;

   procedure Set_Last_Case (Idx : Flow_Idx; Cidx : Case_Idx)
     with Pre  => Present (Idx) and then Present (Cidx),
          Post => Last_Case (Idx) = Cidx, Inline;

   pragma Annotate (Xcov, Exempt_On, "Debug helper");

   procedure Dump_Flow (J : Pos) with Export, External_Name => "dfl";
   --  Dump a flow to stderr. To simplify its use, this can be called
   --  either with the actual Flow_Idx value or a smaller integer which
   --  represents the low-order digits of the value.

   pragma Annotate (Xcov, Exempt_Off, "Debug helper");

end CCG.Flow;
