------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2023, AdaCore                     --
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

   procedure Discard (Idx : Line_Idx) is null;
   procedure Discard (Idx : Case_Idx) is null;
   procedure Discard (Idx : If_Idx)   is null;
   procedure Discard (Idx : Flow_Idx) is null;

   function Get_Or_Create_Flow (B : Basic_Block_T) return Flow_Idx
     with Pre  => Present (B),
          Post => Present (Get_Or_Create_Flow'Result)
                  and then Get_Flow (B) = Get_Or_Create_Flow'Result;
   function Get_Or_Create_Flow (V : Value_T) return Flow_Idx is
     (Get_Or_Create_Flow (Value_As_Basic_Block (V)))
   with Pre  => Is_A_Basic_Block (V),
        Post => Present (Get_Or_Create_Flow'Result);
   --  Get (and create if needed) a Flow for a block

   procedure Add_Use (Idx : Flow_Idx) with Inline;
   procedure Remove_Use (Idx : Flow_Idx) with Inline;
   --  Add or remove (respectively) a usage of the Flow denoted by Idx,
   --  if any. Because we remove the uses of anything that has zero
   --  uses, if we're moving a flow index from one location to another,
   --  be sure that we add it to the new place before removing it from
   --  the previous.

   procedure Add_Line
     (S          : Str;
      V          : Value_T;
      Force_Left : Boolean := False;
      Semicolon  : Boolean := True)
     with Pre => Present (S) and then Present (V);
   --  Add a line and corresponding instruction to the current flow

   procedure Simplify_Flow (Idx : Flow_Idx)
     with Pre => Present (Idx);
   --  Perform simplifications of Idx and the flows referenced by it

   procedure Output_Flow (Idx : Flow_Idx)
     with Pre => Present (Idx);
   --  Output the flow for Idx, if Present, and all nested flows

   procedure Maybe_Dump_Flow (Idx : Flow_Idx; V : Value_T; Desc : String)
     with Pre => Present (Idx) and then Present (V);
   --  Idx is the flow for the entry block of V. If -gnatd_u is specified,
   --  label the flow with Desc, and dump it.

   pragma Annotate (Xcov, Exempt_On, "Debug helper");

   procedure Dump_Flow (J : Pos; Dump_All : Boolean)
     with Export, External_Name => "dfl";
   --  Dump a flow to stderr. To simplify its use, this can be called
   --  either with the actual Flow_Idx value or a smaller integer which
   --  represents the low-order digits of the value.

   pragma Annotate (Xcov, Exempt_Off, "Debug helper");

end CCG.Flow;
