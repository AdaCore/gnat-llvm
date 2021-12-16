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

with Table;

package body CCG.Flow is

   --  First is a table containing pairs of switch statement values and
   --  targets.

   type Case_Data is record
      Value  : Value_T;
      --  Value corresponding to this case or No_Value_T for "default"

      Target : Flow_Idx;
      --  Destination for this value

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
      Test   : Value_T;
      --  Expression corresponding to the test, if Present. If not, this
      --  represents an "else".

      Inst   : Value_T;
      --  Instruction that does test (for debug info), if any

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
      First_Stmt : Stmt_Idx;
      --  First statement that's part of this flow, if any

      Last_Stmt  : Stmt_Idx;
      --  Last statement that's part of this flow, if any

      Next       : Flow_Idx;
      --  Next flow executed after this one has completed, if any

      Is_Return  : Boolean;
      --  This is set for the unique return flow

      First_If   : If_Idx;
      Last_If    : If_Idx;
      --  First and last if/then/elseif/else parts, if any

      Case_Expr  : Value_T;
      --  Expression for switch statement, if any

      First_Case : Case_Idx;
      Last_Case  : Case_Idx;
      --  First and last of cases for a switch statement, if any

   end record;

   package Flows is new Table.Table
     (Table_Component_Type => Flow_Data,
      Table_Index_Type     => Flow_Idx,
      Table_Low_Bound      => Flow_Idx_Low_Bound,
      Table_Initial        => 100,
      Table_Increment      => 200,
      Table_Name           => "Flows");

   -----------
   -- Value --
   -----------

   function Value (Idx : Case_Idx) return Value_T is
     (Cases.Table (Idx).Value);

   ------------
   -- Target --
   ------------

   function Target (Idx : Case_Idx) return Flow_Idx is
     (Cases.Table (Idx).Target);

   --------------
   -- Set_Value --
   --------------

   procedure Set_Value (Idx : Case_Idx; V : Value_T) is
   begin
      Cases.Table (Idx).Value := V;
   end Set_Value;

   ----------------
   -- Set_Target --
   ----------------

   procedure Set_Target (Idx : Case_Idx; Fidx : Flow_Idx) is
   begin
      Cases.Table (Idx).Target := Fidx;
   end Set_Target;

   ----------
   -- Test --
   ----------

   function Test (Idx : If_Idx) return Value_T is
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

   procedure Set_Test (Idx : If_Idx; V : Value_T) is
   begin
      Ifs.Table (Idx).Test := V;
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
      Ifs.Table (Idx).Target := Fidx;
   end Set_Target;

   ----------------
   -- First_Stmt --
   ----------------

   function First_Stmt (Idx : Flow_Idx) return Stmt_Idx is
     (Flows.Table (Idx).First_Stmt);

   ---------------
   -- Last_Stmt --
   ---------------

   function Last_Stmt (Idx : Flow_Idx) return Stmt_Idx is
     (Flows.Table (Idx).Last_Stmt);

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

   function Case_Expr (Idx : Flow_Idx)  return Value_T is
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

   --------------------
   -- Set_First_Stmt --
   --------------------

   procedure Set_First_Stmt (Idx : Flow_Idx; S : Stmt_Idx) is
   begin
      Flows.Table (Idx).First_Stmt := S;
   end Set_First_Stmt;

   -------------------
   -- Set_Last_Stmt --
   -------------------

   procedure Set_Last_Stmt (Idx : Flow_Idx; S : Stmt_Idx) is
   begin
      Flows.Table (Idx).Last_Stmt := S;
   end Set_Last_Stmt;

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next (Idx, Nidx : Flow_Idx) is
   begin
      Flows.Table (Idx).Next := Nidx;
   end Set_Next;

   -------------------
   -- Set_Is_Return --
   -------------------

   procedure Set_Is_Return (Idx : Flow_Idx; B : Boolean := True) is
   begin
      Flows.Table (Idx).Is_Return := B;
   end Set_Is_Return;

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

   procedure Set_Case_Expr (Idx : Flow_Idx; V : Value_T) is
   begin
      Flows.Table (Idx).Case_Expr := V;
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

begin
   --  Ensure we have an empty entry in the tables

   Cases.Increment_Last;
   Ifs.Increment_Last;
   Flows.Increment_Last;

end CCG.Flow;
