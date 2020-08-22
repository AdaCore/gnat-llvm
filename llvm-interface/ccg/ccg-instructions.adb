------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020, AdaCore                          --
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

with Interfaces.C; use Interfaces.C;

with LLVM.Core; use LLVM.Core;

with CCG.Output;      use CCG.Output;
with CCG.Subprograms; use CCG.Subprograms;

package body CCG.Instructions is

   function Num_Uses (V : Value_T) return Nat
     with Pre => Present (V), Post => Num_Uses'Result >= 1;
   --  Returns the number of uses of V

   procedure Assignment (LHS : Value_T; RHS : Str)
     with Pre => Present (LHS) and then Present (RHS);
   --  Take action to assign LHS the value RHS

   function Cmp_Instruction (V, Op1, Op2 : Value_T) return Str
     with Pre  => Get_Instruction_Opcode (V) in Op_I_Cmp | Op_F_Cmp
                  and then Present (Op1) and then Present (Op2),
          Post => Present (Cmp_Instruction'Result);
   --  Return the value corresponding to a comparison instruction

   procedure Call_Instruction (V : Value_T; Ops : Value_Array)
     with Pre => Present (V);
   --  Process a call instruction

   procedure Maybe_Decl (V : Value_T)
     with Pre => Present (V), Post => Get_Is_Decl_Output (V);
   --  See if we need to write a declaration for V and write one if so

   function Maybe_Unsigned
     (V : Value_T; Is_Unsigned : Boolean := True) return Str
   is
     ((if   Is_Unsigned then TP (" (unsigned #T) #1", V, T => Type_Of (V))
       else +V))
     with Pre => Present (V), Post => Present (Maybe_Unsigned'Result);
   --  Return V if it's not unsigned and return a cast to unsigned if it is.

   --------
   -- TP --
   --------

   function TP
     (S : String;
      Op1 : Value_T;
      Op2 : Value_T := No_Value_T;
      Op3 : Value_T := No_Value_T;
      T   : Type_T  := No_Type_T) return Str
   is
      Start     : Integer := S'First;
      Result    : Str     := No_Str;
      Mark_Seen : Boolean := False;
      B_Seen    : Boolean := False;
      D_Seen    : Boolean := False;
      Op        : Value_T;
      Last      : Integer;

   begin
      for J in S'Range loop

         --  If we've seen '#', look for 'B' or 'D'

         if Mark_Seen then
            if S (J) = 'B' then
               B_Seen := True;
            elsif S (J) = 'D' then
               D_Seen := True;

            --  If neither, then this is a number, representing which operand
            --  to output, possibly as modified by 'B' or 'D'.

            else
               Op := (case S (J) is when '1' => Op1, when '2' => Op2,
                                    when others => Op3);

               --  The end of any string to output is before our mark, which
               --  may be, e.g., #1 or #B2.

               Last := J - 2 - (if B_Seen or D_Seen then 1 else 0);
               if Start <= Last then
                  Result := Result & S (Start .. Last);
               end if;

               --  Output the (possibly modified) operand and reset for the
               --  next string and/or mark.

               if B_Seen then
                  Result := Result & Value_As_Basic_Block (Op);
               elsif D_Seen then
                  Result := Result & To_Data (Op);
               elsif S (J) = 'T' then
                  Result := Result & T;
               else
                  Result := Result & Op;
               end if;

               B_Seen    := False;
               D_Seen    := False;
               Mark_Seen := False;
               Start     := J + 1;
            end if;

         elsif S (J) = '#' then
            Mark_Seen := True;
         end if;
      end loop;

      --  See if we have a final string to output and output it if so

      if Start < S'Last then
         Result := Result & S (Start .. S'Last);
      end if;

      return Result;
   end TP;

   ----------------
   -- Maybe_Decl --
   ----------------

   procedure Maybe_Decl (V : Value_T) is
   begin
      if not Get_Is_Decl_Output (V) then
         Write_Decl (V);
      end if;
   end Maybe_Decl;

   --------------
   -- Num_Uses --
   --------------

   function Num_Uses (V : Value_T) return Nat is
      V_Use : Use_T := Get_First_Use (V);

   begin
      return J : Nat := 0 do
         while Present (V_Use) loop
            J := J + 1;
            V_Use := Get_Next_Use (V_Use);
         end loop;
      end return;
   end Num_Uses;

   ---------------------
   -- Cmp_Instruction --
   ---------------------

   function Cmp_Instruction (V, Op1, Op2 : Value_T) return Str is

   begin
      if Get_Instruction_Opcode (V) = Op_I_Cmp then
         declare
            type I_Info is record
               Is_Unsigned : Boolean;
               Length      : Integer;
               Op          : String (1 .. 2);
            end record;
            type I_Info_Array is array (Int_Predicate_T range <>) of I_Info;
            Int_Info : constant I_Info_Array :=
              (Int_EQ  => (False, 2, "=="),
               Int_NE  => (False, 2, "!="),
               Int_UGT => (True,  1, "> "),
               Int_UGE => (True,  2, ">="),
               Int_ULT => (True,  1, "< "),
               Int_ULE => (True,  2, "<="),
               Int_SGT => (False, 1, "> "),
               Int_SGE => (False, 2, ">="),
               Int_SLT => (False, 1, "< "),
               Int_SLE => (False, 2, "<="));
            Info     : constant I_Info := Int_Info (Get_I_Cmp_Predicate (V));
            LHS      : constant Str    :=
              Maybe_Unsigned (Op1, Info.Is_Unsigned);
            RHS      : constant Str    :=
              Maybe_Unsigned (Op2, Info.Is_Unsigned);

         begin
            return LHS & " " & Info.Op (1 .. Info.Length) & " " & RHS;
         end;

      elsif Get_Instruction_Opcode (V) = Op_F_Cmp then

         case Get_F_Cmp_Predicate (V) is
            when Real_OEQ | Real_UEQ =>
               return TP ("#1 == #2", Op1, Op2);
            when Real_OGT | Real_UGT =>
               return TP ("#1 > #2", Op1, Op2);
            when Real_OGE | Real_UGE =>
               return TP ("#1 >= #2", Op1, Op2);
            when Real_OLT | Real_ULT =>
               return TP ("#1 < #2", Op1, Op2);
            when Real_OLE | Real_ULE =>
               return TP ("#1 <= #2", Op1, Op2);
            when Real_ONE | Real_UNE =>
               return TP ("#1 != #2", Op1, Op2);
            when others =>
               null;
         end case;
      end if;

      return +"<unsupported comparison>";

   end Cmp_Instruction;

   ----------------------
   -- Call_Instruction --
   ----------------------

   procedure Call_Instruction (V : Value_T; Ops : Value_Array) is
      Func  : constant Value_T := Ops (Ops'Last);
      Call  : Str              := Func & " (";
      First : Boolean          := True;

   begin
      --  Generate the argument list for the call

      for Op of Ops (Ops'First .. Ops'Last - 1) loop
         if First then
            Call  := Call & Op;
            First := False;
         else
            Call := Call & ", " & Op;
         end if;
      end loop;

      --  Add the final close paren. If this is a procedure call,
      --  output it. Otherwise, set this as the value of V.

      Call := Call & ")";
      if Get_Type_Kind (Type_Of (V)) = Void_Type_Kind then
         Output_Stmt (Call);
      else
         Assignment (V, Call);
      end if;
   end Call_Instruction;

   ----------------
   -- Assignment --
   ----------------

   procedure Assignment (LHS : Value_T; RHS : Str) is
   begin
      --  If LHS is an entry alloca or has more than one use in the IR,
      --  generate an assignment statement into LHS. Otherwise, mark LHS
      --  as having value RHS.

      if Get_Is_Variable (LHS) or else Num_Uses (LHS) > 1 then
         Output_Stmt (LHS & " = " & RHS);
      else
         Set_C_Value (LHS, RHS);
      end if;
   end Assignment;

   -------------------------
   --  Output_Instruction --
   -------------------------

   procedure Output_Instruction (V : Value_T; Ops : Value_Array) is
      Op1 : constant Value_T  :=
        (if Ops'Length >= 1 then Ops (1) else No_Value_T);
      Op2 : constant Value_T  :=
        (if Ops'Length >= 2 then Ops (2) else No_Value_T);
      Op3 : constant Value_T  :=
        (if Ops'Length >= 3 then Ops (3) else No_Value_T);
      Opc : constant Opcode_T := Get_Instruction_Opcode (V);
   begin
      --  See if we need to write a declaration for an operand

      for Op of Ops loop
         Maybe_Decl (Op);
      end loop;

      --  Handle the instruction according to its opcode

      case Opc is

         when Op_Ret =>
            if Present (Op1) then
               Output_Stmt ("return " & Op1);
            else
               Output_Stmt ("return");
            end if;

         when Op_Call =>
            Call_Instruction (V, Ops);

         when Op_Alloca =>
            if Get_Is_Entry (Get_Instruction_Parent (V)) then
               Set_Is_Variable (V);
               Write_Decl (V);
            else
               Output_Stmt ("<unsupported instruction>");
            end if;

         when Op_Load =>
            Assignment
              (V, TP ((if Get_Is_Variable (Op1) then "#D1" else "*#1"), Op1));

         when Op_Store =>
            Output_Stmt (TP ((if   Get_Is_Variable (Op2) then "#D2 = #1"
                              else "*#2 = #1"),
                             Op1, Op2));

         when Op_I_Cmp | Op_F_Cmp =>
            Assignment (V, Cmp_Instruction (V, Op1, Op2));

         when Op_Br =>
            if Ops'Length = 1 then
               Output_Stmt (TP ("goto #B1", Op1));
            else
               Output_Stmt (TP ("if (#1) goto #B3; else goto #B2",
                               Op1, Op2, Op3));
            end if;

         when Op_Add =>
            Assignment (V, TP ("#1 + #2", Op1, Op2));

         when Op_Sub =>
            Assignment (V, TP ("#1 - #2", Op1, Op2));

         when Op_Mul =>
            Assignment (V, TP ("#1 * #2", Op1, Op2));

         when Op_S_Div | Op_U_Div =>
            Assignment (V, Maybe_Unsigned (Op1, Opc = Op_U_Div) & " / " &
                          Maybe_Unsigned (Op2, Opc = Op_U_Div));

         when Op_S_Rem | Op_U_Rem =>
            Assignment (V, Maybe_Unsigned (Op1, Opc = Op_U_Rem) & " % " &
                          Maybe_Unsigned (Op2, Opc = Op_U_Rem));

         when Op_Shl =>
            Assignment (V, TP ("#1 << #2", Op1, Op2));

         when Op_L_Shr | Op_A_Shr =>
            Assignment
              (V, Maybe_Unsigned (Op1, Opc = Op_L_Shr) & " >> " & Op2);

         when Op_F_Add =>
            Assignment (V, TP ("#1 + #2", Op1, Op2));

         when Op_F_Sub =>
            Assignment (V, TP ("#1 - #2", Op1, Op2));

         when Op_F_Mul =>
            Assignment (V, TP ("#1 * #2", Op1, Op2));

         when Op_F_Div =>
            Assignment (V, TP ("#1 / #2", Op1, Op2));

         when Op_And =>
            Assignment (V, TP ("#1 & #2", Op1, Op2));

         when Op_Or =>
            Assignment (V, TP ("#1 | #2", Op1, Op2));

         when Op_Xor =>
            Assignment (V, TP ("#1 ^ #2", Op1, Op2));

         when Op_F_Neg =>
            Assignment (V, TP (" -#1", Op1));

         when Op_Trunc | Op_SI_To_FP | Op_FP_Trunc | Op_FP_Ext | Op_S_Ext =>
            Assignment (V, TP ("(#T) #1", Op1, T => Type_Of (V)));

         when Op_UI_To_FP | Op_Z_Ext =>
            Assignment (V, "(" & Type_Of (V) & ") " & Maybe_Unsigned (Op1));

         when others =>
            Output_Stmt ("<unsupported instruction>");

      end case;
   end Output_Instruction;

end CCG.Instructions;
