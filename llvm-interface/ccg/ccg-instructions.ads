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

with GNATLLVM; use GNATLLVM;

with CCG.Helper; use CCG.Helper;
with CCG.Tables; use CCG.Tables;

package CCG.Instructions is

   function TP
     (S : String;
      Op1 : Value_T;
      Op2 : Value_T := No_Value_T;
      Op3 : Value_T := No_Value_T;
      T   : Type_T  := No_Type_T) return Str
     with Pre => Present (Op1), Post => Present (TP'Result);
   --  This provides a simple template facility for insertion of operands.
   --  Every character up to '#' in S is placed in Str.  '#' is followed
   --  optionally by an 'B' or 'D' and then by a number or 'T'.  The
   --  operand of that number (or the value of T, if 'T' was given) is
   --  inserted into Str at that point.  If 'B' is present, the operand is
   --  interpreted as a basic block.  If 'D' is present, then we want the
   --  data form of the operand.

   procedure Output_Instruction (V : Value_T; Ops : Value_Array)
     with Pre => Is_A_Instruction (V);
   --  Output the instruction V with operands Ops

end CCG.Instructions;
