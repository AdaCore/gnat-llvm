------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2013-2020, AdaCore                     --
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

with Output; use Output;
with Types;  use Types;

with LLVM.Core; use LLVM.Core;

package body CCG.Output is

   -----------------
   -- Write_Value --
   -----------------

   procedure Write_Value (V : Value_T) is
      subtype LLI is Long_Long_Integer;
   begin
      --  If this is a constant, we have to output the value of the
      --  constant.

      if Present (Is_A_Constant (V)) then

         --  ??? Start with just integer constants and just small ones

         if Present (Is_A_Constant_Int (V)) then
            declare
               Val : constant LLI := Const_Int_Get_S_Ext_Value (V);

            begin
               if Val in LLI (Int'First) .. LLI (Int'Last) then
                  Write_Int (Int (Val));
               else
                  Write_Str ("<overflow>");
               end if;
            end;
         else
            Write_Str ("<unknown constant>");
         end if;
      else
         --  Otherwise, it's either a global or a computed value.
         --  If it has a name, write that name and we're done.  Otherwise,
         --  mark it as not having a name if we haven't already.

         if not Get_No_Name (V) then
            declare
               S : constant String := Get_Value_Name (V);

            begin
               if S'Length > 0 then
                  Write_Str (S);
                  return;
               end if;

               Set_No_Name (V);
            end;
         end if;

         --  Print (and make if necessary) an internal name for this value

         Write_Str ("ccg_v");
         Write_Int (Get_Output_Idx (V));
      end if;

   end Write_Value;

   ----------------
   -- Write_Decl --
   ----------------

   procedure Write_Decl (V : Value_T) is
   begin
      Set_Is_Decl_Output (V);
   end Write_Decl;

   -----------------
   -- Write_Type --
   -----------------

   procedure Write_Type (T : Type_T) is
      pragma Unreferenced (T);
   begin
      null;
   end Write_Type;

   -------------------
   -- Write_Typedef --
   --------------------

   procedure Write_Typedef (T : Type_T) is
   begin
      Set_Is_Typedef_Output (T);
   end Write_Typedef;

   --------------
   -- Write_BB --
   --------------

   procedure Write_BB (B : Basic_Block_T) is
   begin
      Write_Str ("ccg_l");
      Write_Int (Get_Output_Idx (B));
   end Write_BB;

end CCG.Output;
