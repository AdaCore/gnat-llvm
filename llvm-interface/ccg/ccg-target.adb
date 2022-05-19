------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                     --
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

with GNATLLVM.Codegen; use GNATLLVM.Codegen;

package body CCG.Target is

   procedure Set_C_Parameter (Name : String; Value : String);

   ---------------------
   -- Set_C_Parameter --
   ---------------------

   procedure Set_C_Parameter (Name : String; Value : String) is
      Bool_Val    : Boolean;

   begin
      --  First handle boolean values

      if Name = "warns-parens" or else Name = "always-brace"
        or else Name = "have-includes"
      then
         if Value = "yes" or else Value = "y"
           or else Value = "true" or else Value = "t" or else Value = "on"
         then
            Bool_Val := True;
         elsif Value = "no" or else Value = "n"
           or else Value = "false" or else Value = "f" or else Value = "off"
         then
            Bool_Val := False;
         else
            Early_Error ("illegal value '" & Value & "' for parameter '"
                           & Name & "'");
            return;
         end if;

         if Name = "warns-parens" then
            Warns_Parens := Bool_Val;
         elsif Name = "always-brace" then
            Always_Brace := Bool_Val;
         else  -- Name = "have-includes"
            Have_Includes := Bool_Val;
         end if;

      --  Now handle integer values. Make a block here to catch an exception

      elsif Name = "indent" or else Name = "max-depth" or else Name = "version"
      then
         declare
            Value_First : Integer := Value'First;
            Int_Val     : Integer;

         begin
            --  For "version", we allow a leading "C", which we ignore

            if Value (Value_First) = 'C' or else Value (Value_First) = 'c' then
               Value_First := Value_First + 1;
            end if;

            Int_Val := Integer'Value (Value (Value_First .. Value'Last));
            if Name = "indent" then
               C_Indent := Int_Val;
            elsif Name = "max-depth" then
               Max_Depth := Int_Val;
            else --  Name = "version"
               if Int_Val < 80 then
                  Int_Val := Int_Val + 2000;
               elsif Int_Val < 1900 then
                  Int_Val := Int_Val + 1900;
               end if;

               Version := Int_Val;
            end if;
         end;

      else
         Early_Error ("unknown C parameter '" & Name & "'");
      end if;

   exception
      when Constraint_Error =>
         Early_Error ("illegal value '" & Value & "' for parameter '"
                        & Name & "'");
   end Set_C_Parameter;

   ----------------------
   --  Set_C_Parameter --
   ----------------------

   procedure Set_C_Parameter (S : String) is
      Equal_Pos : Integer := S'First - 1;

   begin
      for J in S'Range loop
         if S (J) = '=' then
            Equal_Pos := J;
         end if;
      end loop;

      if Equal_Pos = S'First - 1 then
         Early_Error ("Missing equal sign in parameter specification");
      elsif Equal_Pos = S'Last then
         Early_Error ("Missing value in parameter specification");
      else
         Set_C_Parameter (S (S'First .. Equal_Pos - 1),
                          S (Equal_Pos + 1 .. S'Last));
      end if;
   end Set_C_Parameter;

end CCG.Target;
