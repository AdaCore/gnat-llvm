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

package body CCG.Output is

   -----------------
   -- Write_Value --
   -----------------

   procedure Write_Value (V : Value_T) is
      pragma Unreferenced (V);
   begin
      null;
   end Write_Value;

   -----------------
   -- Write_Type --
   -----------------

   procedure Write_Type (T : Type_T) is
      pragma Unreferenced (T);
   begin
      null;
   end Write_Type;

   --------------
   -- Write_BB --
   --------------

   procedure Write_BB (B : Basic_Block_T) is
      pragma Unreferenced (B);
   begin
      null;
   end Write_BB;

   -------------------
   -- Write_Typedef --
   --------------------

   procedure Write_Typedef (T : Type_T) is
      pragma Unreferenced (T);
   begin
      null;
   end Write_Typedef;

end CCG.Output;
