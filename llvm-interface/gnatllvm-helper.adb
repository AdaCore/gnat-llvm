------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2023, AdaCore                     --
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

package body GNATLLVM.Helper is
   --------------------
   -- Set_Subprogram --
   --------------------

   procedure Set_Subprogram_Debug_Metadata (V : GL_Value; M : Metadata_T) is
   begin
      Set_Subprogram (+V, M);
   end Set_Subprogram_Debug_Metadata;

   --------------------------------
   -- Add_Named_Metadata_Operand --
   --------------------------------

   procedure Add_Named_Metadata_Operand (Name : String; M : Metadata_T) is
   begin
      Add_Named_Metadata_Operand (Name, Metadata_As_Value (M));
   end Add_Named_Metadata_Operand;

   --------------------------------
   -- Add_Named_Metadata_Operand --
   --------------------------------

   procedure Add_Named_Metadata_Operand (Name : String; V : Value_T) is
   begin
      Add_Named_Metadata_Operand (Module, Name, V);
   end Add_Named_Metadata_Operand;

   --------------------------------
   -- Set_Current_Debug_Location --
   -------------------------------

   procedure Set_Current_Debug_Location (MD : Metadata_T) is
   begin
      Set_Current_Debug_Location_2 (IR_Builder, MD);
   end Set_Current_Debug_Location;

end GNATLLVM.Helper;
