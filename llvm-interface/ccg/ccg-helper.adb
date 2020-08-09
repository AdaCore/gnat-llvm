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

with Interfaces.C; use Interfaces.C;

package body CCG.Helper is

   ---------------------------
   -- Const_Real_Get_Double --
   ---------------------------

   function Const_Real_Get_Double
     (V : Value_T; Loses_Info : out Boolean) return Double
   is
      C_Loses_Info : aliased Bool_T;
      Result       : constant Double :=
        Const_Real_Get_Double (V, C_Loses_Info'Access);

   begin
      Loses_Info := C_Loses_Info /= 0;
      return Result;
   end Const_Real_Get_Double;

end CCG.Helper;
