------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

package body GNATLLVM is

   ------------------------
   -- Get_LLVM_Error_Msg --
   ------------------------

   function Get_LLVM_Error_Msg (Msg : Ptr_Err_Msg_Type) return String is
      Err_Msg_Length : Integer := Msg'Length;
   begin
      for J in Err_Msg_Type'Range loop
         if Msg (J) = ASCII.NUL then
            Err_Msg_Length := J - 1;
            exit;
         end if;
      end loop;

      return Msg (1 .. Err_Msg_Length);
   end Get_LLVM_Error_Msg;

end GNATLLVM;
