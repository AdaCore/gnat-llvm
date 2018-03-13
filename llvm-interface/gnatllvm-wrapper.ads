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

with LLVM.Types; use LLVM.Types;

package GNATLLVM.Wrapper is

   pragma Annotate (Xcov, Exempt_On, "Defensive programming");

   function LLVM_Init_Module (Module : LLVM.Types.Module_T) return Integer;
   pragma Import (C, LLVM_Init_Module, "LLVM_Init_Module");
   --  Initialize the LLVM module.  Returns 0 if it succeeds.

   function LLVM_Write_Module_Internal
     (Module   : LLVM.Types.Module_T;
      Object   : Integer;
      Filename : String) return Integer;
   pragma Import (C, LLVM_Write_Module_Internal, "LLVM_Write_Object");

end GNATLLVM.Wrapper;
