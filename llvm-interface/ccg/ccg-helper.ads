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

with LLVM.Types; use LLVM.Types;

with GNATLLVM; use GNATLLVM;

package CCG.Helper is

   subtype Double is Interfaces.C.double;

   --  This package contains helper subprograms of the same name as LLVM
   --  API subprograms, but with different parameter or return types or
   --  other minor changes.

   function Const_Real_Get_Double
     (V : Value_T; Loses_Info : out Boolean) return Double
     with Pre => Present (V);

end CCG.Helper;
