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

package CCG.Blocks is

   --  This package contains subprograms used in the handling of blocks

   procedure Output_BB (BB : Basic_Block_T)
     with Pre => Present (BB);
   procedure Output_BB (V : Value_T)
     with Pre => Is_A_Basic_Block (V), Inline;
   --  Generate the code for basic block unless already output

   procedure Output_Branch
     (From       : Value_T;
      To         : Value_T;
      Need_Block : Boolean := False;
      Had_Phi    : Boolean := False)
     with Pre => Present (From) and then Present (To);
   procedure Output_Branch
     (From       : Value_T;
      To         : Basic_Block_T;
      Need_Block : Boolean := False;
      Had_Phi    : Boolean := False)
     with Pre => Present (From) and then Present (To);
   --  Generate code to jump from instruction From to instruction or basic
   --  block To, taking care of any phi instructions at the target.
   --  Need_Block says whether we need to generate a "{ ... }" construct.

end CCG.Blocks;
