------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

with LLVM.Core; use LLVM.Core;

with CCG.Helper; use CCG.Helper;
with CCG.Strs;   use CCG.Strs;

package CCG.Transform is

   --  This package contains subprograms used to do transformations to the
   --  LLVM IR prior to generating C code from them.

   function Has_Unique_Predecessor (BB : Basic_Block_T) return Boolean
     with Pre => Present (BB);
   function Has_Unique_Predecessor (V : Value_T) return Boolean is
     (Has_Unique_Predecessor (Value_As_Basic_Block (V)))
     with Pre => Value_Is_Basic_Block (V);
   --  Return True iff BB has only one effective predeccessor. By "effective"
   --  we mean that if the it does have a single predecessor but that block
   --  is just an unconditional branch plus optionally Phi nodes, that
   --  predecessor also must only have a single predecessor.

   procedure Transform_Blocks (V : Value_T)
     with Pre => Is_A_Function (V);
   --  Transform the basic blocks in V so that we can generate cleaner code

end CCG.Transform;
