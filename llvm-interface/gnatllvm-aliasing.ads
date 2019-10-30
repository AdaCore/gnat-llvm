------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2019, AdaCore                     --
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

with GNATLLVM.GLValue; use GNATLLVM.GLValue;

package GNATLLVM.Aliasing is

   --  LLVM has "new format" TBAA type and access tags.  We use them because
   --  they allow the access type to be an aggregate.  We need this because
   --  loads and stores of aggregates are very common in Ada.
   --
   --  Unfortunately, there's no documentation of this "new format" in LLVM
   --  and the only way to find out about it is to read the relevant source
   --  file, which is lib/Analysis/TypeBasedAliasAnalysis.cpp, so we're
   --  including that documentation here.
   --
   --  The major difference in the "new format" is that sizes are included.
   --  This applies to types, fields, and access tags.
   --
   --  The format of a scalar type tag is:
   --
   --    !1 = !{!2, i64 4, !"integer", i64 0}
   --
   --  where !2 is a pointer to the parent, in this case the root, 4 is the
   --  size, in bytes, of the type, and 0 means that this isn't a type where
   --  all members are constant (a value of 1 indicates an immutable type).
   --
   --  The format of an aggregate type tag is:
   --
   --    TBD
   --
   --  The format of an access tag is:
   --
   --  !0 = !{!1, !1, i64 0, i64 4, i64 1}
   --
   --  where !1 is both a pointer to the base and access type, 0 is the offset
   --  from the start of the base type, 4 is the size (in bytes) of the access,
   --  and 1 means that this access is to constant (immutable) memory.
   --
   --  Note that "immutable" here means that the value of the memory in
   --  question is NEVER changed, not that it isn't changed at a later point
   --  in the execution (the latter is what the invariant.start intrinsic is
   --  for).  However, LLVM already knows when memory is actually a constant,
   --  so it's not clear when we'd actually use this option.

   procedure Initialize;
   --  Perform initialization for this compilation

   procedure Initialize_TBAA (V : in out GL_Value)
     with Pre => Present (V);
   function Initialize_TBAA (V : GL_Value) return GL_Value
     with Pre => Present (V);
   --  V is a value that we know nothing about except for its type.  If
   --  it's data, we have no idea of its TBAA information, but if it's a
   --  reference we can initialize the TBAA data.

   procedure Add_Aliasing_To_Instruction (Inst : Value_T; V : GL_Value)
     with Pre => Present (Is_A_Instruction (Inst)) and then Present (V);
   --  Add aliasing information from V to Inst

end GNATLLVM.Aliasing;
