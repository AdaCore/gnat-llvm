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

with GNATLLVM.Codegen;     use GNATLLVM.Codegen;
with GNATLLVM.GLValue;     use GNATLLVM.GLValue;

package GNATLLVM.Aliasing is

   --  LLVM has "new format" TBAA type and access tags. We use them because
   --  they allow the access type to be an aggregate. We need this because
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
   --    !1 = !{!2, i64 16, !"rectbaa__r", !3, i64 0, i64 4, !4, i64 4,
   --           i64 4, !5, i64 8, i64 4, !6, i64 12, i64 4}
   --
   --  where !2 is also a pointer to the parent, 16 is the size, in bytes,
   --  of the type, and then we have triples of three entries each. The
   --  first in each triple is the type tag for that field, the second is
   --  the offset of that type from the start of the struct, and the third is
   --  the size of that field.
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
   --  for). However, LLVM already knows when memory is actually a constant,
   --  so it's not clear when we'd actually use this option.

   --  Unlike in C, access types can only point to aliased objects
   --  (RM 3.10(9/3)), so if we have two names for the same type, they can
   --  only reference the same location if both are aliased. We implement
   --  this by using a per-type TBAA type tag for the cases where the object
   --  is aliased and a unique TBAA type tag for other references to objects
   --  of the type (e.g., a non-aliased field) and track the TBAA type tag
   --  for each GL_Value.
   --
   --  However, there will be cases where the operations performed on
   --  GL_Values are too complex to accurately track (we need to know both
   --  the struct tag and offset and may not be able to find a constant
   --  offset). We could omit giving those a TBAA type tag, but that would
   --  mean any such could alias any other such, which would pessimize the
   --  code because we do know that it can't alias anything other than its
   --  own type (with the exception of unchecked-converion issues, which
   --  are handled elsewhere). So instead, we define a "native" TBAA type
   --  for a type which is the parent of both the TBAA type for aliased
   --  objects of that type and all the unique TBAA types made for that
   --  type.
   --
   --  This produces the following, which is exactly the semantics we need:
   --
   --  - No GL_Value known to be from an aliased TBAA type tag will alias
   --    with any GL_Value known to be from a non-aliased TBAA type tag
   --
   --  - No GL_Value known to be from one unique non-aliased TBAA type tag
   --    will alias any other such valule from a different tag
   --
   --  - A GL_Value of the same type for which we don't know its origin
   --    can potentially alias with either of the above cases.
   --
   --  We implement this by storing the TBAA type to be used for aliased
   --  objects as the TBAA type tag for a type because we can obtain the
   --  native TBAA type tag (its parent) from it and use an enumeration
   --  type to specify what kind of TBAA type we're looking for.
   --
   --  We support three options for aliasing: the default, which is the above,
   --  -fno-strict-aliasing, where we don't generate TBAA tags at all, and
   --  -fc-style-aliasing, where we effectively treat all objects and
   --  components as aliased.

   type TBAA_Kind is
     (Native,
      --  The lowest-level TBAA type for a type. Everything else related
      --  to that type has it as a parent. We use this for accesses where
      --  all we know is the type.

      For_Aliased,
      --  Used for references via an access type and is also in the parent
      --  chain for any aliased object.

      Unique,
      --  Used for non-aliased objects of the type (but not if
      --  -fc-style-aliasing). The parent is the Native kind.

      Unique_Aliased
      --  Used for aliased object of the type (or all objects if
      --  -fc-style-aliasing). The parent is the For_Aliased kind.
     );

   function Kind_From_Aliased (Is_Aliased : Boolean) return TBAA_Kind is
     ((if C_Style_Aliasing or Is_Aliased then Unique_Aliased else Unique));
   --  Given whether an item is aliased, return the TBAA_Kind to use

   function Kind_From_Decl (E : Entity_Id) return TBAA_Kind is
     (Kind_From_Aliased (Present (E) and then Is_Aliased (E)));
   --  Given E (which may be Empty), determine what TBAA_Kind is
   --  appropriate for something that points to that decl, if any.

   procedure Initialize;
   --  Perform initialization for this compilation

   procedure Initialize_TBAA (V : in out GL_Value; Kind : TBAA_Kind := Native)
     with Pre => Present (V);
   function Initialize_TBAA
     (V : GL_Value; Kind : TBAA_Kind := Native) return GL_Value
     with Pre => Present (V);
   --  V is a value that we know nothing about except for its type.  If
   --  it's data, we have no idea of its TBAA information, but if it's a
   --  reference we can initialize the TBAA data.

   procedure Initialize_TBAA_If_Changed (V : in out GL_Value; Old_V : GL_Value)
     with Pre => Present (V) and Present (Old_V);
   --  Like Initialize_TBAA, but only set the new TBAA data if V and Old_V
   --  have different types or relationships.

   procedure Maybe_Initialize_TBAA_For_Field
     (V : in out GL_Value; F : Record_Field_Kind_Id; F_GT : GL_Type)
     with Pre => Present (V);
   --  If V doesn't have a TBAA type tag, set it one corresponding to F

   procedure Maybe_Initialize_TBAA_For_Array_Component
     (V : in out GL_Value; GT : GL_Type)
     with Pre => Present (V) and then Present (GT);
   --  Similarly, for a component of array type GT.  We can't test for
   --  Is_Array_Type here since it would produce a circular dependency.

   procedure Add_Aliasing_To_Instruction (Inst : Value_T; V : GL_Value)
     with Pre => Present (Is_A_Instruction (Inst)) and then Present (V);
   --  Add aliasing information from V, which is the pointer to the data
   --  being stored, to Inst.

   function Common_TBAA (M1, M2 : Metadata_T) return Metadata_T;
   --  If M1 and M2 are the same or have a common parent, return that.
   --  Otherwise, return No_Metadata_T.

   function Compute_TBAA_Access (LHS, RHS, Size : GL_Value) return Metadata_T
     with Pre => Present (LHS) and then Present (Size);
   --  LHS, RHS, and Size are parameters of a memcpy instruction.
   --  If we can compute a access tag from those, return it.

   function Compute_TBAA_Struct (LHS, RHS, Size : GL_Value) return Metadata_T
     with Pre => Present (LHS) and then Present (RHS) and then Present (Size);
   --  LHS, RHS, and Size are parameters of a memcpy instruction.
   --  If we can compute a tbaa.struct value from those, return it.

end GNATLLVM.Aliasing;
