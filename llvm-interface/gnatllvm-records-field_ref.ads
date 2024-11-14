------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2024, AdaCore                     --
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

package GNATLLVM.Records.Field_Ref is

   --  We need to handle a nested set of component and index references
   --  where at least the outer one is a bitfield reference. In that case,
   --  the front end will always produce a reference to a field of either a
   --  primitive type or a small aggregate type. We want to always process
   --  only that inner reference. This is both for efficiency reasons (to
   --  avoid loading and storing lots of data we don't need and duplicate
   --  data since front end splits up the reference) and because there's no
   --  straightforward way of writing such in C for objects wider than the
   --  size of "long long".
   --
   --  We define here a record that's passed to and returned by subrograms
   --  that implement this feature. It contains value pointing near the
   --  first bit of the data, the number of bits that the needed field is
   --  located past LHS, the type of the field, and its size in bits.

   type Bitfield_Ref_Desc is record
      LHS    : GL_Value;
      GT     : GL_Type;
      Offset : Nat;
      Size   : Nat;
   end record;

   No_BRD : Bitfield_Ref_Desc := (No_GL_Value, No_GL_Type, 0, 0);
   function Present (BRD : Bitfield_Ref_Desc) return Boolean is
     (Present (BRD.LHS));
   function No      (BRD : Bitfield_Ref_Desc) return Boolean is
     (No (BRD.LHS));

   function Is_Normalized (BRD : Bitfield_Ref_Desc) return Boolean;
   --  Used for pre- and post-conditions. A BRD is "normalized" if
   --
   --  (1) its size isn't larger than the maximum intger type
   --  (2) its GL_Type isn't padded
   --  (3) either LHS isn't a reference or the offset is within a byte

   function Normalize (BRD : Bitfield_Ref_Desc) return Bitfield_Ref_Desc
     with Pre  => Present (BRD),
          Post => Is_Normalized (Normalize'Result);
   --  Return a BRD that has been normalized

   function Collect_Mixed_Bitfield
     (In_N       : N_Subexpr_Id;
      For_LHS    : Boolean := False;
      Prefer_LHS : Boolean := False) return Bitfield_Ref_Desc
     with Post => No (Collect_Mixed_Bitfield'Result)
                  or else Is_Normalized (Collect_Mixed_Bitfield'Result);

   --  If In_N is a nested mix of component and indexed references and one of
   --  the component references is to a bitfield, return data describing
   --  the final reference.

   function Build_Bitfield_Load
     (BRD : Bitfield_Ref_Desc; LHS : GL_Value := No_GL_Value) return GL_Value
     with Pre => Is_Normalized (BRD);
   --  Generate a load of the data from a description of a bitfield.  LHS,
   --  if Present, is a possible location for the result.

   procedure  Build_Bitfield_Store (RHS : GL_Value; BRD : Bitfield_Ref_Desc)
     with Pre => Present (RHS) and then Is_Normalized (BRD)
                 and then Is_Reference (BRD.LHS);
   --  Generate a store of RHS to a description of a bitfield

   function Build_Bitfield_Store
     (RHS : GL_Value; BRD : Bitfield_Ref_Desc) return GL_Value
   with Pre  => Present (RHS) and then Is_Normalized (BRD),
        Post => Present (Build_Bitfield_Store'Result) =
                not Is_Reference (BRD.LHS);
   --  Generate a store of RHS to a description of a bitfield. If BRD.LHS is
   --  data, we return the new value of the bitfield field in the record.

end GNATLLVM.Records.Field_Ref;
