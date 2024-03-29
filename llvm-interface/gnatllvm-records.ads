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

with Einfo.Utils; use Einfo.Utils;
with Table;       use Table;

with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.GLType;      use GNATLLVM.GLType;
with GNATLLVM.GLValue;     use GNATLLVM.GLValue;
with GNATLLVM.Types;       use GNATLLVM.Types;

package GNATLLVM.Records is

   --  There are three options for how fields can be packed:
   --
   --  1) They can't be packed at all and must remain at their natural
   --     alignment
   --  2) They can be packed to a byte boundary, but no further
   --  3) They can be packed to a bit boundary

   type Pack_Kind is (None, Byte, Bit);

   function Use_Discriminant_For_Bound (E : E_Discriminant_Id) return GL_Value
     with Post => Present (Use_Discriminant_For_Bound'Result);
   --  E is an E_Discriminant that we've run into while emitting an expression.
   --  If we are expecting one as a possible bound, evaluate this discriminant
   --  as required to compute that bound.

   function Record_Field_Offset
     (V : GL_Value; Field : Record_Field_Kind_Id) return GL_Value
     with Pre  => not Is_Data (V),
          Post => Present (Record_Field_Offset'Result);
   --  Return a GL_Value that represents the offset of a given record field

   function Get_Record_Size_Complexity
     (TE : Record_Kind_Id; Max_Size : Boolean := False) return Nat;
   --  Return the complexity of computing the size of a record. This roughly
   --  gives the number of "things" needed to access to compute the size.
   --  This returns zero iff the record type is of a constant size.

   function Get_Record_Type_Size
     (TE         : Record_Kind_Id;
      V          : GL_Value;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return GL_Value
     with Post => Present (Get_Record_Type_Size'Result);
   --  Like Get_Type_Size, but only for record types

   function Get_Record_Type_Size
     (TE         : Record_Kind_Id;
      V          : GL_Value;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return IDS
     with Post => Present (Get_Record_Type_Size'Result);

   function Get_Record_Type_Size
     (TE         : Record_Kind_Id;
      V          : GL_Value;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return BA_Data;

   function Effective_Field_Alignment (F : Record_Field_Kind_Id) return Nat
     with Post => Effective_Field_Alignment'Result > 0;

   function Get_Record_Type_Alignment (TE : Record_Kind_Id) return Nat;
   --  Like Get_Type_Alignment, but only for records and is called with
   --  the GNAT type.

   function Emit_Record_Aggregate
     (N : N_Subexpr_Id; Result_So_Far : GL_Value) return GL_Value
     with Pre  => Nkind (N) in N_Aggregate | N_Extension_Aggregate
                  and then Is_Record_Type (Full_Etype (N)),
          Post => Present (Emit_Record_Aggregate'Result);
   --  Emit code for a record aggregate at Node. Result_So_Far, if
   --  Present, contain any fields already filled in for the record.

   function Find_Matching_Field
     (TE    : Record_Kind_Id;
      Field : Record_Field_Kind_Id) return Opt_Record_Field_Kind_Id;

   --  Find a field, if any, in the entity list of TE that has the same
   --  name as F and has Field_Info.

   function Emit_Field_Position
     (E : Record_Field_Kind_Id; V : GL_Value) return GL_Value
     with Post => No (Emit_Field_Position'Result)
                  or else Type_Of (Emit_Field_Position'Result) = Size_T;
   --  Compute and return the position in bits of the field specified by E
   --  from the start of its type as a value of Size_Type. If Present, V is
   --  a value of that type, which is used in the case of a discriminated
   --  record.

   --  Because the structure of record and field info is private and we
   --  don't want to generate too many accessors, we provide a function
   --  here to collect and return information about fields in an RI.

   type Struct_Field is record
      Field      : Record_Field_Kind_Id;
      Offset     : ULL;
      T          : Type_T;
      GT         : GL_Type;
   end record;

   type Struct_Field_Array is array (Nat range <>) of Struct_Field;

   function RI_To_Struct_Field_Array
     (Ridx : Record_Info_Id) return Struct_Field_Array
     with Pre => Present (Ridx);
   --  Return an array of struct field entries for the fields in the RI

   function Field_Ordinal (F : Record_Field_Kind_Id) return unsigned;
   --  Return the index of the field denoted by F. We assume here, but
   --  don't check, that the F is in a record with just a single RI.

   function Parent_Field
     (F : Record_Field_Kind_Id) return Opt_Record_Field_Kind_Id;
   function Ancestor_Field
     (F : Record_Field_Kind_Id) return Record_Field_Kind_Id
     with Post => Ekind (F) = Ekind (Ancestor_Field'Result);
   --  Find the parent or ancestor field by walking up both the
   --  Original_Record_Component chain and the
   --  Corresponding_Record_Component chains. Only look at records whose
   --  base types have the same representation as our base type. Only return
   --  a parent if there is one, but the ancestor can be the original field.

   function Field_Type (F : Record_Field_Kind_Id) return GL_Type
     with Pre  => Present (Get_Field_Info (F)),
          Post => Present (Field_Type'Result);
   --  Return the GL_Type of the field denoted by F

   function Field_Bit_Offset (F : Record_Field_Kind_Id) return Uint
     with Pre  => Present (Get_Field_Info (F)),
          Post => Present (Field_Bit_Offset'Result);
   --  Return the bitfield offset of F or zero if it's not a bitfield

   function Is_Bitfield (F : Record_Field_Kind_Id) return Boolean
     with Pre => Present (Get_Field_Info (F));
   --  Indicate whether F is a bitfield, meaning that shift/mask operations
   --  are required to access it.

   function Cant_Misalign_Field
     (F : Record_Field_Kind_Id; GT : GL_Type) return Boolean
   is
     (Strict_Alignment (GT) or else Is_Aliased (F)
        or else Is_Independent (F) or else Is_Independent (GT)
        or else Is_Full_Access (F) or else Is_Full_Access (GT))
     with Pre => Present (GT);
   --  Return True iff a field F, whose type is GT, is not permitted to be
   --  misaligned.

   function Field_Pack_Kind
     (F           : Record_Field_Kind_Id;
      Force       : Boolean := False;
      Ignore_Size : Boolean := False) return Pack_Kind;
   --  Returns how tightly we can pack F. If Force is True, we want to
   --  pack the field if it's valid to do so, not only when we does
   --  something from the perspective of this field (e.g., because we have
   --  some bits to fill). If Ignore_Size, ignore the fact that the field
   --  may be too large to pack.

   function Is_Bitfield_By_Rep
     (F            : Record_Field_Kind_Id;
      Pos          : Uint := No_Uint;
      Size         : Uint := No_Uint;
      Use_Pos_Size : Boolean := False) return Boolean;
   --  True if we need bitfield processing for this field based on its
   --  rep clause. If Use_Pos_Size is specified, Pos and Size
   --  override that from F.

   function Is_Array_Bitfield (F : Record_Field_Kind_Id) return Boolean
     with Pre => Present (Get_Field_Info (F));
   --  If True, this is a bitfield and the underlying LLVM field is an
   --  array.

   function Is_Large_Array_Bitfield (F : Record_Field_Kind_Id) return Boolean
     with Pre => Present (Get_Field_Info (F));
   --  Likewise, but it's also a large array

   function TBAA_Type (Fidx : Field_Info_Id) return Metadata_T
     with Pre => Present (Fidx), Inline;
   procedure Set_TBAA_Type (Fidx : Field_Info_Id; M : Metadata_T)
     with Pre => Present (Fidx), Post => TBAA_Type (Fidx) = M, Inline;
   --  Set and get the TBAA type entry for Fidx

   function Align_To
     (V : GL_Value; Cur_Align, Must_Align : Nat) return GL_Value
     with Pre => Present (V), Post => Present (Align_To'Result);
   --  V is a value aligned to Cur_Align. Ensure that it's aligned to
   --  Align_To.
   --  ??? We might be better off getting the current alignment from
   --  V but then we have to properly handle the alignment in the BA_Data
   --  case and that's a lot of work.

   function Field_To_Use
     (LHS : GL_Value; F : Record_Field_Kind_Id) return Record_Field_Kind_Id
     with Pre => Present (LHS);
   --  Return the actual field to use to access field F of LHS. This may
   --  be a field from a related type.

   function Build_Field_Load
     (In_V       : GL_Value;
      In_F       : Record_Field_Kind_Id;
      LHS        : GL_Value := No_GL_Value;
      For_LHS    : Boolean  := False;
      Prefer_LHS : Boolean  := False;
      VFA        : Boolean  := False) return GL_Value
     with  Pre  => Is_Record_Type (In_V),
           Post => Present (Build_Field_Load'Result);
   --  V represents a record. Return a value representing loading field
   --  In_F from that record. If For_LHS is True, this must be a reference
   --  to the field, otherwise, it may or may not be a reference, depending
   --  on what's simpler and the value of Prefer_LHS.

   function Build_Field_Store
     (In_LHS : GL_Value;
      In_F   : Record_Field_Kind_Id;
      RHS    : GL_Value;
      VFA    : Boolean := False) return GL_Value
     with Pre => Is_Record_Type (In_LHS) and then Present (RHS);
   --  Likewise, but perform a store of RHS into the F component of In_LHS.
   --  If we return a value, that's the record that needs to be stored into
   --  the actual LHS. If no value if returned, all our work is done.

   procedure Build_Field_Store
     (LHS  : GL_Value;
      In_F : Record_Field_Kind_Id;
      RHS  : GL_Value;
      VFA  : Boolean := False)
     with  Pre => Is_Record_Type (LHS) and then Present (RHS);
   --  Similar to the function version, but we always update LHS.

   procedure Add_Write_Back
     (LHS : GL_Value; F : Opt_Record_Field_Kind_Id; RHS : GL_Value)
     with  Pre  => (No (F) or else Is_Record_Type (LHS))
                   and then Present (RHS);
   --  Like Build_Field_Store, but stack the operation to be performed
   --  later. The operations are performed LIFO.

   procedure Perform_Writebacks;
   --  Perform any writebacks put onto the stack by the Add_Write_Back
   --  procedure.

   function Record_Has_Aliased_Components (TE : Record_Kind_Id) return Boolean;
   --  Return True if any component of TE other than the tag is aliased

   --  The following are debug procedures to print information about records
   --  and fields.

   procedure Print_Field_Info (E : Record_Field_Kind_Id)
     with Export, External_Name => "dfi";
   procedure Print_Record_Info (TE : Record_Kind_Id; Eol : Boolean := False)
     with Export, External_Name => "dri";

private

   --  We can't represent all records by a single native LLVM type, so we
   --  create two data structures to represent records and the positions of
   --  fields within the record.
   --
   --  The Record_Info type is the format of an entry in the
   --  Record_Info_Table, indexed by the Record_Info_Id type. The
   --  Field_Info type is the format of an entry in the Field_Info_Table,
   --  indexed by the Field_Info_Id type. Get_Record_Info applied to a
   --  record type points to a Record_Info_Id, which is the start of the
   --  description of the record. Get_Field_Info for each field points to a
   --  Field_Info_Id, which contains information about how to locate that
   --  field within the record. Record_Info objects are chained. For
   --  variant records, we use one chain for the common part of the record
   --  and chain for each variant.
   --
   --  The Record_Info data is used to compute the size of a record and, in
   --  conjunction with the Field_Info data, to determine the offset of a
   --  field from the start of an object of that record type. We record
   --  information for each subtype separately.
   --
   --  A single Record_Info item can represent one of the following:
   --
   --      nothing, meaning that either the record or part of a variant
   --      record is empty
   --
   --      the variant part of a record
   --
   --      a single GL_Type, which must be a non-native (and hence usually
   --      of dynamic size)
   --
   --      a single LLVM type, which is a struct containing one or more
   --      fields
   --
   --  A Field_Info type locates a record by saying in which Record_Info
   --  piece it's located and, in the case where that piece contains an
   --  LLVM type, how to locate the field within that type.
   --
   --  A simple record (unpacked, with just scalar components) is
   --  represented by a single Record_Info item which points to the LLVM
   --  struct type corresponding to the Ada record. More complex but
   --  non-variant cases containing variable-sized objects require a mix of
   --  Record_Info items corresponding to LLVM and GL types. Note that a
   --  reference to a discriminant is handled within the description of
   --  array types.
   --
   --  For more complex records, the LLVM type generated may not directly
   --  correspond to that of the Ada type for two reasons. First, the
   --  GL_Type of a field may have an alignment larger than the alignment
   --  of the native LLVM type of that field or there may be record rep
   --  clauses that creates holes either at the start of a record or
   --  between Ada fields. In both of those cases, we add extra fields to
   --  the LLVM type to reflect the padding.
   --
   --  Secondly, LLVM doesn't support bitfields, so we have to do the work
   --  of generating the corresponding operations directly. We make a
   --  field corresponding to a primitive scalar type with the proper size
   --  and alignments to represent one or more bit fields. In the
   --  Field_Info item corresponding to each bitfield, we identify the
   --  ordinal of the field in the LLVM type as well as the starting bit
   --  position and bit size.
   --
   --  A fixed-size field may have an alignment requirement that's stricter
   --  than the alignment of the corresponding LLVM type, so we need to record
   --  the requested alignment in the Record_Info object.
   --
   --  We always use a packed LLVM struct type and add explicit fields for
   --  any needed padding. We also manually lay out fields that become
   --  bitfields. It's tempting to only use a packed struct for records
   --  that have a nonstandard representation and only add padding fields
   --  when a rep clause leaves space, but that causes issues with the size
   --  of the struct. For example, if we have { i32, i8 }, LLVM aligns the
   --  length and sets it to eight bytes, but there are many cases where we
   --  need it to be five bytes. One case is if this is a complete record
   --  with a Size clause of 80 bytes. But we'd also have issues with a
   --  record like:
   --
   --      type R (D : Integer) is record
   --         C : Character;
   --         S : String (1 .. D);
   --      end record;
   --
   --  The first RI for this is { i32, i8} and the second is the array. But
   --  if you then have:
   --
   --      subtype R5 is R(5);
   --
   --  This is { i32, i8, [5 x i8] } and now the size of { i32, i8} is
   --  relevant because that's how the offset to S is computed in the base
   --  type. We could record the length of the first RI as five bytes, but
   --  here's the risk of the LLVM optimizer creating a store through that
   --  type and clobbering what was beyond when it stores the padding.
   --  To avoid this issue, we always use a packed struct type, at least
   --  for now.
   --
   --  For a variant part, we record the following in the corresponding
   --  Record_Info item:
   --
   --      A pointer to the GNAT tree for the variant part (to obtain the
   --      discriminant value corresponding to each variant)
   --
   --      The expression to be evaluated (which may be a reference to a
   --      discriminant) to determine which variant is present
   --
   --      An array of Record_Info chains (corresponding to the order in
   --      the GNAT tree) for each variant. The offset of each of these
   --      chains starts at the offset of the variant Record_Info item.
   --
   --      An array of Record_Info items (in the same order) corresponding
   --      to any fields that are repped into a fixed position. The
   --      relative offset of these fields is zero.

   type Record_Info_Base is record
      LLVM_Type        : Type_T;
      --  LLVM type corresponding to this fragment, if any

      GT               : GL_Type;
      --  GL_Type corresponding to this fragment, if any

      Align            : Nat;
      --  If specified, the alignment of this piece

      Position         : ULL;
      --  If nonzero, a forced starting position (in bits, but on a byte
      --  boundary) of this piece. This can't be set on the first RI for a
      --  record.

      Next             : Record_Info_Id;
      --  Link to the next Record_Info entry for this record or variant

      Variant_List     : List_Id;
      --  List in GNAT tree of the variants for this fragment

      Variant_Expr     : Opt_N_Subexpr_Id;
      --  Expression to evaluate to determine which variant is present

      Variants         : Record_Info_Id_Array_Access;
      --  Pointer to array of Record_Info_Ids representing the variants,
      --  which must be in the same order as in Variant_List.

      Overlap_Variants : Record_Info_Id_Array_Access;
      --  Likewise for any part of the variant who offset starts at
      --  the beginning of a record (for field with record rep
      --  clauses).

      First_Field      : Field_Info_Id;
      --  Id of the first field contained in this record part, if any

      Unused_Bits      : Uint;
      --  The number of unused bits in the last type for this RI, either
      --  a GNAT type or a bitfield type. This is only used in the last
      --  RI of the chain.
   end record;
   --  We want to put a Predicate on this, but can't, so we need to make
   --  a subtype for that purpose.

   function RI_Value_Is_Valid (RI : Record_Info_Base) return Boolean;
   --  Return whether a Record_Info value is valid or not

   subtype Record_Info is Record_Info_Base
     with Predicate => RI_Value_Is_Valid (Record_Info);

   package Record_Info_Table is new Table.Table
     (Table_Component_Type => Record_Info,
      Table_Index_Type     => Record_Info_Id,
      Table_Low_Bound      => Record_Info_Low_Bound,
      Table_Initial        => 100,
      Table_Increment      => 50,
      Table_Name           => "Record_Info_Table");

   --  The information for a field is the index of the piece in the record
   --  information and optionally the location within the piece in the case
   --  when the Record_Info is an LLVM_type. We also record the GL_Type
   --  used to represent the field and bit positions if this is a bitfield.

   type Field_Info is record
      Field                : Record_Field_Kind_Id;
      --  Field for this which this is the information

      Rec_Info_Idx         : Record_Info_Id;
      --  Index into the record info table that contains this field

      Next                 : Field_Info_Id;
      --  Index of the next field in the Rec_Info_Idx

      Field_Ordinal        : Nat;
      --  Ordinal of this field within the contents of the record info table

      GT                   : GL_Type;
      --  GL_Type correspond to this field, which takes into account a
      --  possible change in size

      First_Bit            : Uint;
      --  If Present, the first bit (0-origin) within the LLVM field that
      --  corresponds to this field.

      Num_Bits             : Uint;
      --  If Present, the number of bits within the LLVM field that
      --  corresponds to this field.

      TBAA_Type            : Metadata_T;
      --  The TBAA type tag corresponding to this field

      Array_Bitfield       : Boolean;
      --  If True, the underlying LLVM field is an array. This means that
      --  we must use pointer-punning as part of accessing this field
      --  unless it's a constant, which forces it in memory and means we
      --  can't do get a static access to this field.

      Large_Array_Bitfield : Boolean;
      --  If True, this is an array bitfield, but one where the size is
      --  larger than a word. In this case, we can't even handle constants
      --  statically.

   end record;

   package Field_Info_Table is new Table.Table
     (Table_Component_Type => Field_Info,
      Table_Index_Type     => Field_Info_Id,
      Table_Low_Bound      => Field_Info_Low_Bound,
      Table_Initial        => 1000,
      Table_Increment      => 100,
      Table_Name           => "Field_Info_Table");

   function Get_Discriminant_Constraint
     (TE : E_Record_Subtype_Id; E : E_Discriminant_Id) return N_Subexpr_Id;
   --  Get the expression that constrains the discriminant E of type TE

   function Field_Position
     (E : Record_Field_Kind_Id; V : GL_Value) return BA_Data;
   --  Back-annotation version of Emit_Field_Position

end GNATLLVM.Records;
