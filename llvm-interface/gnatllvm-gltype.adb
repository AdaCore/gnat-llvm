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

with Lib;        use Lib;
with Output;     use Output;
with Repinfo;    use Repinfo;
with Sprint;     use Sprint;
with Table;
with Uintp.LLVM; use Uintp.LLVM;

with GNATLLVM.Conversions;  use GNATLLVM.Conversions;
with GNATLLVM.Exprs;        use GNATLLVM.Exprs;
with GNATLLVM.Instructions; use GNATLLVM.Instructions;
with GNATLLVM.Records;      use GNATLLVM.Records;
with GNATLLVM.Wrapper;      use GNATLLVM.Wrapper;

with CCG; use CCG;

package body GNATLLVM.GLType is

   --  A GL_Type can be of various different kinds. We list them here.

   type GT_Kind_Type is
     (None,
      --  A so-far-unused entry

      Primitive,
      --  The actual type to perform computations in

      Dummy,
      --  A dummy type, made due to a chain of access types. There are two
      --  cases, each handled differently. The most common case is an access
      --  type pointing to a record. In that case, we can make an opaque
      --  record that we can actually use for the record. In that case,
      --  that's the same type that we really be used for the record, so the
      --  access type is "real" and the record type will only be considered
      --  "dummy" for a transitory period after which we'll change this entry
      --  to Primitive kind.
      --
      --  The other case is when we have an access to something else. In that
      --  case, we have to make a completely fake access type that points to
      --  something else. In that case, we'll keep this entry around as a
      --  GL_Type because things will have that type and we'll have to convert
      --  as appropriate.

      Int_Alt,
      --  An integral type of a different width then the primitive type
      --  (either wider or narrower), but not a biased type.

      Access_Alt,
      --  An alternate representation of an access type, either a thin
      --  pointer when the primitive type is a fat pointer, or vice versa

      Biased,
      --  An integral type narrower than the primitive type and for which
      --  a bias is added value of the type to obtain a value of the
      --  primitive type.

      Padded,
      --  A record whose first field is the primitive type and the second
      --  is padding to make the record the proper length. This can only
      --  be done if the primitive type is a native LLVM type.

      Truncated,
      --  A type that's shorter than the underlying LLVM type because
      --  (e.g.) the LLVM type represents an array with padding. We only
      --  can have this if the primitive type is a native LLVM type. We
      --  disallow the use of Load or Store instructions of this type by
      --  marking it as a non-loadable type, so all operations on the type
      --  are done with memcpy and a specified length.

      Byte_Array,
      --  An array of bytes (i8) whose length is the desired size of the
      --  GL_Type. This should only be used when the primitive type is not
      --  a native LLVM type.

      Max_Size_Type,
      --  We're denoting that the maximum size of the type is used, but
      --  that maximum size is dynamic, so the LLVM type is actually that
      --  of the primitive type. This also implies that the LLVM type is
      --  non-native.

      Aligning);
      --  The same LLVM type as for the primitive type, but recorded to
      --  indicate that we need to align it differently. This occurs
      --  when the primitive type is not a native LLVM type or when we're
      --  just changing the alignment and not type.

   --  Define the fields in the table for GL_Type's

   type GL_Type_Info_Base is record
      GNAT_Type   : Void_Or_Type_Kind_Id;
      --  GNAT type

      LLVM_Type   : Type_T;
      --  LLVM type used for this alternative

      TBAA        : Metadata_T;
      --  If Present, the TBAA tag to use

      Next        : GL_Type;
      --  If Present, link to next alternative

      Size        : GL_Value;
      --  If Present, size of this alternative in bits

      Alignment   : Nat;
      --  If nonzero, the alignment of this alternative in bits

      Bias        : GL_Value;
      --  If Present, the amount of bias for integral types

      Array_Types : Array_Types_Id;
      --  If this is an array type and Present, gives the table index at
      --  which the types used for parts of arrays, such as bounds, can
      --  be found.

      Max_Size    : Boolean;
      --  If True, this corresponds to the maxumum size of an unconstrained
      --  variant record with default discriminant values;

      Kind        : GT_Kind_Type;
      --  Says what type of alternative type this is

      Default     : Boolean;
      --  Marks the default GL_Type

   end record;
   --  We want to put a Predicate on this, but can't, so we need to make
   --  a subtype for that purpose.

   function GL_Type_Info_Is_Valid (GTI : GL_Type_Info_Base) return Boolean;
   --  Return whether GT is a valid GL_Type or not

   subtype GL_Type_Info is GL_Type_Info_Base
     with Predicate => GL_Type_Info_Is_Valid (GL_Type_Info);
   --  Subtype used by everybody except validation function

   function GL_Type_Info_Is_Valid_Int (GTI : GL_Type_Info_Base) return Boolean;
   --  Internal version of GL_Value_Is_Valid

   package GL_Types is new Table.Table
     (Table_Component_Type => GL_Type_Info,
      Table_Index_Type     => GL_Type'Base,
      Table_Low_Bound      => GL_Type_Low_Bound,
      Table_Initial        => 2000,
      Table_Increment      => 200,
      Table_Name           => "GL_Types");

   procedure Next (GT : in out GL_Type)
     with Pre => Present (GT), Inline;

   function Is_Default (GT : GL_Type) return Boolean
     with Pre => Present (GT), Inline;

   function Get_Or_Create_GL_Type
     (TE : Void_Or_Type_Kind_Id; Create : Boolean) return GL_Type
     with Post => not Create or else Present (Get_Or_Create_GL_Type'Result);

   function Convert_Int (V : GL_Value; GT : GL_Type) return GL_Value
     with Pre  => Is_Data (V) and then Is_Discrete_Or_Fixed_Point_Type (V)
                  and then Is_Discrete_Or_Fixed_Point_Type (GT)
                  and then Full_Etype (Related_Type (V)) = Full_Etype (GT),
          Post => Related_Type (Convert_Int'Result) = GT;
   --  Convert V, which is of one integral type, to GT, an alternative
   --  of that type.

   function Convert_Access (V : GL_Value; GT : GL_Type) return GL_Value
     with Pre  => Is_Data (V) and then Is_Access_Type (V)
                  and then Is_Unconstrained_Array
                             (Full_Designated_GL_Type (V)),
          Post => Related_Type (Convert_Access'Result) = GT;
   --  Likewise, for converting between forms of access to unconstrained

   function Convert_Via_Copy (V : GL_Value; GT : GL_Type) return GL_Value
     with Pre  => Is_Reference (V) and then Present (GT),
          Post => Related_Type (Convert_Via_Copy'Result) = GT;
   --  Likewise, for cases where we have to make a copy

   function Make_GT_Alternative_Internal
     (GT        : GL_Type;
      Size      : Uint;
      Align     : Uint;
      For_Type  : Boolean;
      Max_Size  : Boolean;
      Is_Biased : Boolean) return GL_Type
     with Pre  => Present (GT),
          Post => Full_Etype (Make_GT_Alternative_Internal'Result)
                   = Full_Etype (GT);
   --  Internal version of Make_GT_Alternative to actually make the GL_Type

   function GT_Kind (GT : GL_Type) return GT_Kind_Type is
     (GL_Types.Table (GT).Kind)
     with Pre => Present (GT);

   ---------------------------
   -- GL_Type_Info_Is_Valid --
   ---------------------------

   function GL_Type_Info_Is_Valid (GTI : GL_Type_Info_Base) return Boolean is
      Valid : constant Boolean := GL_Type_Info_Is_Valid_Int (GTI);
   begin
      --  This function exists so a conditional breakpoint can be set at
      --  the following line to see the invalid value. Otherwise, there
      --  seems no other reasonable way to get to see it.

      return Valid;
   end GL_Type_Info_Is_Valid;

   -------------------------------
   -- GL_Type_Info_Is_Valid_Int --
   -------------------------------

   function GL_Type_Info_Is_Valid_Int
     (GTI : GL_Type_Info_Base) return Boolean
   is
      TE : constant Void_Or_Type_Kind_Id := GTI.GNAT_Type;
      T  : constant Type_T               := GTI.LLVM_Type;

   begin
      --  We have to be careful below and not call anything that will cause
      --  a validation of a GL_Value because that will cause mutual
      --  recursion with us.

      if GTI.Kind = None then
         return True;

      elsif not Is_Type_Or_Void (TE) or else No (T)
        or else (GTI.Size /= No_GL_Value
                   and then No (Is_A_Constant_Int (GTI.Size.Value)))
        or else (GTI.Alignment /= 0 and then GTI.Alignment mod BPU /= 0)
        or else (GTI.Bias /= No_GL_Value
                   and then No (Is_A_Constant_Int (GTI.Bias.Value)))
      then
         return False;
      end if;

      case GTI.Kind is
         when None  | Primitive | Aligning =>
            return True;
         when Dummy =>
            return Is_Record_Type (TE) or else Is_Access_Type (TE);
         when Int_Alt =>
            return Is_Discrete_Or_Fixed_Point_Type (TE);
         when Access_Alt =>
            return Is_Access_Type (TE)
              and then Is_Unconstrained_Array (Full_Designated_Type (TE));
         when Biased =>
            return GTI.Bias /= No_GL_Value and then Is_Discrete_Type (TE);
         when Padded =>
            return not Is_Nonnative_Type (TE)
              and then Get_Type_Kind (T) = Struct_Type_Kind;
         when Byte_Array =>
            return Is_Nonnative_Type (TE)
              and then Get_Type_Kind (T) = Array_Type_Kind;
         when Truncated =>
            return not Is_Nonnative_Type (TE);
         when Max_Size_Type =>
            return Is_Nonnative_Type (TE)
              and then Is_Unconstrained_Record (TE);
      end case;

   end GL_Type_Info_Is_Valid_Int;

   ----------
   -- Next --
   ----------

   procedure Next (GT : in out GL_Type) is
   begin
      GT := GL_Types.Table (GT).Next;
   end Next;

   -------------
   -- GT_Size --
   -------------

   function GT_Size (GT : GL_Type) return GL_Value is
     (GL_Types.Table (GT).Size);

   -----------------
   -- Is_Max_Size --
   -----------------

   function Is_Max_Size (GT : GL_Type) return Boolean is
     (GL_Types.Table (GT).Max_Size);

   ----------------
   -- Is_Default --
   ----------------

   function Is_Default (GT : GL_Type) return Boolean is
     (GL_Types.Table (GT).Default);

   ------------------
   -- GT_Alignment --
   ------------------

   function GT_Alignment (GT : GL_Type) return Nat is
     (GL_Types.Table (GT).Alignment);

   ---------------
   -- TBAA_Type --
   ---------------

   function TBAA_Type (GT : GL_Type) return Metadata_T is
     (GL_Types.Table (GT).TBAA);

   -------------------
   -- Set_TBAA_Type --
   -------------------

   procedure Set_TBAA_Type (GT : GL_Type; MD : Metadata_T) is
   begin
      GL_Types.Table (GT).TBAA := MD;
   end Set_TBAA_Type;

   ---------------------
   -- Get_Array_Types --
   ---------------------

   function Get_Array_Types
     (GT : Array_Or_PAT_GL_Type) return Array_Types_Id
   is
     (GL_Types.Table (GT).Array_Types);

   ---------------------
   -- Set_Array_Types --
   ---------------------

   procedure Set_Array_Types (GT : Array_Or_PAT_GL_Type; ATs : Array_Types_Id)
   is
   begin
      GL_Types.Table (GT).Array_Types := ATs;
   end Set_Array_Types;

   ---------------------------
   -- Get_Or_Create_GL_Type --
   ---------------------------

   function Get_Or_Create_GL_Type
     (TE : Void_Or_Type_Kind_Id; Create : Boolean) return GL_Type is
   begin
      return GT : GL_Type := Get_GL_Type (TE) do
         if No (GT) and then Create then
            Discard (Type_Of (TE));
            GT := Get_GL_Type (TE);
         end if;
      end return;
   end Get_Or_Create_GL_Type;

   ------------
   -- New_GT --
   ------------

   function New_GT (TE : Void_Or_Type_Kind_Id) return GL_Type is
      GT : GL_Type;

   begin
      GL_Types.Append ((GNAT_Type   => TE,
                        LLVM_Type   => No_Type_T,
                        TBAA        => No_Metadata_T,
                        Next        => Get_GL_Type (TE),
                        Size        => No_GL_Value,
                        Alignment   => 0,
                        Bias        => No_GL_Value,
                        Array_Types => Empty_Array_Types_Id,
                        Max_Size    => False,
                        Kind        => None,
                        Default     => False));

      GT := GL_Types.Last;
      Set_GL_Type (TE, GT);
      return GT;
   end New_GT;

   -------------------------
   -- Make_GT_Alternative --
   -------------------------

   function Make_GT_Alternative
     (GT            : GL_Type;
      E             : Entity_Id;
      Size          : Uint    := No_Uint;
      Align         : Uint    := No_Uint;
      For_Type      : Boolean := False;
      For_Component : Boolean := False;
      Max_Size      : Boolean := False;
      Is_Biased     : Boolean := False;
      Align_For_Msg : Uint    := No_Uint) return GL_Type
   is
      In_Sz     : constant GL_Value  := GT_Size (GT);
      Out_GT    : constant GL_Type   :=
        Make_GT_Alternative_Internal (GT, Size, Align, For_Type, Max_Size,
                                      Is_Biased);
      T         : constant Type_T    := Type_Of (GT);
      Err_Ident : constant Entity_Id :=
        (if   Present (E) and then Is_Packed_Array_Impl_Type (E)
         then Original_Array_Type (E) else E);

   begin
      --  Struct types that have names aren't shared, so we can link them
      --  to the GNAT entity.

      if Get_Type_Kind (T) = Struct_Type_Kind and then Struct_Has_Name (T) then
         C_Set_Entity (T, Full_Etype (GT));
      end if;

      --  If this is an entity that comes from source, is in the unit being
      --  compiled, a size was specified, and we've made a padded type, set
      --  a warning saying how many bits are unused. Consider the alignment
      --  of the type when doing that.

      if Present (Err_Ident) and then Comes_From_Source (Err_Ident)
        and then In_Extended_Main_Code_Unit (Err_Ident)
        and then (Has_Padding (Out_GT)
                    or else Is_Packed_Array_Impl_Type (GT))
        and then Present (In_Sz) and then Present (Size)
      then
         declare
            Align_V1   : constant Nat      :=
              (if    Present (Align_For_Msg) then +Align_For_Msg
               elsif Present (Align) and then Is_Composite_Type (GT)
               then  +Align else BPU);
            Align_V     : constant Nat      :=
              Nat'Max (Align_V1, Get_Type_Alignment (GT));
            Out_Sz      : constant GL_Value := Size_Const_Int (Size);
            In_Sz_Align : constant GL_Value :=
              Align_To (GT_Size (GT), 1, Align_V);
            Pad_Sz      : constant GL_Value :=
              (if   Present (In_Sz_Align) then Out_Sz - In_Sz_Align
               else No_GL_Value);
            Err_Node    : Entity_Id            := Empty;

         begin
            if Present (Pad_Sz) and then Pad_Sz > 0 then
               if Ekind (Err_Ident) in E_Component | E_Discriminant
                 and then Present (Component_Clause (Err_Ident))
               then
                  Err_Node := Last_Bit (Component_Clause (Err_Ident));
               elsif Has_Size_Clause (Err_Ident) then
                  Err_Node := Expression (Size_Clause (Err_Ident));
               elsif Is_Type (Err_Ident)
                 and then Has_Object_Size_Clause (Err_Ident)
               then
                  Err_Node := Expression (Object_Size_Clause (Err_Ident));
               end if;

               if For_Component then
                  Error_Msg_NE_Num ("??component of& padded by ^ bits",
                                    Err_Ident, Err_Ident, Pad_Sz);
               elsif Present (Err_Node) then
                  Error_Msg_NE_Num ("??^ bits of & unused", Err_Node,
                                    Err_Ident, Pad_Sz);
               end if;
            end if;
         end;
      end if;

      return Out_GT;
   end Make_GT_Alternative;

   ----------------------------------
   -- Make_GT_Alternative_Internal --
   ----------------------------------

   function Make_GT_Alternative_Internal
     (GT        : GL_Type;
      Size      : Uint;
      Align     : Uint;
      For_Type  : Boolean;
      Max_Size  : Boolean;
      Is_Biased : Boolean) return GL_Type
   is
      function Make_Large_Array (Count : ULL) return Type_T
        with Post => Get_Type_Kind (Make_Large_Array'Result) = Array_Type_Kind;
      --  Build an array (possibly a nested array) of Count entries

      In_GTI      : constant GL_Type_Info := GL_Types.Table (GT);
      Needs_Bias  : constant Boolean      :=
        Is_Biased or else In_GTI.Kind = Biased;
      Needs_Max   : constant Boolean      := Max_Size or else In_GTI.Max_Size;
      TE          : constant Type_Kind_Id := Full_Etype (GT);
      Prim_GT     : constant GL_Type      := Primitive_GL_Type (GT);
      Prim_Native : constant Boolean      := not Is_Nonnative_Type (Prim_GT);
      Prim_T      : constant Type_T       := Type_Of (Prim_GT);
      Prim_Fixed  : constant Boolean      :=
        not Is_Dynamic_Size (Prim_GT, Allow_Overflow => True);
      Prim_Size   : constant GL_Value     :=
        (if Prim_Fixed then Get_Type_Size (Prim_GT) else No_GL_Value);
      Prim_Align  : constant Nat          := Get_Type_Alignment (Prim_GT);
      Int_Sz      : constant Uint         :=
        (if No (Size) or else Size = 0 then Uint_1 else Size);
      Size_V      : GL_Value              :=
        (if   No (Size) or else not UI_Is_In_ULL_Range (Size)
              or else Is_Dynamic_SO_Ref (Size)
         then In_GTI.Size else Size_Const_Int (Size));
      Align_N     : constant Nat          :=
        (if No (Align) then In_GTI.Alignment else +Align);
      Found_GT    : GL_Type               := Get_GL_Type (TE);

      ----------------------
      -- Make_Large_Array --
      ----------------------

      function Make_Large_Array (Count : ULL) return Type_T is
         Count_Left : ULL := Count;
         This_Count : ULL;

      begin
         --  We need an array of Count elements, but can only make an array
         --  of Unsigned'Last elements, so if we need more, we need to make
         --  a nested array. Set a limit a bit under the maximum to avoid
         --  any corner cases.

         return Result : Type_T := Byte_T do
            loop
               This_Count := ULL'Min (Count_Left, ULL (unsigned'Last / 2));
               Result     := Array_Type (Result, unsigned (This_Count));
               exit when This_Count = Count_Left;
               Count_Left := Count_Left / This_Count;
            end loop;
         end return;
      end Make_Large_Array;

   begin
      --  If we're not specifying a size, alignment, or a request for
      --  maximum size, we want the original type. This isn't quite the
      --  same test as below since it will get confused with 0-sized types.

      if No (Size_V) and then Align_N = 0 and then not Needs_Max then
         return GT;

      --  If the best type we had is a dummy type, don't make any alternatives

      elsif Is_Dummy_Type (Prim_GT) then
         return Prim_GT;

      --  If we're asking for the maximum size, the maximum size is a
      --  constant, and we don't have a specified size, use the maximum size
      --  if we have one.

      elsif Needs_Max and then not Is_Dynamic_Size (Prim_GT, Max_Size => True)
        and then No (Size_V)
      then
         Size_V := Get_Type_Size (Prim_GT, Max_Size => True);

         if Overflowed (Size_V) then
            Size_V := No_GL_Value;
         end if;
      end if;

      --  If this is for a type, we have to align the input size

      if For_Type and then Present (Size_V) and then Align_N /= 0
        and then (+Size_V) mod ULL (Align_N) /= 0
      then
         Size_V := Align_To (Size_V, 1, Align_N);
      end if;

      --  See if we already made a matching GL_Type

      while Present (Found_GT) loop
         declare
            GTI : constant GL_Type_Info := GL_Types.Table (Found_GT);
         begin
            if (Size_V = GTI.Size and then Align_N = GTI.Alignment
                  and then Needs_Bias = (GTI.Kind = Biased)
                  and then not (Needs_Max
                                  and then (No (Size_V)
                                              or else not Prim_Native))
                  and then not (Present (Size)
                                  and then Get_Type_Kind (GTI.LLVM_Type) =
                                             Integer_Type_Kind
                                  and then ULL'(Get_Type_Size (GTI.LLVM_Type))
                                             /= +Size))
              --  If the size and alignment are the same, this must be the
              --  same type. But this isn't the case if we need the
              --  maximim size and there's no size for the type or the
              --  primitive type isn't native (the latter can happen for a
              --  variant record where all the variants are the same size.)
              --  Also check for the integral case when the size isn't the
              --  number of bits.

              or else (Needs_Max and then GTI.Max_Size
                         and then ((No (Size_V) and then No (GTI.Size))
                                   or else Size_V = GTI.Size)
                         and then Align_N = GTI.Alignment)
              --  It's also the same type even if there's no match if
              --  we want the maximum size and we have an entry where
              --  we got the maximum size. But we need the right alignment.

            then
               return Found_GT;
            end if;
         end;

         Next (Found_GT);
      end loop;

      --  Otherwise, we have to create a new GL_Type. We know that the
      --  size, alignment, or both differ from that of the primitive type.
      --  Once we set GTI below, be sure that we don't do any operations
      --  that could create a new GL_Type since that will invalidate it.

      declare
         Ret_GT : constant GL_Type := New_GT (TE);
         GTI    : GL_Type_Info renames GL_Types.Table (Ret_GT);

      begin
         --  Record the basic parameters of what we're making

         GTI.Size      := Size_V;
         GTI.Alignment := Align_N;
         GTI.Max_Size  := Needs_Max;

         --  If this is a biased type, make a narrower integer and set the
         --  bias.

         if Needs_Bias then
            GTI.LLVM_Type :=
              (if No (Int_Sz) then Prim_T else Int_Ty (Int_Sz));
            GTI.Kind      := Biased;
            GTI.Bias      :=
              Emit_Convert_Value
                (Low_Bound (Simplify_Range (Scalar_Range (Prim_GT))), Prim_GT);

         --  If this is a discrete or fixed-point type and a size was
         --  specified that's no larger than the largest integral type,
         --  make an alternate integer type.

         elsif Is_Discrete_Or_Fixed_Point_Type (GT)
           and then Present (Size) and then Size <= Max_Int_Size
         then
            GTI.LLVM_Type := Int_Ty (Int_Sz);
            GTI.Kind      := Int_Alt;

         --  If this is an access type to an unconstrained array and we have
         --  a size that corresponds to either a thin or fat pointer, make
         --  and alternate access type.

         elsif Is_Access_Type (GT)
           and then Is_Unconstrained_Array (Full_Designated_Type (GT))
           and then (Size = Thin_Pointer_Size or else Size = Fat_Pointer_Size)
         then
            declare
               DT    : constant GL_Type         :=
                 Full_Designated_GL_Type (GT);
               R     : constant GL_Relationship :=
                 Relationship_For_Access_Type (GT);
               New_R : constant GL_Relationship :=
                 (if   Size = Thin_Pointer_Size then Thin_Pointer
                  else Fat_Pointer);

            begin
               pragma Assert (R in Thin_Pointer | Fat_Pointer);
               GTI.LLVM_Type := Type_For_Relationship (DT, New_R);
               GTI.Kind      := Access_Alt;
            end;

         --  If we have a native primitive type, we specified a size, and
         --  the size is larger that that of the primitive, or if the
         --  alignment is different, we make a padded or aligning type.

         elsif Prim_Native and then Present (Size_V)
           and then (Size_V > Prim_Size
                       or else (Align_N /= 0 and then Prim_Align /= Align_N))
         then
            declare
               Pad_Size  : constant GL_Value := Size_V - Prim_Size;
               Pad_Count : constant LLI      := To_Bytes (+Pad_Size);

            begin
               --  If there's a padding amount, this a padded type.
               --  Otherwise, this is an aligning type.

               if Pad_Count > 0 then
                  GTI.LLVM_Type :=
                    Build_Struct_Type
                    ((1 => Prim_T, 2 => Make_Large_Array (ULL (Pad_Count))),
                     Name        => Get_Ext_Name (GT, "_PAD"),
                     Field_Names => (1 => Name_Find ("DATA"), 2 => No_Name),
                     Packed      => (+Size_V) mod ULL (Align_N) /= 0);
                  GTI.Kind      := Padded;
               else
                  GTI.LLVM_Type := Prim_T;
                  GTI.Kind      := Aligning;
               end if;
            end;

         --  Similarly, if he size is smaller, make a truncating type

         elsif Prim_Native and then Present (Size_V)
           and then Size_V < Prim_Size
         then
            GTI.LLVM_Type := Prim_T;
            GTI.Kind      := Truncated;

         --  If we're making a fixed-size version of something of dynamic
         --  size (possibly because we need the maximum size), we need a
         --  Byte_Array.

         elsif not Prim_Native and then Present (Size_V) then
            GTI.LLVM_Type := Make_Large_Array (To_Bytes (+Size_V));
            GTI.Kind      := Byte_Array;

         --  If we're looking for the maximum size and none of the above cases
         --  are true, we just make a GT showing that's what we need.

         elsif Needs_Max then
            GTI.LLVM_Type := Prim_T;
            GTI.Kind      := Max_Size_Type;

         --  Othewise, we must just be changing the alignment of a
         --  variable-size type.

         else
            GTI.LLVM_Type := Prim_T;
            GTI.Kind      := Aligning;
         end if;

         if For_Type then
            Mark_Default (Ret_GT);
         end if;

         return Ret_GT;
      end;
   end Make_GT_Alternative_Internal;

   --------------------
   -- Update_GL_Type --
   --------------------

   procedure Update_GL_Type (GT : GL_Type; T : Type_T; Is_Dummy : Boolean) is
      GTI : GL_Type_Info renames GL_Types.Table (GT);

   begin
      GTI.LLVM_Type := T;
      GTI.Kind      := (if Is_Dummy then Dummy else Primitive);
      Mark_Default (GT);

      --  Struct types that have names aren't shared, so we can link them
      --  to the GNAT entity.

      if Get_Type_Kind (T) = Struct_Type_Kind and then Struct_Has_Name (T) then
         C_Set_Entity (T, Full_Etype (GT));
      end if;

      --  If Size_Type hasn't been elaborated yet, we're done for now.
      --  If this is a E_Void or E_Subprogram_Type, it doesn't have a
      --  size or alignment. Otherwise, set the alignment and also
      --  set the size if it's a constant. If T is a non-native type,
      --  the size in the GT must agree with the size of T since this is
      --  a primitive type.

      if Present (Size_GL_Type) and then not Is_Dummy
        and then Ekind (GT) not in E_Void | E_Subprogram_Type
        and then not (Decls_Only and then not Type_Is_Sized (T))
      then
         GTI.Alignment  := Get_Type_Alignment (GT, Use_Specified => False);

         if not Is_Dynamic_Size (GT) then
            GTI.Size    :=
              (if    Is_Integer_Type (GT)
               then  Size_Const_Int (Get_Scalar_Bit_Size (T))
               elsif Is_Nonnative_Type (GT) then Get_Type_Size (GT)
               else  Size_Const_Int (Get_Type_Size (T)));

            --  In the case where this isn't a native type, pad the size to
            --  if we have something that requires strict alignment.

            if Is_Nonnative_Type (GT) and then Strict_Alignment (GT) then
               GTI.Size := Align_To (GTI.Size, 1, Get_Type_Alignment (GT));
            end if;
         end if;
      end if;

   end Update_GL_Type;

   -----------------------
   -- Primitive_GL_Type --
   -----------------------

   function Primitive_GL_Type (TE : Void_Or_Type_Kind_Id) return GL_Type is
      GT : GL_Type := Get_Or_Create_GL_Type (TE, True);

   begin
      --  First look for a primitive type. If there isn't one, then a
      --  dummy type is the best we have.

      while Present (GT) loop
         exit when GT_Kind (GT) = Primitive;
         Next (GT);
      end loop;

      if No (GT) then
         GT := Get_GL_Type (TE);
         while Present (GT) loop
            exit when GT_Kind (GT) = Dummy;
            Next (GT);
         end loop;
      end if;

      --  If what we got was a dummy type, try again to make a type. Note that
      --  we may not have succeded, so we may get the dummy type back.

      if Present (GT) and then Is_Dummy_Type (GT) then
         Discard (Type_Of (TE));
         GT := Get_GL_Type (TE);

         while Present (GT) loop
            exit when GT_Kind (GT) = Primitive;
            Next (GT);
         end loop;

         if No (GT) then
            GT := Get_GL_Type (TE);
            while Present (GT) loop
               exit when GT_Kind (GT) = Dummy;
               Next (GT);
            end loop;
         end if;
      end if;

      return GT;
   end Primitive_GL_Type;

   -----------------------
   -- Primitive_GL_Type --
   -----------------------

   function Primitive_GL_Type (GT : GL_Type) return GL_Type is
     (Primitive_GL_Type (Full_Etype (GT)));

   -----------------------
   -- Primitive_GL_Type --
   -----------------------

   function Primitive_GL_Type (V : GL_Value) return GL_Type is
     (Primitive_GL_Type (Full_Etype (Related_Type (V))));

   -------------------
   -- Dummy_GL_Type --
   -------------------

   function Dummy_GL_Type (TE : Void_Or_Type_Kind_Id) return GL_Type is
   begin
      return GT : GL_Type := Get_Or_Create_GL_Type (TE, False) do
         while Present (GT) loop
            exit when GT_Kind (GT) = Dummy;
            Next (GT);
         end loop;
      end return;
   end Dummy_GL_Type;

   ---------------------
   -- Default_GL_Type --
   ---------------------

   function Default_GL_Type
     (TE : Void_Or_Type_Kind_Id; Create : Boolean := True) return GL_Type is
   begin
      return GT : GL_Type := Get_Or_Create_GL_Type (TE, Create) do
         while Present (GT) loop
            exit when GL_Types.Table (GT).Default;
            Next (GT);
         end loop;

         --  If what we got was a dummy type, try again to make a type.
         --  Note that we may not have succeded, so we may get the dummy
         --  type back.

         if Create and then Present (GT) and then Is_Dummy_Type (GT) then
            Discard (Type_Of (TE));
            GT := Get_GL_Type (TE);

            while Present (GT) loop
               exit when GL_Types.Table (GT).Default;
               Next (GT);
            end loop;
         end if;
      end return;
   end Default_GL_Type;

   ---------------------
   -- Default_GL_Type --
   ---------------------

   function Default_GL_Type (GT : GL_Type) return GL_Type is
     (Default_GL_Type (Full_Etype (GT)));

   ---------------------
   -- Default_GL_Type --
   ---------------------

   function Default_GL_Type (V : GL_Value) return GL_Type is
     (Default_GL_Type (Full_Etype (Related_Type (V))));

   ------------------
   -- Mark_Default --
   ------------------

   procedure Mark_Default (GT : GL_Type) is
      All_GT : GL_Type := Get_GL_Type (Full_Etype (GT));

   begin
      --  Mark each GT as default or not, depending on whether it's ours

      while Present (All_GT) loop
         GL_Types.Table (All_GT).Default := All_GT = GT;
         Next (All_GT);
      end loop;
   end Mark_Default;

   ---------------------
   -- Get_Unused_Bits --
   ---------------------

   function Get_Unused_Bits (GT : GL_Type) return Uint is
   begin
      --  There are two ways we can have unused bits. We can have a
      --  composite type with padding at the end.

      return Bits : Uint := Uint_0 do
         if Is_Composite_Type (GT) and then Known_Static_Esize (GT)
           and then Known_Static_RM_Size (GT)
         then
            Bits := Bits + Esize (GT) - RM_Size (GT);
         end if;

         --  Another case is if we have a GT that's not the default, in
         --  which case we have to adjust between its size and the default
         --  size. For elementary types, we only do this for padding types
         --  and if the difference is positive.

         if not Is_Default (GT) and then not Is_Dynamic_Size (GT) then
            declare
               Def_GT   : constant GL_Type  := Default_GL_Type (GT);
               Our_Size : constant GL_Value := Get_Type_Size (GT);

            begin
               if not Is_Dynamic_Size (Def_GT)
                 and then (Is_Composite_Type (GT)
                             or else (Our_Size > Get_Type_Size (Def_GT)
                                        and then Has_Padding (GT)))
               then
                  Bits := Bits + (Our_Size - Get_Type_Size (Def_GT));
               end if;
            end;
         end if;
      end return;

   end Get_Unused_Bits;

   -----------------
   -- Convert_Int --
   -----------------

   function Convert_Int (V : GL_Value; GT : GL_Type) return GL_Value is
      type Cvtf is access function
        (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value;

      T           : constant Type_T  := Type_Of (GT);
      In_GT       : constant GL_Type := Related_Type (V);
      Src_Uns     : constant Boolean := Is_Unsigned_For_RM (In_GT);
      Src_Size    : constant Nat     := Nat (ULL'(Get_Scalar_Bit_Size (V)));
      Dest_Size   : constant Nat     := Nat (ULL'(Get_Scalar_Bit_Size (T)));
      Is_Trunc    : constant Boolean := Dest_Size < Src_Size;
      Subp        : Cvtf             := null;

   begin
      --  If the value is already of the desired LLVM type, we're done.

      if Type_Of (V) = Type_Of (GT) then
         return G_Is (V, GT);
      elsif Is_Trunc then
         Subp := Trunc'Access;
      else
         Subp := (if Src_Uns then Z_Ext'Access else S_Ext'Access);
      end if;

      return Subp (V, GT);
   end Convert_Int;

   --------------------
   -- Convert_Access --
   --------------------

   function Convert_Access (V : GL_Value; GT : GL_Type) return GL_Value is
     (To_Access (Get (From_Access (V), Relationship_For_Access_Type (GT)),
                 GT));

   ----------------------
   -- Convert_Via_Copy --
   ----------------------

   function Convert_Via_Copy (V : GL_Value; GT : GL_Type) return GL_Value is
      Memory : constant GL_Value := Allocate_For_Type (GT);

   begin
      --  We've allocated memory for the type we're converting into, which
      --  is the wider type. Get a pointer to it in the narrower type and
      --  copy V to it. The result is the memory.

      Emit_Assignment (Ptr_To_Relationship (Memory, V, Relationship (V)),
                       Value => V);
      return Memory;

   end Convert_Via_Copy;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive
     (V : GL_Value; No_Copy : Boolean := False) return GL_Value
   is
      In_GT  : constant GL_Type         := Related_Type (V);
      In_R   : constant GL_Relationship := Relationship (V);
      In_GTI : constant GL_Type_Info    := GL_Types.Table (In_GT);
      Out_GT : constant GL_Type         := Primitive_GL_Type (In_GT);
      Result : GL_Value                 := V;

   begin
      --  If we're already primitive, done

      if Is_Primitive_GL_Type (In_GT) then
         return Result;

      --  If this is Aligning, the object is the same, we just note that it
      --  now has the right type.

      elsif In_GTI.Kind = Aligning then
         return G_Is (Result, Out_GT);

      --  For Biased, we need to be sure we have data, then convert to
      --  the underlying type, then add the bias.

      elsif In_GTI.Kind = Biased then
         return Convert_Int (Get (Result, Data), Out_GT) + In_GTI.Bias;

      --  For Dummy, this must be an access type, so just convert to the
      --  proper pointer.

      elsif In_GTI.Kind = Dummy then
         return Convert_Pointer (Get (Result, Data), Out_GT);

      --  For Int_Alt, this must be an integral type, so convert it to
      --  the desired alternative.

      elsif In_GTI.Kind = Int_Alt then
         return Convert_Int (Get (Result, Data), Out_GT);

      --  For Access_Alt, convert to the other type of access type
      --  using our helper.

      elsif In_GTI.Kind = Access_Alt then
         return Convert_Access (Get (Result, Data), Out_GT);

      --  For Padded, use either GEP or Extract_Value, depending on whether
      --  this is a reference or not. But only do this for an actual
      --  Reference, not something else, such as a double reference or
      --  a reference to bounds and data

      elsif In_GTI.Kind = Padded and then In_R in Data | Reference then
         return (if   In_R =  Reference
                 then GEP (Out_GT,  Result,
                           (1 => Const_Null_32, 2 => Const_Null_32))
                 else Extract_Value (Out_GT, Result, 0));

      --  If this is an aggregate constant, we may be able to convert it

      elsif Can_Convert_Aggregate_Constant (Result, Out_GT) then
         return Convert_Aggregate_Constant (Result, Out_GT);

      --  Otherwise we need the data in memory

      else
         if Is_Double_Reference (Result) or else Is_Data (Result) then
            Result := Get (Result, Any_Reference);
         end if;

         --  If we're making a wider type, we can't just pun the pointer
         --  since it would result in an access outside the object, so we
         --  need to allocate a copy of the data and copy that. Use our
         --  helper. For tagged types, we know this punning is safe.

         if In_GTI.Kind = Truncated and then not No_Copy
           and then not Is_Tagged_Type (Out_GT)
         then
            return Convert_Via_Copy (Result, Out_GT);

         --  Otherwise, convert the pointer

         else
            return Ptr_To_Relationship (Result, Out_GT, Relationship (Result));
         end if;
      end if;

   end To_Primitive;

   --------------------
   -- From_Primitive --
   --------------------

   function From_Primitive
     (V       : GL_Value;
      GT      : GL_Type;
      No_Copy : Boolean := False) return GL_Value
   is
      GTI    : constant GL_Type_Info := GL_Types.Table (GT);
      Result : GL_Value              := V;

   begin
      --  If we're already the requested type, done

      if Related_Type (V) = GT then
         return Result;

      --  If the result is Aligning the object is the same, we just note
      --  that it now has the right type.

      elsif GTI.Kind = Aligning then
         return G_Is (Result, GT);

      --  For Biased, we need to be sure we have data, then subtract the
      --  bias, then convert to the underlying type.

      elsif GTI.Kind = Biased then
         return Trunc (Get (Result, Data) - GTI.Bias, GT);

      --  For Dummy, this must be an access type, so just convert to the
      --  proper pointer.

      elsif GTI.Kind = Dummy then
         return Convert_Pointer (Get (Result, Data), GT);

      --  For Int_Alt, this must be an integral type, so convert it to
      --  the desired alternative.

      elsif GTI.Kind = Int_Alt then
         return Convert_Int (Get (Result, Data), GT);

      --  For Access_Alt, convert to the other type of access type
      --  using our helper.

      elsif GTI.Kind = Access_Alt then
         return Convert_Access (Get (Result, Data), GT);

      --  For Padded data, use Insert_Value to make the padded version

      elsif GTI.Kind = Padded and then Is_Loadable_Type (Result) then
         return Insert_Value (Get_Undef (GT), Get (Result, Data), 0);

      --  If we're truncating and we have data, that data has to have been
      --  truncated, so we're fine.

      elsif GTI.Kind = Truncated and then Is_Data (Result) then
         return G_Is (Result, GT);

      --  If this is an aggregate constant, we may be able to convert it

      elsif Can_Convert_Aggregate_Constant (Result, GT) then
         return Convert_Aggregate_Constant (Result, GT);

      --  Otherwise we need the data in memory

      else
         Result := Get (Result, Any_Reference);

         --  If we're making a wider type, we can't just pun the pointer
         --  since this will result in an access outside the object, so we
         --  need to allocate a copy of the data and copy that. Use our
         --  helper. For tagged types, we know this punning is safe.

         if GTI.Kind in Padded | Byte_Array | Max_Size_Type
           and then not No_Copy and then not Is_Tagged_Type (GT)
         then
            return Convert_Via_Copy (Result, GT);

         --  Otherwise, convert the pointer

         else
            return Ptr_To_Relationship (Result, GT, Relationship (Result));
         end if;
      end if;

   end From_Primitive;

   ----------------
   -- Full_Etype --
   ----------------

   function Full_Etype (GT : GL_Type) return Void_Or_Type_Kind_Id is
     (GL_Types.Table (GT).GNAT_Type);

   -------------
   -- Type_Of --
   -------------

   function Type_Of (GT : GL_Type) return Type_T is
     (GL_Types.Table (GT).LLVM_Type);

   ------------------
   -- Base_GL_Type --
   -----------------

   function Base_GL_Type (TE : Type_Kind_Id) return GL_Type is
     (Primitive_GL_Type (Full_Base_Type (TE)));

   ------------------
   -- Base_GL_Type --
   -----------------

   function Base_GL_Type (GT : GL_Type) return GL_Type is
     (Primitive_GL_Type (Full_Base_Type (GT)));

   ------------------------
   -- Array_Base_GL_Type --
   ------------------------

   function Array_Base_GL_Type (GT : GL_Type) return GL_Type is
     ((if   Is_Packed_Array_Impl_Type (GT)
       then Primitive_GL_Type (Packed_Array_Impl_Type
                                 (Base_Type (Full_Original_Array_Type (GT))))
       else Base_GL_Type (GT)));

   ------------------------
   -- Full_Alloc_GL_Type --
   ------------------------

   function Full_Alloc_GL_Type (N : N_Subexpr_Id) return GL_Type is
      TE : Type_Kind_Id := Full_Etype (N);

   begin
      if Is_Entity_Name (N)
        and then (Ekind (Entity (N)) in E_Constant | E_Variable
                    or else Is_Formal (Entity (N)))
        and then Present (Actual_Subtype (Entity (N)))
      then
         TE := Get_Fullest_View (Actual_Subtype (Entity (N)));
      end if;

      return Default_GL_Type (TE);
   end Full_Alloc_GL_Type;

   --------------------
   --  Is_Dummy_Type --
   --------------------

   function Is_Dummy_Type (GT : GL_Type) return Boolean is
     (GT_Kind (GT) = Dummy);

   ---------------------------
   --  Is_Primitive_GL_Type --
   ---------------------------

   function Is_Primitive_GL_Type (GT : GL_Type) return Boolean is
     (GT_Kind (GT) = Primitive);

   ------------------------
   --  Is_Biased_GL_Type --
   ------------------------

   function Is_Biased_GL_Type (GT : GL_Type) return Boolean is
     (GT_Kind (GT) = Biased);

   -------------------------
   --  Is_Int_Alt_GL_Type --
   -------------------------

   function Is_Int_Alt_GL_Type (GT : GL_Type) return Boolean is
     (GT_Kind (GT) = Int_Alt);

   ------------------------
   --  Is_Padded_GL_Type --
   ------------------------

   function Is_Padded_GL_Type (GT : GL_Type) return Boolean is
     (GT_Kind (GT) = Padded);

   ---------------------------
   --  Is_Truncated_GL_Type --
   ---------------------------

   function Is_Truncated_GL_Type (GT : GL_Type) return Boolean is
     (GT_Kind (GT) = Truncated);

   ---------------------------
   --  Is_Bye_Array_GL_Type --
   ---------------------------

   function Is_Byte_Array_GL_Type (GT : GL_Type) return Boolean is
     (GT_Kind (GT) = Byte_Array);

   ----------------------
   -- Is_Empty_GL_Type --
   ----------------------

   function Is_Empty_GL_Type (GT : GL_Type) return Boolean is
     (GT_Kind (GT) = None);

   -----------------------
   -- Is_Nonnative_Type --
   -----------------------

   function Is_Nonnative_Type (GT : GL_Type) return Boolean is
      GTI  : constant GL_Type_Info := GL_Types.Table (GT);

   begin
      --  If we've built an LLVM type to do padding, then that's a native
      --  type. Otherwise, we have to look at whether the underlying type
      --  has a native representation or not.

      return GTI.Kind not in Padded | Byte_Array
        and then Is_Nonnative_Type (GTI.GNAT_Type);
   end Is_Nonnative_Type;

   -----------------------------
   -- Full_Designated_GL_Type --
   -----------------------------

   function Full_Designated_GL_Type (GT : Access_GL_Type) return GL_Type is
      TE : constant Type_Kind_Id := Full_Etype (GT);
      DT : constant GL_Type      := Get_Associated_GL_Type (TE);

   begin
      --  Normally, we've saved the associated GL_Type. But we don't do
      --  this in the E_Subprogram_Type case.

      return (if   Present (DT) then DT
              else Default_GL_Type (Full_Designated_Type (TE)));

   end Full_Designated_GL_Type;

   -----------------------------
   -- Full_Designated_GL_Type --
   -----------------------------

   function Full_Designated_GL_Type (V : GL_Value) return GL_Type is
      TE : constant Type_Kind_Id := Full_Etype (Related_Type (V));

   begin
      --  If this isn't an actual access type, but a reference to
      --  something, the type is that thing.

      if Is_Reference (V) then
         return Related_Type (V);

      --  Otherwise, return the associated type, if there is one, of the
      --  designated type.

      elsif Present (Get_Associated_GL_Type (TE)) then
         return Get_Associated_GL_Type (TE);

      --  Otherwise, get the default_GL_Type of what it points to (the --
      --  E_Subprogram_Type case).

      else
         return Default_GL_Type (Full_Designated_Type (TE));
      end if;

   end Full_Designated_GL_Type;

   ------------------
   -- C_Set_Entity --
   ------------------

   procedure C_Set_Entity (V : GL_Value; GT : GL_Type) is
   begin
      C_Set_Entity (V, Full_Etype (GT));
   end C_Set_Entity;

   ------------------
   -- C_Set_Entity --
   ------------------

   procedure C_Set_Entity
     (V : Value_T; GT : GL_Type; Reference : Boolean := False)
   is
   begin
      C_Set_Entity (V, Full_Etype (GT), Reference => Reference);
   end C_Set_Entity;

   ------------------
   -- Dump_GL_Type --
   ------------------

   procedure Dump_GL_Type (GT : GL_Type) is
   begin
      if No (GT) then
         Write_Line ("None");
         return;
      end if;

      Dump_GL_Type_Int (GT, True);
   end Dump_GL_Type;

   ----------------------
   -- Dump_GL_Type_Int --
   ----------------------

   procedure Dump_GL_Type_Int (GT : GL_Type; Full_Dump : Boolean) is
      GTI  : constant GL_Type_Info := GL_Types.Table (GT);

      procedure Write_Int_From_LLI (J : LLI);

      ------------------------
      -- Write_Int_From_ULL --
      ------------------------

      procedure Write_Int_From_LLI (J : LLI) is
      begin
         if J < LLI (Int'First) or else J > LLI (Int'Last) then
            Write_Str ("<overflow>");
         else
            Write_Int (Int (J));
         end if;
      end Write_Int_From_LLI;

   begin
      Write_Str (GT_Kind_Type'Image (GTI.Kind) & "(");
      Write_Int (Int (GTI.GNAT_Type));

      if Present (GTI.Size) then
         Write_Str (", S=");
         Write_Int_From_LLI (+GTI.Size);
      end if;

      if GTI.Alignment /= 0 then
         Write_Str (", A=");
         Write_Int (GTI.Alignment);
      end if;

      if Present (GTI.Bias) then
         Write_Str (", B=");
         Write_Int_From_LLI (+GTI.Bias);
      end if;

      if GTI.Default then
         Write_Str (", default");
      end if;

      Write_Str (")");

      if Full_Dump then
         Write_Str (": ");

         if Present (GTI.LLVM_Type) then
            Dump_LLVM_Type (GTI.LLVM_Type);
         end if;

         pg (Union_Id (GTI.GNAT_Type));

         if Present (GTI.TBAA) then
            Dump_LLVM_Metadata (GTI.TBAA);
         end if;
      end if;
   end Dump_GL_Type_Int;

begin
   --  Make a dummy entry in the table, so the "No" entry is never used.

   GL_Types.Increment_Last;
end GNATLLVM.GLType;
