------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2025, AdaCore                          --
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

with Output; use Output;
with Table;  use Table;

with GNATLLVM.Types; use GNATLLVM.Types;
with GNATLLVM.Utils; use GNATLLVM.Utils;

with CCG; use CCG;

package body GNATLLVM.MDType is

   --  Define the enumeration that says what kind of type this is.

   type MD_Kind is
     (Continuation,
      --  A continuation of a type, presently used for function and struct
      --  types. The related type is the type of the next field or
      --  parameter.  For the fields in a struct, the flag indicates that
      --  the field represents padding.

      Void,
      --  A null type, which is used for a function that doesn't return,
      --  a generic pointer, or a pointer where we don't know what it
      --  points to.

      Integer,
      --  An integer. The count field is the number of bits and the flag
      --  indicates signedness.

      Unknown_Integer,
      --  Like Integer, but where we don't know the signedness

      Float,
      --  A floating-point value. The count field is the number of bits.

      Array_Type,
      --  An array. The count field is the number of elements, if fixed
      --  size, the related type is the element type, and the flag is true
      --  for a variable-size array.

      Struct,
      --  A struct. All fields are in Continuation entries since they need
      --  a name field and we want to use the name field in this kind for
      --  the name of the type. The Count is the number of fields and the
      --  flag is True if the struct is packed.

      Pointer,
      --  A pointer to another type.  The count is the address space and
      --  the related type is the type being pointed to.

      Func);
   --  A function type. The related type is the return type and the
   --  parameter types are given in the type continuations. The Count is
   --  the number of parameters. The flag is True if this is a varargs
   --  function.

   --  Now define the representation of an MD_Type.

   type MD_Type_Info is record
      Kind           : MD_Kind;
      --  The kind of type or type part

      Count          : Nat     := 0;
      --  An integer used in various ways depending on the kind

      Related_Type   : MD_Type := No_MD_Type;
      --  Another type related to this one depending on the kind

      Cont_Type      : MD_Type := No_MD_Type;
      --  If Present, the continuation of this type

      Hash_Link      : MD_Type := No_MD_Type;
      --  The next MD_Type in the hash chain, if Present

      Name           : Name_Id := No_Name;
      --  The name of this type or field (for continuation)

      LLVM_Type      : Type_T  := No_Type_T;
      --  The actual LLVM type

      Is_Volatile    : Boolean := False;
      --  Whether the type is volatile

      Has_Fields     : Boolean := False;
      --  For Struct, indicates that we have set the fields for this
      --  type, meaning that we called either Build_Struct_Type or both
      --  Struct_Create_Named and Struct_Set_Body.

      Entity         : Opt_Record_Field_Kind_Id := Empty;
      --  ??? Temporarily store the entity here

      Flag           : Boolean := False;
      --  Used as deined above for each kind
   end record;

   function "=" (Info1, Info2 : MD_Type_Info) return Boolean;

   package MD_Types is new Table.Table
     (Table_Component_Type => MD_Type_Info,
      Table_Index_Type     => MD_Type'Base,
      Table_Low_Bound      => MD_Type_Low_Bound,
      Table_Initial        => 500,
      Table_Increment      => 20,
      Table_Name           => "MD_Types");

   type MD_Hash_Type is mod 2 ** 32;
   Hash_Num : constant MD_Hash_Type := 4093;
   subtype Hash_Index_Type is MD_Hash_Type range 0 .. Hash_Num - 1;

   Hash_Table : array (Hash_Index_Type) of MD_Type := (others => No_MD_Type);

   function Hash (Info : MD_Type_Info) return Hash_Index_Type;
   function MD_Find (Info : MD_Type_Info) return MD_Type;

   --  Here are the internal accessors for MD_Type components

   function Kind (MD : MD_Type) return MD_Kind is
     (MD_Types.Table (MD).Kind)
     with Pre => Present (MD), Post => Kind'Result /= Continuation;

   function Related (MD : MD_Type) return MD_Type is
     (MD_Types.Table (MD).Related_Type)
     with Pre => Present (MD), Post => Present (Related'Result);

   function Continuation_Type (MD : MD_Type) return MD_Type is
     (MD_Types.Table (MD).Cont_Type)
     with Pre => Present (MD);

   function LLVM_Type (MD : MD_Type) return Type_T is
     (MD_Types.Table (MD).LLVM_Type)
     with Pre => Present (MD);

   function MD_Count (MD : MD_Type) return Nat is
     (MD_Types.Table (MD).Count)
     with Pre => Present (MD);

   function MD_Entity (MD : MD_Type) return Opt_Record_Field_Kind_Id is
     (MD_Types.Table (MD).Entity)
     with Pre => Present (MD);

   function Flag (MD : MD_Type) return Boolean is
     (MD_Types.Table (MD).Flag)
     with Pre => Present (MD);
   function Not_Flag (MD : MD_Type) return Boolean is (not Flag (MD));

   procedure Struct_Set_Body_Internal (MD : MD_Type)
     with Pre => Has_Fields (MD);
   --  Set up the LLVM type corresponding to MD with the field information
   --  from MD. This may either create or update the type.

   procedure Set_LLVM_Type (MD : MD_Type; T : Type_T)
     with Pre =>  Present (MD) and then Present (T)
                  and then No (LLVM_Type (MD)),
          Post => LLVM_Type (MD) = T, Inline;
   --  Set the LLVM_Type of MD to T

   -----------
   -- Class --
   -----------

   function Class (MD : MD_Type) return MD_Type_Class is
   begin
      case Kind (MD) is
         when Void =>
            return Void_Class;

         when Integer | Unknown_Integer =>
            return Integer_Class;

         when Float =>
            return Float_Class;

         when Array_Type =>
            return Array_Class;

         when Struct =>
            return Struct_Class;

         when Pointer =>
            return Pointer_Class;

         when Func =>
            return Function_Class;

         when others =>
            pragma Assert (False);
            return Void_Class;
      end case;
   end Class;

   ----------
   -- Hash --
   ----------

   function Hash (Info : MD_Type_Info) return Hash_Index_Type is
     ((MD_Hash_Type (MD_Kind'Pos (Info.Kind)) +
       MD_Hash_Type ((Info.Name - Names_Low_Bound) * 11) +
       MD_Hash_Type (Info.Count * 3) +
       MD_Hash_Type ((Info.Related_Type - MD_Type'First) * 5) +
       MD_Hash_Type ((Info.Cont_Type - MD_Type'First) * 7) +
       MD_Hash_Type (Info.Entity * 42) +
       MD_Hash_Type (Boolean'Pos (Info.Is_Volatile) * 1283) +
       MD_Hash_Type (Boolean'Pos (Info.Flag) * 2039))
     mod Hash_Num);

   ---------
   -- "=" --
   ---------

   function "=" (Info1, Info2 : MD_Type_Info) return Boolean is
     (Info1.Kind = Info2.Kind and then Info1.Name = Info2.Name
      and then Info1.Count = Info2.Count
      and then Info1.Related_Type = Info2.Related_Type
      and then Info1.Cont_Type = Info2.Cont_Type
      and then Info1.Entity = Info2.Entity
      and then Info1.Is_Volatile = Info2.Is_Volatile
      and then Info1.Flag = Info2.Flag);
   --  Note that we don't want to compare Hash_Link and LLVM_Type because
   --  we want to know that the types are structually the same and those
   --  fields don't relate to the structure.

   -------------------
   -- Set_LLVM_Type --
   -------------------

   procedure Set_LLVM_Type (MD : MD_Type; T : Type_T) is
   begin
      MD_Types.Table (MD).LLVM_Type := T;
      C_Set_MD_Type (T, MD);
   end Set_LLVM_Type;

   ----------------
   -- Has_Fields --
   ----------------

   function Has_Fields (MD : MD_Type) return Boolean is
     (MD_Types.Table (MD).Has_Fields);

   ------------------
   -- Is_Same_Kind --
   ------------------

   function Is_Same_Kind (MD1, MD2 : MD_Type) return Boolean is
     (Kind (MD1) = Kind (MD2)
      or else (Is_Integer (MD1) and then Is_Integer (MD2)));

   -------------
   -- Is_Void --
   -------------

   function Is_Void (MD : MD_Type) return Boolean is (Kind (MD) = Void);

   ----------------
   -- Is_Integer --
   ----------------

   function Is_Integer (MD : MD_Type) return Boolean is
     (Kind (MD) in Integer | Unknown_Integer);

   ----------------
   -- Is_Float --
   ----------------

   function Is_Float (MD : MD_Type) return Boolean is (Kind (MD) = Float);

   ---------------
   -- Is_Signed --
   ---------------

   function Is_Signed (MD : MD_Type) return Boolean is
     (Kind (MD) = Integer and then not Flag (MD));

   -----------------
   -- Is_Unsigned --
   -----------------

   function Is_Unsigned (MD : MD_Type) return Boolean renames Flag;

   ---------------------
   -- Is_Unknown_Sign --
   --------------------

   function Is_Unknown_Sign (MD : MD_Type) return Boolean is
     (Kind (MD) = Unknown_Integer);

   --------------
   -- Is_Array --
   --------------

   function Is_Array (MD : MD_Type) return Boolean is
     (Kind (MD) = Array_Type);

   --------------------
   -- Is_Fixed_Array --
   --------------------

   function Is_Fixed_Array (MD : MD_Type) return Boolean renames Not_Flag;

   -----------------------
   -- Is_Variable_Array --
   -----------------------

   function Is_Variable_Array (MD : MD_Type) return Boolean renames Flag;

   -----------------------
   -- Is_Variable_Array --
   -----------------------

   function Is_Varargs_Function (MD : MD_Type) return Boolean renames Flag;

   --------------
   -- Is_Struct --
   --------------

   function Is_Struct (MD : MD_Type) return Boolean is (Kind (MD) = Struct);

   ---------------
   -- Is_Packed --
   ---------------

   function Is_Packed (MD : MD_Type) return Boolean renames Flag;

   ----------------
   -- Is_Pointer --
   ----------------

   function Is_Pointer (MD : MD_Type) return Boolean is
     (Kind (MD) = Pointer);

   ----------------------
   -- Is_Function_Type --
   ----------------------

   function Is_Function_Type (MD : MD_Type) return Boolean is
     (Kind (MD) = Func);

   --------------
   -- Int_Bits --
   --------------

   function Int_Bits (MD : MD_Type) return Nat renames MD_Count;

   ----------------
   -- Float_Bits --
   ----------------

   function Float_Bits (MD : MD_Type) return Nat renames MD_Count;

   -----------------
   -- Array_Count --
   -----------------

   function Array_Count (MD : MD_Type) return Nat renames MD_Count;

   -------------------
   -- Element_Count --
   -------------------

   function Element_Count (MD : MD_Type) return Nat renames MD_Count;

   -------------------
   -- Pointer_Space --
   -------------------

   function Pointer_Space (MD : MD_Type) return Nat renames MD_Count;

   ---------------------
   -- Parameter_Count --
   ---------------------

   function Parameter_Count (MD : MD_Type) return Nat renames MD_Count;

   ---------------------
   -- Designated_Type --
   ---------------------

   function Designated_Type (MD : MD_Type) return MD_Type renames Related;

   ------------------
   -- Element_Type --
   ------------------

   function Element_Type (MD : MD_Type) return MD_Type renames Related;

   -----------------
   -- Return_Type --
   -----------------

   function Return_Type (MD : MD_Type) return MD_Type renames Related;

   -----------------
   -- Is_Volatile --
   -----------------

   function Is_Volatile (MD : MD_Type) return Boolean is
      (MD_Types.Table (MD).Is_Volatile);

   ----------
   -- Name --
   ----------

   function MD_Name (MD : MD_Type) return Name_Id is
     (MD_Types.Table (MD).Name);

   ------------------
   -- Element_Name --
   ------------------

   function Element_Name (MD : MD_Type; Idx : Nat) return Name_Id is
      E_MD : MD_Type := Continuation_Type (MD);

   begin
      for J in 0 .. Idx - 1 loop
         E_MD := Continuation_Type (E_MD);
      end loop;

      return MD_Name (E_MD);
   end Element_Name;

   ------------------
   -- Element_Type --
   ------------------

   function Element_Type (MD : MD_Type; Idx : Nat) return MD_Type is
      E_MD : MD_Type := Continuation_Type (MD);

   begin
      for J in 0 .. Idx - 1 loop
         E_MD := Continuation_Type (E_MD);
      end loop;

      return Related (E_MD);
   end Element_Type;

   ------------------
   -- Element_Entity --
   ------------------

   function Element_Entity (MD : MD_Type; Idx : Nat) return Entity_Id is
      E_MD : MD_Type := Continuation_Type (MD);

   begin
      for J in 0 .. Idx - 1 loop
         E_MD := Continuation_Type (E_MD);
      end loop;

      return MD_Entity (E_MD);
   end Element_Entity;

   -----------------
   --  Is_Padding --
   -----------------

   function Is_Padding (MD : MD_Type; Idx : Nat) return Boolean is
      E_MD : MD_Type := Continuation_Type (MD);

   begin
      for J in 0 .. Idx - 1 loop
         E_MD := Continuation_Type (E_MD);
      end loop;

      return Flag (E_MD);
   end Is_Padding;

   --------------------
   -- Parameter_Name --
   --------------------

   function Parameter_Name (MD : MD_Type; Idx : Nat) return Name_Id is
      E_MD : MD_Type := Continuation_Type (MD);

   begin
      for J in 0 .. Idx - 1 loop
         E_MD := Continuation_Type (E_MD);
      end loop;

      return MD_Name (E_MD);
   end Parameter_Name;

   --------------------
   -- Parameter_Type --
   --------------------

   function Parameter_Type (MD : MD_Type; Idx : Nat) return MD_Type is
      E_MD : MD_Type := Continuation_Type (MD);

   begin
      for J in 0 .. Idx - 1 loop
         E_MD := Continuation_Type (E_MD);
      end loop;

      return Related (E_MD);
   end Parameter_Type;

   -------------
   -- MD_Find --
   -------------

   function MD_Find (Info : MD_Type_Info) return MD_Type is
      Hash_Index : constant Hash_Index_Type := Hash (Info);
      New_Id     : MD_Type                  := Hash_Table (Hash_Index);
      Prev_Id    : MD_Type                  := New_Id;

   begin
      if No (New_Id) then
         MD_Types.Append (Info);
         Hash_Table (Hash_Index) := MD_Types.Last;
         return MD_Types.Last;
      end if;

      while Present (New_Id) loop
         if MD_Types.Table (New_Id) = Info then
            return New_Id;
         end if;

         Prev_Id := New_Id;
         New_Id  := MD_Types.Table (Prev_Id).Hash_Link;
      end loop;

      MD_Types.Append (Info);
      MD_Types.Table (Prev_Id).Hash_Link := MD_Types.Last;
      return MD_Types.Last;
   end MD_Find;

   -------------
   -- Void_Ty --
   -------------

   function Void_Ty return MD_Type is
     (MD_Find ((Kind => Void, others => <>)));

   ------------
   -- Int_Ty --
   ------------

   function Int_Ty
     (Bits     : Nat;
      Unsigned : Boolean := False;
      Unknown  : Boolean := False) return MD_Type
   is
     (MD_Find
        ((Kind   => (if Unknown then Unknown_Integer else Integer),
          Count  => Bits,
          Flag   => Unsigned,
          others => <>)));

   ------------
   -- Float_Ty --
   ------------

   function Float_Ty (Bits : Nat) return MD_Type is
     (MD_Find ((Kind => Float, Count => Bits, others => <>)));

   ------------------
   -- Pointer_Type --
   ------------------

   function Pointer_Type
     (Elem_Type : MD_Type;
      Space     : Nat := Address_Space) return MD_Type
   is
      (MD_Find ((Kind         => Pointer,
                Related_Type => Elem_Type,
                Count        => Space,
                others       => <>)));

   ----------------
   -- Array_Type --
   ----------------

   function Array_Type (Elem_Type : MD_Type; Count : Nat) return MD_Type is
     (MD_Find ((Kind         => Array_Type,
                Count        => Count,
                Related_Type => Elem_Type,
                others       => <>)));

   -----------------------
   -- Build_Struct_Type --
   -----------------------

   function Build_Struct_Type
     (Types       : MD_Type_Array;
      Field_Names : Name_Id_Array;
      Fields      : Field_Id_Array := (1 .. 0 => Empty);
      Padding     : Boolean_Array  := (1 .. 0 => False);
      Packed      : Boolean        := False;
      Name        : Name_Id        := No_Name) return MD_Type
   is
      Info : MD_Type_Info :=
        (Kind       => Struct,
         Count      => Field_Names'Length,
         Has_Fields => True,
         Flag       => Packed,
         Name       => Name,
         others     => <>);
      Prev : MD_Type := No_MD_Type;

   begin
      --  We build continuation type records in reverse order and point the
      --  actual type to the first of them (the last we create).

      for J in reverse Field_Names'Range loop
         Prev := MD_Find ((Kind         => Continuation,
                           Related_Type => Types (J),
                           Name         => Field_Names (J),
                           Entity       => (if   Fields'Length > 0
                                            then Fields (J) else Empty),
                           Cont_Type    => Prev,
                           Flag         => (if   Padding'Length > 0
                                            then Padding (J) else False),
                           others       => <>));
      end loop;

      Info.Cont_Type := Prev;
      return MD_Find (Info);
   end Build_Struct_Type;

   ---------------------
   -- Struct_Set_Body --
   ---------------------

   procedure Struct_Set_Body
     (MD      : MD_Type;
      Types   : MD_Type_Array;
      Names   : Name_Id_Array;
      Fields  : Field_Id_Array := (1 .. 0 => Empty);
      Padding : Boolean_Array  := (1 .. 0 => False);
      Packed  : Boolean        := False)
   is
      Prev : MD_Type := No_MD_Type;

   begin
      --  We build continuation type records in reverse order and point the
      --  actual type to the first of them (the last we create).

      for J in reverse Names'Range loop
         Prev := MD_Find ((Kind         => Continuation,
                           Related_Type => Types (J),
                           Name         => Names (J),
                           Entity       => (if   Fields'Length > 0
                                            then Fields (J) else Empty),
                           Flag         => (if   Padding'Length > 0
                                            then Padding (J) else False),
                           Cont_Type    => Prev,
                           others       => <>));
      end loop;

      MD_Types.Table (MD).Cont_Type  := Prev;
      MD_Types.Table (MD).Has_Fields := True;
      MD_Types.Table (MD).Flag       := Packed;
      MD_Types.Table (MD).Count      := Names'Length;

   end Struct_Set_Body;

   -------------------------
   -- Struct_Create_Named --
   -------------------------

   function Struct_Create_Named (Name : Name_Id) return MD_Type is
   begin
      MD_Types.Append ((Kind   => Struct,
                        Name   => Name,
                        Count  => 0,
                        others => <>));
      return MD_Types.Last;
   end Struct_Create_Named;

   -------------------------
   -- Variable_Array_Type --
   -------------------------

   function Variable_Array_Type (Elem_Type : MD_Type) return MD_Type is
   begin
      return MD_Find ((Kind         => Array_Type,
                       Count        => 0,
                       Related_Type => Elem_Type,
                       Flag         => True,
                       others       => <>));
   end Variable_Array_Type;

   -----------
   -- Fn_Ty --
   -----------

   function Fn_Ty
     (Arg_Types   : MD_Type_Array;
      Return_Type : MD_Type;
      Arg_Names   : Name_Id_Array := (1 .. 0 => No_Name);
      Varargs     : Boolean       := False) return MD_Type
   is
      Info : MD_Type_Info :=
        (Kind         => Func,
         Flag         => Varargs,
         Count        => Arg_Types'Length,
         Related_Type => Return_Type,
         others       => <>);
      Prev : MD_Type := No_MD_Type;

   begin
      --  We build continuation type records in reverse order and point the
      --  actual type to the first of them (the last we create).

      for J in reverse Arg_Types'Range loop
         Prev := MD_Find ((Kind         => Continuation,
                           Related_Type => Arg_Types (J),
                           Name         => (if   Arg_Names'Length > 0
                                            then Arg_Names (J) else No_Name),
                           Cont_Type    => Prev,
                           others       => <>));
      end loop;

      Info.Cont_Type := Prev;
      return MD_Find (Info);
   end Fn_Ty;

   ---------------
   -- Name_Type --
   ---------------

   function Name_Type (MD : MD_Type; New_Name : Name_Id) return MD_Type is
      Info : MD_Type_Info := MD_Types.Table (MD);

   begin
      Info.Name := New_Name;
      return MD_Find (Info);
   end Name_Type;

   -------------------
   -- Make_Volatile --
   -------------------

   function Make_Volatile (MD : MD_Type; B : Boolean := True) return MD_Type
   is
      Info : MD_Type_Info := MD_Types.Table (MD);

   begin
      Info.Is_Volatile := B;
      return MD_Find (Info);
   end Make_Volatile;

   -------------------
   -- Make_Volatile --
   -------------------

   procedure Make_Volatile (MD : in out MD_Type; B : Boolean := False) is
   begin
      MD := Make_Volatile (MD, B);
   end Make_Volatile;

   ------------------------------
   -- Struct_Set_Body_Internal --
   ------------------------------

   procedure Struct_Set_Body_Internal (MD : MD_Type) is
      UID   : constant Unique_Id :=
        (if Has_Fields (MD) then New_Unique_Id else No_Unique_Id);
      C_MD : MD_Type := Continuation_Type (MD);
      Typs  : Type_Array (1 .. Element_Count (MD));

   begin
      for J in Typs'Range loop
         Typs (J) := +Related (C_MD);
         C_Set_Field_Info (UID, J - Typs'First, MD_Name (C_MD),
                           Is_Padding => not Has_Name (C_MD),
                           Entity     => MD_Entity (C_MD));
         C_MD := Continuation_Type (C_MD);
      end loop;

      if No (MD_Name (MD)) then
         Set_LLVM_Type (MD, Struct_Type (Typs'Address, Typs'Length,
                                          Is_Packed (MD)));
      else
         if No (LLVM_Type (MD)) then
            Set_LLVM_Type (MD,
                           Struct_Create_Named
                             (Get_Global_Context,
                              Get_Name_String (MD_Name (MD))));
         end if;

         Struct_Set_Body (LLVM_Type (MD), Typs'Address, Typs'Length,
                          Is_Packed (MD));
      end if;

      C_Set_Struct (UID, LLVM_Type (MD));
   end Struct_Set_Body_Internal;

   ------------------
   -- LLVM_Type_Of --
   ------------------

   function LLVM_Type_Of (MD : MD_Type) return Type_T is
      Result : Type_T;

   begin
      --  If we already made an LLVM type, return it

      if Present (LLVM_Type (MD)) then

         --  However, if that's an opaque type, but we've set our field list,
         --  update that type first.

         if Is_Struct (MD) and then Has_Fields (MD)
           and then Is_Opaque_Struct (LLVM_Type (MD))
         then
            Struct_Set_Body_Internal (MD);
         end if;

         return LLVM_Type (MD);
      end if;

      case Kind (MD) is
         when Void =>
            Result := Void_Type;

         when Integer | Unknown_Integer =>
            Result := Int_Type (unsigned (Int_Bits (MD)));

         when Float =>
            case Float_Bits (MD) is
               when 32 =>
                  Result := Float_Type;

               when 64 =>
                  Result := Double_Type;

               when 80 | 96 | 128 =>
                  --  Extended precision; not IEEE_128
                  Result := X86FP80_Type;

               when others =>
                  pragma Assert (Decls_Only);
                  Result := Byte_T;
            end case;

         when Pointer =>
            Result := Pointer_Type ((if   Is_Void (Designated_Type (MD))
                                     then Byte_T else +Designated_Type (MD)),
                                    unsigned (Pointer_Space (MD)));

         when Array_Type =>
            Result := Array_Type (+Element_Type (MD),
                                  (if   Is_Fixed_Array (MD)
                                   then unsigned (Array_Count (MD)) else 0));

         when Struct =>
            if Has_Fields (MD) then
               Struct_Set_Body_Internal (MD);
               return LLVM_Type (MD);
            else
               Result := Struct_Create_Named (Get_Global_Context,
                                              Get_Name_String (MD_Name (MD)));
            end if;

         when Func =>
            declare
               C_MD : MD_Type          := Continuation_Type (MD);
               Ret_T : constant Type_T  :=
                 (if   Is_Void (Related (MD)) then Void_Type
                  else +Related (MD));
               Types : Type_Array (1 .. Parameter_Count (MD));

            begin
               --  Process each argument type. If one type is void
               --  that means we don't know the type, but void isn't
               --  valid, so convert it to void *, which is the best
               --  we can do. This case shouldn't occur in situations
               --  where it matters (it occurs when types are metadata,
               --  for example.

               for J in Types'Range loop
                  Types (J) := (if   Is_Void (Related (C_MD))
                                then Void_Ptr_T else +Related (C_MD));
                  C_MD     := Continuation_Type (C_MD);
               end loop;

               Result := Fn_Ty (Types, Ret_T, Is_Varargs_Function (MD));
            end;

         when others =>
            Result := Void_Ptr_T;
      end case;

      Set_LLVM_Type (MD, Result);
      return Result;
   end LLVM_Type_Of;

   ---------------
   -- From_Type --
   ---------------

   function From_Type (T : Type_T) return MD_Type is
   begin
      --  First see if we already know about this type. This will almost
      --  never be true for the toplevel call, but may be true for internal
      --  calls.

      if Present (C_Get_MD_Type (T)) then
         return C_Get_MD_Type (T);
      end if;

      case Get_Type_Kind (T) is
         when Integer_Type_Kind =>
            return Int_Ty (Nat (Get_Scalar_Bit_Size (T)), Unknown => True);

         when Float_Type_Kind =>
            return Float_Ty (32);

         when Double_Type_Kind =>
            return Float_Ty (64);

         when X86_FP80_Type_Kind =>
            return Float_Ty (80);

         when Pointer_Type_Kind =>
            return Void_Ptr_MD;

         when Array_Type_Kind =>
            return Array_Type (From_Type (Get_Element_Type (T)),
                               Nat (Get_Array_Length (T)));

         when Struct_Type_Kind =>
            declare
               Num_Elts : constant Nat    :=
                 Nat (Count_Struct_Element_Types (T));
               Name     : constant String := Get_Struct_Name (T);
               Types    : Type_Array (1 .. Num_Elts);
               MDs      : MD_Type_Array (1 .. Num_Elts);

            begin
               Get_Struct_Element_Types (T, Types'Address);

               for J in Types'Range loop
                  MDs (J) := From_Type (Types (J));
               end loop;

               return Build_Struct_Type (MDs, (MDs'Range => No_Name),
                                         Packed => Is_Packed_Struct (T),
                                         Name   =>
                                           (if   Name'Length = 0 then No_Name
                                            else Name_Find (Name)));
            end;

         when Function_Type_Kind =>
            declare
               Num_Params : constant Nat := Nat (Count_Param_Types (T));
               Types      : Type_Array (1 .. Num_Params);
               MDs        : MD_Type_Array (1 .. Num_Params);

            begin
               Get_Param_Types (T, Types'Address);

               for J in Types'Range loop
                  MDs (J) := From_Type (Types (J));
               end loop;

               return Fn_Ty (MDs, From_Type (Get_Return_Type (T)));
            end;

         when Void_Type_Kind | Metadata_Type_Kind =>
            return Void_Ty;

         when others =>
            pragma Assert (False);
            return Void_Ty;
      end case;
   end From_Type;

   -------------------
   -- Get_Type_Size --
   -------------------

   function Get_Type_Size (MD : MD_Type) return ULL is
     (Get_Type_Size (Type_T'(+MD)));

   -------------------------
   -- Get_Scalar_Bit_Size --
   -------------------------

   function Get_Scalar_Bit_Size (MD : MD_Type) return ULL is
     (Get_Scalar_Bit_Size (Type_T'(+MD)));

   ------------------------
   -- Get_Type_Alignment --
   ------------------------

   function Get_Type_Alignment (MD : MD_Type) return Nat is
     (Get_Type_Alignment (Type_T'(+MD)));

   -------------------
   -- Contains_Void --
   -------------------

   function Contains_Void (MD : MD_Type) return Boolean is
   begin
      if Is_Void (MD)
        or else (Is_Array (MD) and then Contains_Void (Element_Type (MD)))
      then
         return True;

      elsif Is_Struct (MD) then
         for J in 0 .. Element_Count (MD) - 1 loop
            if Contains_Void (Element_Type (MD, J)) then
               return True;
            end if;
         end loop;

      elsif Is_Function_Type (MD) then
         if Contains_Void (Return_Type (MD)) then
            return True;
         else
            for J in 0 .. Parameter_Count (MD) - 1 loop
               if Contains_Void (Parameter_Type (MD, J)) then
                  return True;
               end if;
            end loop;
         end if;
      end if;

      return False;
   end Contains_Void;

   ---------------------
   -- Check_From_Type --
   ---------------------

   function Check_From_Type (T1, T2 : Type_T) return Boolean
      renames Is_Layout_Identical;

   -------------------------
   -- Is_Layout_Identical --
   -------------------------

   function Is_Layout_Identical
     (MD1, MD2 : MD_Type; Strict : Boolean := False) return Boolean
   is
   begin
      --  If the types are the same, they're identical, but if they have
      --  different kinds, they aren't.

      if MD1 = MD2 then
         return True;
      elsif not Is_Same_Kind (MD1, MD2) then
         return False;

      --  Otherwise, it's kind-specific

      elsif Is_Array (MD1)
        and then Is_Fixed_Array (MD1) and then Is_Fixed_Array (MD2)
        and then Array_Count (MD1) = Array_Count (MD2)
        and then Is_Layout_Identical (Element_Type (MD1), Element_Type (MD2))
      then
         return True;

      elsif Is_Struct (MD1)
        and then Is_Packed (MD1) = Is_Packed (MD2)
        and then Has_Fields (MD1) = Has_Fields (MD2)
        and then (not Has_Fields (MD1)
                  or else Element_Count (MD1) = Element_Count (MD2))
      then
         --  Structures are identical if their packed status is the same,
         --  they have the same number of fields, and each field is
         --  identical.

         for J in 0 ..
           (if Has_Fields (MD1) then Element_Count (MD1) else 0) - 1 loop
            if not Is_Layout_Identical (Element_Type (MD1, J),
                                        Element_Type (MD2, J))
            then
               return False;
            end if;
         end loop;

         return True;

      --  Pointers have the same layout if they're pointing at the
      --  same address space and, if we're being strict, pointing at
      --  the same type.

      elsif Is_Pointer (MD1)
        and then Pointer_Space (MD1) = Pointer_Space (MD2)
        and then (not Strict
                  or else (Designated_Type (MD1) = Designated_Type (MD2)))
      then
         return True;

      --  Two function types have different layouts if their return types
      --  have different layouts or they have a different number of
      --  parameter types.

      elsif Is_Function_Type (MD1)
        and then Is_Layout_Identical (Return_Type (MD1), Return_Type (MD2))
        and then Parameter_Count (MD1) = Parameter_Count (MD2)
      then
         --  If any parameter type is not the identical layout of the
         --  corresponding parameter type, the layouts aren't the same.

         for J in 1 .. Parameter_Count (MD1) loop
            if not Is_Layout_Identical (Parameter_Type (MD1, J),
                                        Parameter_Type (MD2, J))
            then
               return False;
            end if;
         end loop;

         return True;

      --  Otherwise, types are only identical if they're the same and that
      --  was checked above.

      else
         return False;
      end if;
   end Is_Layout_Identical;

   ---------------
   -- To_String --
   ---------------

   function To_String (MD : MD_Type; Top : Boolean := False) return String is
      C_MD   : MD_Type;
      Result : Bounded_String;

   begin
      if No (MD) then
         return "void ";
      elsif not Top and then Present (MD_Name (MD)) then
         return Get_Name_String (MD_Name (MD));
      end if;

      C_MD := Continuation_Type (MD);
      if Is_Volatile (MD) then
         Append (Result, "volatile ");
      end if;

      case Kind (MD) is
         when Void =>
            Append (Result, "void");

         when Integer | Unknown_Integer =>

            if Is_Unsigned (MD) then
               Append (Result, 'u');
            end if;

            Append (Result, "int_");
            Append (Result, Int_Bits (MD));
            Append (Result, "t ");

         when Float =>

            Append (Result, "float_");
            Append (Result, Float_Bits (MD));
            Append (Result, "t ");

         when Array_Type =>
            Append (Result, To_String (Element_Type (MD)));
            Append (Result, "[");

            if Is_Fixed_Array (MD) then
               Append (Result, Array_Count (MD));
            end if;

            Append (Result, "]");

         when Struct =>

            if Is_Packed (MD) then
               Append (Result, "packed ");
            end if;

            Append (Result, "struct ");

            if Has_Name (MD) then
               Append (Result, MD_Name (MD));
               Append (Result, " ");
            end if;

            Append (Result, "{");

            while Present (C_MD) loop
               Append (Result, To_String (Related (C_MD)));

               if Has_Name (C_MD) then
                  Append (Result, " ");
                  Append (Result, MD_Name (C_MD));
               end if;

               C_MD := Continuation_Type (C_MD);

               if Present (C_MD) then
                  Append (Result, ", ");
               end if;
            end loop;

            Append (Result, "}");

         when Pointer =>
            Append (Result, To_String (Designated_Type (MD)));
            Append (Result, "*");
            if Pointer_Space (MD) /= 0 then
               Append (Result, "{");
               Append (Result, Pointer_Space (MD));
               Append (Result, "}");
            end if;

         when Func =>
            Append (Result, To_String (Return_Type (MD)));
            Append (Result, "() (");

            while Present (C_MD) loop
               Append (Result, To_String (Related (C_MD)));
               C_MD := Continuation_Type (C_MD);

               if Present (C_MD) then
                  Append (Result, ", ");
               end if;
            end loop;

            if Is_Varargs_Function (MD) then
               Append (Result, ", ..");
            end if;

            Append (Result, ")");

         when others =>
            raise Program_Error;
      end case;

      if not Is_Struct (MD) and then Present (MD_Name (MD)) then
         Append (Result, "[");
         Append (Result, MD_Name (MD));
         Append (Result, "] ");
      end if;

      return +Result;
   end To_String;

   ------------------
   -- Dump_MD_Type --
   ------------------

   procedure Dump_MD_Type (MD : MD_Type) is
   begin
      Push_Output;
      Set_Standard_Error;

      if No (MD) then
         Write_Line ("None");
      else
         Write_Line (To_String (MD, Top => True));
      end if;

      Pop_Output;
   end Dump_MD_Type;

begin
   --  Make a dummy entry in the table, so the "No" entry is never used.

   MD_Types.Increment_Last;
end GNATLLVM.MDType;
