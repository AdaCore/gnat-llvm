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
      --  types. The related type is the type of the next field or parameter.

      Void,
      --  A null type, which is used for a function that doesn't return or
      --  a generic pointer.

      Integer,
      --  An integer. The count field is the number of bits and the flag
      --  indicates signedness.

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

      Count          : Nat;
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

      Have_Fields    : Boolean := False;
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

   function Kind (MDT : MD_Type) return MD_Kind is
     (MD_Types.Table (MDT).Kind)
     with Pre => Present (MDT), Post => Kind'Result /= Continuation;

   function Related (MDT : MD_Type) return MD_Type is
     (MD_Types.Table (MDT).Related_Type)
     with Pre => Present (MDT), Post => Present (Related'Result);

   function Continuation_Type (MDT : MD_Type) return MD_Type is
     (MD_Types.Table (MDT).Cont_Type)
     with Pre => Present (MDT);

   function LLVM_Type (MDT : MD_Type) return Type_T is
     (MD_Types.Table (MDT).LLVM_Type)
     with Pre => Present (MDT);

   function MD_Count (MDT : MD_Type) return Nat is
     (MD_Types.Table (MDT).Count)
     with Pre => Present (MDT);

   function MD_Entity (MDT : MD_Type) return Opt_Record_Field_Kind_Id is
     (MD_Types.Table (MDT).Entity)
     with Pre => Present (MDT);

   function Flag (MDT : MD_Type) return Boolean is
     (MD_Types.Table (MDT).Flag)
     with Pre => Present (MDT);
   function Not_Flag (MDT : MD_Type) return Boolean is (not Flag (MDT));

   procedure Struct_Set_Body_Internal (MDT : MD_Type)
     with Pre => Have_Fields (MDT);
   --  Set up the LLVM type corresponding to MDT with the field information
   --  from MDT. This may either create or update the type.

   procedure Set_LLVM_Type (MDT : MD_Type; T : Type_T)
     with Pre =>  Present (MDT) and then Present (T)
                  and then No (LLVM_Type (MDT)),
          Post => LLVM_Type (MDT) = T, Inline;
   --  Set the LLVM_Type of MDT to T

   ----------
   -- Hash --
   ----------

   function Hash (Info : MD_Type_Info) return Hash_Index_Type is
     ((MD_Hash_Type (MD_Kind'Pos (Info.Kind)) +
       MD_Hash_Type ((Info.Name - Names_Low_Bound) * 11) +
       MD_Hash_Type (Info.Count * 3) +
       MD_Hash_Type ((Info.Related_Type - MD_Type'First) * 5) +
       MD_Hash_Type ((Info.Cont_Type - MD_Type'First) * 7) +
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
      and then Info1.Is_Volatile = Info2.Is_Volatile
      and then Info1.Flag = Info2.Flag);
   --  Note that we don't want to compare Hash_Link and LLVM_Type because
   --  we want to know that the types are structually the same and those
   --  fields don't relate to the structure.

   -------------------
   -- Set_LLVM_Type --
   -------------------

   procedure Set_LLVM_Type (MDT : MD_Type; T : Type_T) is
   begin
      MD_Types.Table (MDT).LLVM_Type := T;
   end Set_LLVM_Type;

   -----------------
   -- Have_Fields --
   -----------------

   function Have_Fields (MDT : MD_Type) return Boolean is
     (MD_Types.Table (MDT).Have_Fields);

   ------------------
   -- Is_Same_Kind --
   ------------------

   function Is_Same_Kind (MDT1, MDT2 : MD_Type) return Boolean is
      (Kind (MDT1) = Kind (MDT2));

   -------------
   -- Is_Void --
   -------------

   function Is_Void (MDT : MD_Type) return Boolean is (Kind (MDT) = Void);

   ----------------
   -- Is_Integer --
   ----------------

   function Is_Integer (MDT : MD_Type) return Boolean is
     (Kind (MDT) = Integer);

   ----------------
   -- Is_Float --
   ----------------

   function Is_Float (MDT : MD_Type) return Boolean is (Kind (MDT) = Float);

   ---------------
   -- Is_Signed --
   ---------------

   function Is_Signed (MDT : MD_Type) return Boolean renames Not_Flag;

   -----------------
   -- Is_Unsigned --
   -----------------

   function Is_Unsigned (MDT : MD_Type) return Boolean renames Flag;

   --------------
   -- Is_Array --
   --------------

   function Is_Array (MDT : MD_Type) return Boolean is
     (Kind (MDT) = Array_Type);

   --------------------
   -- Is_Fixed_Array --
   --------------------

   function Is_Fixed_Array (MDT : MD_Type) return Boolean renames Not_Flag;

   -----------------------
   -- Is_Variable_Array --
   -----------------------

   function Is_Variable_Array (MDT : MD_Type) return Boolean renames Flag;

   -----------------------
   -- Is_Variable_Array --
   -----------------------

   function Is_Varargs_Function (MDT : MD_Type) return Boolean renames Flag;

   --------------
   -- Is_Struct --
   --------------

   function Is_Struct (MDT : MD_Type) return Boolean is (Kind (MDT) = Struct);

   ---------------
   -- Is_Packed --
   ---------------

   function Is_Packed (MDT : MD_Type) return Boolean renames Flag;

   ----------------
   -- Is_Pointer --
   ----------------

   function Is_Pointer (MDT : MD_Type) return Boolean is
     (Kind (MDT) = Pointer);

   ----------------------
   -- Is_Function_Type --
   ----------------------

   function Is_Function_Type (MDT : MD_Type) return Boolean is
     (Kind (MDT) = Func);

   --------------
   -- Int_Bits --
   --------------

   function Int_Bits (MDT : MD_Type) return Nat renames MD_Count;

   ----------------
   -- Float_Bits --
   ----------------

   function Float_Bits (MDT : MD_Type) return Nat renames MD_Count;

   -----------------
   -- Array_Count --
   -----------------

   function Array_Count (MDT : MD_Type) return Nat renames MD_Count;

   -------------------
   -- Element_Count --
   -------------------

   function Element_Count (MDT : MD_Type) return Nat renames MD_Count;

   -------------------
   -- Pointer_Space --
   -------------------

   function Pointer_Space (MDT : MD_Type) return Nat renames MD_Count;

   ---------------------
   -- Parameter_Count --
   ---------------------

   function Parameter_Count (MDT : MD_Type) return Nat renames MD_Count;

   ---------------------
   -- Designated_Type --
   ---------------------

   function Designated_Type (MDT : MD_Type) return MD_Type renames Related;

   ------------------
   -- Element_Type --
   ------------------

   function Element_Type (MDT : MD_Type) return MD_Type renames Related;

   -----------------
   -- Return_Type --
   -----------------

   function Return_Type (MDT : MD_Type) return MD_Type renames Related;

   -----------------
   -- Is_Volatile --
   -----------------

   function Is_Volatile (MDT : MD_Type) return Boolean is
      (MD_Types.Table (MDT).Is_Volatile);

   ----------
   -- Name --
   ----------

   function MD_Name (MDT : MD_Type) return Name_Id is
     (MD_Types.Table (MDT).Name);

   ------------------
   -- Element_Name --
   ------------------

   function Element_Name (MDT : MD_Type; Idx : Nat) return Name_Id is
      E_MDT : MD_Type := Continuation_Type (MDT);

   begin
      for J in 0 .. Idx - 1 loop
         E_MDT := Continuation_Type (E_MDT);
      end loop;

      return MD_Name (E_MDT);
   end Element_Name;

   ------------------
   -- Element_Type --
   ------------------

   function Element_Type (MDT : MD_Type; Idx : Nat) return MD_Type is
      E_MDT : MD_Type := Continuation_Type (MDT);

   begin
      for J in 0 .. Idx - 1 loop
         E_MDT := Continuation_Type (E_MDT);
      end loop;

      return Related (E_MDT);
   end Element_Type;

   --------------------
   -- Parameter_Type --
   --------------------

   function Parameter_Type (MDT : MD_Type; Idx : Nat) return MD_Type is
      E_MDT : MD_Type := Continuation_Type (MDT);

   begin
      for J in 0 .. Idx - 1 loop
         E_MDT := Continuation_Type (E_MDT);
      end loop;

      return Related (E_MDT);
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
   begin
      return MD_Find ((Kind => Void, others => <>));
   end Void_Ty;

   ------------
   -- Int_Ty --
   ------------

   function Int_Ty
     (Bits : Nat; Unsigned : Boolean := False) return MD_Type is
   begin
      return MD_Find
        ((Kind => Integer, Count => Bits, Flag => Unsigned, others => <>));
   end Int_Ty;

   ------------
   -- Float_Ty --
   ------------

   function Float_Ty (Bits : Nat) return MD_Type is
   begin
      return MD_Find ((Kind => Float, Count => Bits, others => <>));
   end Float_Ty;

   ------------------
   -- Pointer_Type --
   ------------------

   function Pointer_Type
     (Elem_Type : MD_Type;
      Space     : Nat := Address_Space) return MD_Type
   is
   begin
      return MD_Find ((Kind         => Pointer,
                       Related_Type => Elem_Type,
                       Count        => Space,
                       others       => <>));
   end Pointer_Type;

   ----------------
   -- Array_Type --
   ----------------

   function Array_Type (Elem_Type : MD_Type; Count : Nat) return MD_Type is
   begin
      return MD_Find ((Kind         => Array_Type,
                       Count        => Count,
                       Related_Type => Elem_Type,
                       others       => <>));
   end Array_Type;

   -----------------------
   -- Build_Struct_Type --
   -----------------------

   function Build_Struct_Type
     (Types       : MD_Type_Array;
      Field_Names : Name_Id_Array;
      Fields      : Field_Id_Array := (1 .. 0 => Empty);
      Packed      : Boolean := False;
      Name        : Name_Id := No_Name) return MD_Type
   is
      Info : MD_Type_Info :=
        (Kind        => Struct,
         Count       => Field_Names'Length,
         Have_Fields => True,
         Flag        => Packed,
         Name        => Name,
         others => <>);
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
                           others       => <>));
      end loop;

      Info.Cont_Type := Prev;
      return MD_Find (Info);
   end Build_Struct_Type;

   ---------------------
   -- Struct_Set_Body --
   ---------------------

   procedure Struct_Set_Body
     (MDT    : MD_Type;
      Types  : MD_Type_Array;
      Names  : Name_Id_Array;
      Fields : Field_Id_Array := (1 .. 0 => Empty);
      Packed : Boolean := False)
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
                           Cont_Type    => Prev,
                           others       => <>));
      end loop;

      MD_Types.Table (MDT).Cont_Type   := Prev;
      MD_Types.Table (MDT).Have_Fields := True;
      MD_Types.Table (MDT).Flag        := Packed;
      MD_Types.Table (MDT).Count       := Names'Length;

   end Struct_Set_Body;

   -------------------------
   -- Struct_Create_Named --
   -------------------------

   function Struct_Create_Named (Name : Name_Id) return MD_Type is
   begin
      MD_Types.Append ((Kind           => Struct,
                        Name           => Name,
                        Count          => 0,
                        others         => <>));
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
      Varargs     : Boolean := False) return MD_Type
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
                           Cont_Type    => Prev,
                           others       => <>));
      end loop;

      Info.Cont_Type := Prev;
      return MD_Find (Info);
   end Fn_Ty;

   ---------------
   -- Name_Type --
   ---------------

   function Name_Type (MDT : MD_Type; New_Name : Name_Id) return MD_Type is
      Info : MD_Type_Info := MD_Types.Table (MDT);

   begin
      Info.Name := New_Name;
      return MD_Find (Info);
   end Name_Type;

   -------------------
   -- Make_Volatile --
   -------------------

   function Make_Volatile (MDT : MD_Type) return MD_Type is
      Info : MD_Type_Info := MD_Types.Table (MDT);

   begin
      Info.Is_Volatile := True;
      return MD_Find (Info);
   end Make_Volatile;

   ------------------------------
   -- Struct_Set_Body_Internal --
   ------------------------------

   procedure Struct_Set_Body_Internal (MDT : MD_Type) is
      UID   : constant Unique_Id :=
        (if Have_Fields (MDT) then New_Unique_Id else No_Unique_Id);
      C_MDT : MD_Type := Continuation_Type (MDT);
      Typs  : Type_Array (1 .. Element_Count (MDT));

   begin
      for J in Typs'Range loop
         Typs (J) := +Related (C_MDT);
         C_Set_Field_Info (UID, J - Typs'First, MD_Name (C_MDT),
                           Is_Padding => not Has_Name (C_MDT),
                           Entity     => MD_Entity (C_MDT));
         C_MDT := Continuation_Type (C_MDT);
      end loop;

      if No (MD_Name (MDT)) then
         Set_LLVM_Type (MDT, Struct_Type (Typs'Address, Typs'Length,
                                          Is_Packed (MDT)));
      else
         if No (LLVM_Type (MDT)) then
            Set_LLVM_Type (MDT,
                           Struct_Create_Named
                             (Get_Global_Context,
                              Get_Name_String (MD_Name (MDT))));
         end if;

         Struct_Set_Body (LLVM_Type (MDT), Typs'Address, Typs'Length,
                          Is_Packed (MDT));
      end if;

      C_Set_Struct (UID, LLVM_Type (MDT));
   end Struct_Set_Body_Internal;

   ------------------
   -- LLVM_Type_Of --
   ------------------

   function LLVM_Type_Of (MDT : MD_Type) return Type_T is
      Result : Type_T;

   begin
      --  If we already made an LLVM type, return it

      if Present (LLVM_Type (MDT)) then

         --  However, if that's an opaque type, but we've set our field list,
         --  update that type first.

         if Is_Struct (MDT) and then Have_Fields (MDT)
           and then Is_Opaque_Struct (LLVM_Type (MDT))
         then
            Struct_Set_Body_Internal (MDT);
         end if;

         return LLVM_Type (MDT);
      end if;

      case Kind (MDT) is
         when Void =>
            Result := Void_Type;

         when Integer =>
            Result := Int_Type (unsigned (Int_Bits (MDT)));

         when Float =>
            case Float_Bits (MDT) is
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
            Result := Pointer_Type ((if   Is_Void (Designated_Type (MDT))
                                     then Byte_T else +Designated_Type (MDT)),
                                    unsigned (Pointer_Space (MDT)));

         when Array_Type =>
            Result := Array_Type (+Element_Type (MDT),
                                  (if   Is_Fixed_Array (MDT)
                                   then unsigned (Array_Count (MDT)) else 0));

         when Struct =>
            if Have_Fields (MDT) then
               Struct_Set_Body_Internal (MDT);
               return LLVM_Type (MDT);
            else
               Result := Struct_Create_Named (Get_Global_Context,
                                              Get_Name_String (MD_Name (MDT)));
            end if;

         when Func =>
            declare
               C_MDT : MD_Type          := Continuation_Type (MDT);
               Ret_T : constant Type_T  :=
                 (if   Is_Void (Related (MDT)) then Void_Type
                  else +Related (MDT));
               Types : Type_Array (1 .. Parameter_Count (MDT));

            begin
               for J in Types'Range loop
                  Types (J) := +Related (C_MDT);
                  C_MDT     := Continuation_Type (C_MDT);
               end loop;

               Result := Fn_Ty (Types, Ret_T, Is_Varargs_Function (MDT));
            end;

         when others =>
            Result := Void_Ptr_T;
      end case;

      Set_LLVM_Type (MDT, Result);
      return Result;
   end LLVM_Type_Of;

   ---------------
   -- From_Type --
   ---------------

   function From_Type (T : Type_T) return MD_Type is
   begin
      case Get_Type_Kind (T) is
         when Integer_Type_Kind =>
            return Int_Ty (Nat (Get_Scalar_Bit_Size (T)));

         when Float_Type_Kind =>
            return Float_Ty (32);

         when Double_Type_Kind =>
            return Float_Ty (64);

         when X86_FP80_Type_Kind =>
            return Float_Ty (80);

         when Pointer_Type_Kind =>
            return Pointer_Type ((if   Pointer_Type_Is_Opaque (T)
                                  then Void_Ty
                                  else From_Type (Get_Element_Type (T))));

         when Array_Type_Kind =>
            return Array_Type (From_Type (Get_Element_Type (T)),
                               Nat (Get_Array_Length (T)));

         when Struct_Type_Kind =>
            declare
               Num_Elts : constant Nat := Nat (Count_Struct_Element_Types (T));
               Types    : Type_Array (1 .. Num_Elts);
               MDTs     : MD_Type_Array (1 .. Num_Elts);

            begin
               Get_Struct_Element_Types (T, Types'Address);

               for J in Types'Range loop
                  MDTs (J) := From_Type (Types (J));
               end loop;

               return Build_Struct_Type (MDTs, (MDTs'Range => No_Name));
            end;

         when Function_Type_Kind =>
            declare
               Num_Params : constant Nat := Nat (Count_Param_Types (T));
               Types      : Type_Array (1 .. Num_Params);
               MDTs       : MD_Type_Array (1 .. Num_Params);

            begin
               Get_Param_Types (T, Types'Address);

               for J in Types'Range loop
                  MDTs (J) := From_Type (Types (J));
               end loop;

               return Fn_Ty (MDTs, From_Type (Get_Return_Type (T)));
            end;

         when Void_Type_Kind =>
            return Void_Ty;

         when others =>
            pragma Assert (False);
            return Void_Ty;
      end case;
   end From_Type;

   -------------------
   -- Get_Type_Size --
   -------------------

   function Get_Type_Size (MDT : MD_Type) return ULL is
     (Get_Type_Size (Type_T'(+MDT)));

   -------------------------
   -- Get_Scalar_Bit_Size --
   -------------------------

   function Get_Scalar_Bit_Size (MDT : MD_Type) return ULL is
     (Get_Scalar_Bit_Size (Type_T'(+MDT)));

   ------------------------
   -- Get_Type_Alignment --
   ------------------------

   function Get_Type_Alignment (MDT : MD_Type) return Nat is
     (Get_Type_Alignment (Type_T'(+MDT)));

   ---------------
   -- To_String --
   ---------------

   function To_String (MDT : MD_Type; Top : Boolean := False) return String is
      C_MDT  : MD_Type;
      Result : Bounded_String;

   begin
      if No (MDT) then
         return "void ";
      elsif not Top and then Present (MD_Name (MDT)) then
         return Get_Name_String (MD_Name (MDT));
      end if;

      C_MDT := Continuation_Type (MDT);
      if Is_Volatile (MDT) then
         Append (Result, "volatile ");
      end if;

      case Kind (MDT) is
         when Void =>
            Append (Result, "void");

         when Integer =>

            if Is_Unsigned (MDT) then
               Append (Result, 'u');
            end if;

            Append (Result, "int_");
            Append (Result, Int_Bits (MDT));
            Append (Result, "t ");

         when Float =>

            Append (Result, "float_");
            Append (Result, Float_Bits (MDT));
            Append (Result, "t ");

         when Array_Type =>
            Append (Result, To_String (Element_Type (MDT)));
            Append (Result, "[");

            if Is_Fixed_Array (MDT) then
               Append (Result, Array_Count (MDT));
            end if;

            Append (Result, "]");

         when Struct =>

            if Is_Packed (MDT) then
               Append (Result, "packed ");
            end if;

            Append (Result, "struct ");

            if Has_Name (MDT) then
               Append (Result, MD_Name (MDT));
               Append (Result, " ");
            end if;

            Append (Result, "{");

            while Present (C_MDT) loop
               Append (Result, To_String (Related (C_MDT)));

               if Has_Name (C_MDT) then
                  Append (Result, " ");
                  Append (Result, MD_Name (C_MDT));
               end if;

               C_MDT := Continuation_Type (C_MDT);

               if Present (C_MDT) then
                  Append (Result, ", ");
               end if;
            end loop;

            Append (Result, "}");

         when Pointer =>
            Append (Result, To_String (Designated_Type (MDT)));
            Append (Result, "*");
            if Pointer_Space (MDT) /= 0 then
               Append (Result, "{");
               Append (Result, Pointer_Space (MDT));
               Append (Result, "}");
            end if;

         when Func =>
            Append (Result, To_String (Return_Type (MDT)));
            Append (Result, "() (");

            while Present (C_MDT) loop
               Append (Result, To_String (Related (C_MDT)));
               C_MDT := Continuation_Type (C_MDT);

               if Present (C_MDT) then
                  Append (Result, ", ");
               end if;
            end loop;

            if Is_Varargs_Function (MDT) then
               Append (Result, ", ..");
            end if;

            Append (Result, ")");

         when others =>
            raise Program_Error;
      end case;

      if not Is_Struct (MDT) and then Present (MD_Name (MDT)) then
         Append (Result, "[");
         Append (Result, MD_Name (MDT));
         Append (Result, "] ");
      end if;

      return +Result;
   end To_String;

   ------------------
   -- Dump_MD_Type --
   ------------------

   procedure Dump_MD_Type (MDT : MD_Type) is
   begin
      Write_Line (To_String (MDT, Top => True));
   end Dump_MD_Type;

begin
   --  Make a dummy entry in the table, so the "No" entry is never used.

   MD_Types.Increment_Last;
end GNATLLVM.MDType;
