------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

with Ada.Unchecked_Conversion;

with Get_Targ; use Get_Targ;

with GNATLLVM.Arrays;      use GNATLLVM.Arrays;
with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.Types;       use GNATLLVM.Types;
with GNATLLVM.Utils;       use GNATLLVM.Utils;

package body GNATLLVM.GLValue is

   function Call_Internal
     (Func        : GL_Value;
      Args        : GL_Value_Array;
      Name        : String := "") return Value_T;
   --  Internal version of Call and Call_Ref

   function GL_Value_Is_Valid_Int (V : GL_Value_Base) return Boolean;
   --  Internal version of GL_Value_Is_Valid

   -----------------------
   -- GL_Value_Is_Valid --
   -----------------------

   function GL_Value_Is_Valid (V : GL_Value_Base) return Boolean is
      Valid : constant Boolean := GL_Value_Is_Valid_Int (V);
   begin
      --  This function exists so a conditional breakpoint can be set at
      --  the following line to see the invalid value.  Otherwise, there
      --  seems no other reasonable way to get to see it.

      return Valid;
   end GL_Value_Is_Valid;

   ----------------------------
   --  GL_Value_Is_Valid_Int --
   ----------------------------

   function GL_Value_Is_Valid_Int (V : GL_Value_Base) return Boolean is
   begin
      --  We have to be very careful in this function not to call any
      --  functions that take a GL_Value as an operand to avoid infinite
      --  recursion.  So we can't call "No" below, for example.

      if V = No_GL_Value then
         return True;
      elsif No (V.Value) or else No (V.Typ) then
         return False;
      end if;

      case V.Relationship is
         when Data =>
            return Is_Type (V.Typ) and then not Is_Dynamic_Size (V.Typ)
              and then Ekind (V.Typ) /= E_Subprogram_Type;

         when Reference =>
            return Is_Type (V.Typ)
              and then (Get_Type_Kind (Type_Of (V.Value)) = Pointer_Type_Kind
                          or else Is_Unconstrained_Array (V.Typ)
            --  ??? Keep the above test until we see if we can use Fat_Pointer
            --  consistently for this.
                          or else (Ekind (V.Typ) = E_Subprogram_Type
                                     and then Needs_Activation_Record
                                     (V.Typ)));

         when Double_Reference =>
            return Is_Type (V.Typ)
              and then Get_Type_Kind (Type_Of (V.Value)) = Pointer_Type_Kind;

         when Fat_Pointer | Bounds | Bounds_And_Data =>
            return Is_Array_Type (V.Typ);

         when Array_Data | Thin_Pointer
            | Reference_To_Bounds | Reference_To_Bounds_And_Data =>
            return Is_Type (V.Typ)
              and then Get_Type_Kind (Type_Of (V.Value)) = Pointer_Type_Kind
              and then Is_Array_Type (V.Typ);

         when Activation_Record  | Fat_Reference_To_Subprogram =>
            return Ekind (V.Typ) = E_Subprogram_Type;

         when Reference_To_Activation_Record =>
            return Ekind (V.Typ) = E_Subprogram_Type
              and then Get_Type_Kind (Type_Of (V.Value)) = Pointer_Type_Kind;

         when Reference_To_Subprogram =>
            return Get_Type_Kind (Type_Of (V.Value)) = Pointer_Type_Kind;

         when Any_Reference | Object | Invalid =>
            return False;
      end case;
   end GL_Value_Is_Valid_Int;

   -----------------------------
   -- Is_Access_Unconstrained --
   -----------------------------

   function Is_Access_Unconstrained (V : GL_Value) return Boolean is
     (Is_Access_Type (V) and then Ekind (V.Typ) /= E_Void
        and then not Is_Subprogram_Reference (V)
        and then Is_Unconstrained_Array (Full_Designated_Type (V))
        and then not Is_Raw_Array (V)
        and then Relationship (V) /= Reference_To_Subprogram);

   ---------------------
   -- Is_Dynamic_Size --
   ---------------------

   function Is_Dynamic_Size (V : GL_Value) return Boolean is
     (not Is_Reference (V) and then Is_Dynamic_Size (Full_Etype (V)));

   -------------
   -- Discard --
   -------------

   procedure Discard (V : GL_Value) is
      pragma Unreferenced (V);
   begin
      null;
   end Discard;

   ----------------------------------
   -- Relationship_For_Access_Type --
   ----------------------------------

   function Relationship_For_Access_Type
     (TE : Entity_Id) return GL_Value_Relationship
   is
      DT : constant Entity_Id := Full_Designated_Type (TE);

   begin
      --  If this points to an unconstrained array, this is either a
      --  fat or thin pointer, depending on the size.

      if Is_Access_Unconstrained (TE) then
         return (if RM_Size (TE) = Get_Pointer_Size
                 then Thin_Pointer else Fat_Pointer);

      --  If this is an access type that points to a type created as a
      --  a nominal subtype of an unconstrained type for an aliased
      --  object, in order to point to the data, we need a thin pointer.

      elsif Is_Constr_Subt_For_UN_Aliased (DT) then
         return Thin_Pointer;

      --  If this is an access to subprogram, this is either a pointer to
      --  the subprogram or a pair of pointers that includes the activation
      --  record.

      elsif Ekind (DT) = E_Subprogram_Type then
         return (if Needs_Activation_Record (DT)
                 then Fat_Reference_To_Subprogram else Reference);

      --  Otherwise, it's a normal pointer and it's just a Reference

      else
         return Reference;
      end if;
   end Relationship_For_Access_Type;

   ------------------------
   -- Equiv_Relationship --
   ------------------------

   function Equiv_Relationship
     (V : GL_Value; Rel : GL_Value_Relationship) return Boolean
   is
      TE     : constant Entity_Id    := Related_Type (V);
      R      : GL_Value_Relationship := Rel;

   begin
      if R = Object then
         R := (if Is_Dynamic_Size (TE) then Any_Reference else Data);
      end if;

      return Relationship (V) = R
        or else (R = Any_Reference and then Is_Any_Reference (V));
   end Equiv_Relationship;

   ---------
   -- Get --
   ---------

   function Get (V : GL_Value; Rel : GL_Value_Relationship) return GL_Value is
      Value  : constant Value_T      := LLVM_Value (V);
      TE     : constant Entity_Id    := Related_Type (V);
      R      : GL_Value_Relationship := Rel;
      Result : GL_Value;
      T      : Type_T;

   begin
      --  Handle relationship of Object by converting it to the appropriate
      --  relationship for TE.

      if R = Object then
         R := (if Is_Dynamic_Size (TE) then Any_Reference else Data);
      end if;

      --  If it's already the desired relationship, done

      if Equiv_Relationship (V, R) then
         return V;

      --  If we just need a dereference, do that

      elsif Equiv_Relationship (Deref (Relationship (V)), R) then
         return Load (V);

      --  Likewise for a double dereference

      elsif Deref (Deref (Relationship (V))) = R then
         return Load (Load (V));

      --  If we just need to make this into a reference, we can store
      --  it into memory since we only have those relationships if
      --  this is a actual LLVM value.

      elsif Equiv_Relationship (Ref (Relationship (V)), R) then
         Result := G (Alloca (IR_Builder, Type_Of (V), ""),
                      Related_Type (V), Ref (Relationship (V)));
         Store (V, Result);
         return Result;
      end if;

      --  Now we have specific rules for each relationship type

      case R is
         when Bounds =>

            --  If we have something that we can use to get the address of
            --  bounds, convert to that and then dereference.

            if Relationship (V) = Fat_Pointer
              or else Relationship (V) = Thin_Pointer
              or else Relationship (V) = Reference_To_Bounds_And_Data
            then
               return Load (Get (V, Reference_To_Bounds));

            --  If we have both bounds and data, extract the bounds

            elsif Relationship (V) = Bounds_And_Data then
               return G (Extract_Value (IR_Builder, Value, 0, ""), TE, R);

            --  Otherwise, compute the bounds from the type (pass in V
            --  just in case, though we should have handled all the cases
            --  where it's useful above).

            else
               return Get_Array_Bounds (TE, V);
            end if;

         when Bounds_And_Data =>

            --  If we have data, we can add the bounds

            if Relationship (V) = Data then
               T := Build_Struct_Type ((1 => Create_Array_Bounds_Type (TE),
                                        2 => Type_Of (V)));
               Result := G (Get_Undef (T), TE, R);
               return Insert_Value (Insert_Value (Result,
                                                  Get_Array_Bounds (TE, V), 0),
                                    V, 1);
            end if;

         when Reference_To_Bounds =>
            T := Pointer_Type (Create_Array_Bounds_Type (TE), 0);

            --  If we have a fat pointer, part of it is a pointer to the
            --  bounds.

            if Relationship (V) = Fat_Pointer then
               return G (Extract_Value (IR_Builder, Value, 1, ""), TE, R);

            --  A reference to bounds and data is a reference to bounds

            elsif Relationship (V) = Reference_To_Bounds_And_Data then
               return G (Pointer_Cast (IR_Builder, Value, T, ""), TE, R);

            --  The bounds are in front of the data for a thin pointer

            elsif Relationship (V) = Thin_Pointer then
               Result := NSW_Sub (Ptr_To_Int (V, Size_Type),
                                  Get_Bound_Part_Size (TE));
               return G (Int_To_Ptr (IR_Builder, LLVM_Value (Result), T, ""),
                         TE, R);

            --  Otherwise get the bounds and force them into memory

            else
               return Get (Get (V, Bounds), R);
            end if;

         when Array_Data =>

            --  For Reference and Thin_Pointer, we have the value we need,
            --  possibly just converting it.  For FAT pointer, we can
            --  extract it.

            if Relationship (V) = Reference then
               return G (Value, TE, R);
            elsif Relationship (V) = Thin_Pointer then
               return G (Pointer_Cast
                           (IR_Builder, Value, Create_Type (TE), ""), TE, R);
            elsif Relationship (V) = Fat_Pointer then
               return G (Extract_Value (IR_Builder, Value, 0, ""), TE, R);

            --  If we have a reference to both bounds and data, we can
            --  compute where the data starts.  If we have the actual
            --  bounds and data, we can store them and proceed as above.

            elsif Relationship (V) = Reference_To_Bounds_And_Data then
               Result := NSW_Add (Ptr_To_Int (V, Size_Type),
                                  Get_Bound_Part_Size (TE));
               return G (Int_To_Ptr (IR_Builder, LLVM_Value (Result),
                                     Create_Type (TE), ""),
                         TE, R);
            elsif Relationship (V) = Bounds_And_Data then
               return Get (Get (V, Reference_To_Bounds_And_Data), R);
            end if;

         when Reference =>

            --  If we have Array_Data, we have the value we need.  Otherwise,
            --  try to convert to Array_Data and then to this.

            if Relationship (V) = Array_Data then
               return G (Value, TE, R);
            else
               return Get (Get (V, Array_Data), R);
            end if;

         when Thin_Pointer =>

            --  There are only two cases where we can make a thin pointer.
            --  One is where we have the address of bounds and data (or the
            --  bounds and data themselves).  The other is if we have a fat
            --  pointer.  In the latter case, we can't know directly that
            --  the address in the fat pointer is actually suitable, but
            --  Ada language rules guarantee that it will be.

            if Relationship (V) = Reference_To_Bounds_And_Data then
               Result := NSW_Add (Ptr_To_Int (V, Size_Type),
                                  Get_Bound_Part_Size (TE));
               return G (Int_To_Ptr (IR_Builder, LLVM_Value (Result),
                                     Create_Type (TE), ""),
                         TE, R);
            elsif Relationship (V) = Bounds_And_Data then
               return Get (Get (V, Reference_To_Bounds_And_Data), R);
            elsif Relationship (V) = Fat_Pointer then
               return G (Extract_Value (IR_Builder, Value, 0, ""), TE, R);
            end if;

         when Fat_Pointer =>

            --  To make a fat pointer, we make the address of the bounds
            --  and the address of the data and put them together.

            declare
               Val     : constant GL_Value :=
                 (if Is_Reference (V) then V
                  else Get (V, Ref (Relationship (V))));
               --  If we have something that isn't a reference, start by
               --  getting a reference to it.

               Bounds  : constant GL_Value  := Get (Val, Reference_To_Bounds);
               Data    : constant GL_Value  := Get (Val, Array_Data);
               Fat_Ptr : constant GL_Value  :=
                 G (Get_Undef (Create_Array_Fat_Pointer_Type (TE)),
                    TE, Fat_Pointer);

            begin
               return Insert_Value (Insert_Value (Fat_Ptr, Data, 0),
                                    Bounds,  1);
            end;

         when Reference_To_Activation_Record =>
            if Relationship (V) = Fat_Reference_To_Subprogram then
               return G (Extract_Value (IR_Builder, Value, 1, ""), TE, Rel);
            end if;

         when Reference_To_Subprogram =>
            if Relationship (V) = Fat_Reference_To_Subprogram then
               return G (Extract_Value (IR_Builder, Value, 0, ""), TE, Rel);
            end if;

         when Any_Reference =>

            --  The old case where we have some GL_Value that's not already
            --  handled by one of the cases above the "case" statement is if
            --  it's a Reference_To_Bounds_And_Data, in which case the most
            --  general thing to convert it to is a thin pointer.

            return Get (V, Thin_Pointer);

         when others =>
            null;

      end case;

      --  If we reach here, this is case we can't handle.  Return null, which
      --  will cause our postcondition to fail.
      return No_GL_Value;
   end Get;

   ------------
   -- Alloca --
   ------------

   function Alloca (TE : Entity_Id; Name : String := "") return GL_Value is
      Inst : constant Value_T := Alloca (IR_Builder, Create_Type (TE), Name);
   begin
      Set_Alloca_Align (Inst, Get_Type_Alignment (TE));
      return G_Ref (Inst, TE);
   end Alloca;

   ------------------
   -- Array_Alloca --
   ------------------

   function Array_Alloca
     (TE       : Entity_Id;
      Num_Elts : GL_Value;
      Name     : String := "") return GL_Value
   is
      Inst : constant Value_T :=
        Array_Alloca (IR_Builder, Create_Type (TE),
                      LLVM_Value (Num_Elts), Name);
   begin
      Set_Alloca_Align (Inst, Get_Type_Alignment (TE));
      return G_Ref (Inst, TE);
   end Array_Alloca;

   ---------------
   -- Get_Undef --
   ---------------

   function Get_Undef (TE : Entity_Id) return GL_Value is
     (G (Get_Undef (Create_Type (TE)), TE));

   -------------------
   -- Get_Undef_Ref --
   -------------------

   function Get_Undef_Ref (TE : Entity_Id) return GL_Value is
     (G_Ref (Get_Undef (Create_Access_Type (TE)), TE));

   ----------------
   -- Const_Null --
   ----------------

   function Const_Null (TE : Entity_Id) return GL_Value is
     (G (Const_Null (Create_Type (TE)), TE));

   --------------------
   -- Const_Null_Ref --
   --------------------

   function Const_Null_Ref (TE : Entity_Id) return GL_Value is
     (G_Ref (Const_Null (Create_Access_Type (TE)), TE));

   ---------------
   -- Const_Int --
   ---------------

   function Const_Int (TE : Entity_Id; N : Uint) return GL_Value
   is
     (G (Const_Int (Create_Type (TE), N), TE));

   ---------------
   -- Const_Int --
   ---------------

   function Const_Int
     (TE : Entity_Id; N : ULL; Sign_Extend : Boolean := False) return GL_Value
   is
     (G (Const_Int (Create_Type (TE), N, Sign_Extend => Sign_Extend), TE));

   ----------------
   -- Const_Real --
   ----------------

   function Const_Real (TE : Entity_Id; V : double) return GL_Value
   is
     (G (Const_Real (Create_Type (TE), V), TE));

   ----------------
   -- Int_To_Ptr --
   ----------------

   function Int_To_Ptr
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
   is
     (G (Int_To_Ptr (IR_Builder, LLVM_Value (V), Create_Type (TE), Name), TE));

   ----------------
   -- Int_To_Ref --
   ----------------

   function Int_To_Ref
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
   is
      (G_Ref (Int_To_Ptr (IR_Builder, LLVM_Value (V),
                          Pointer_Type (Create_Type (TE), 0),
                          Name),
              TE));

   ----------------------
   -- Int_To_Raw_Array --
   ----------------------

   function Int_To_Raw_Array
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
   is
      (G (Int_To_Ptr (IR_Builder, LLVM_Value (V),
                      Create_Array_Raw_Pointer_Type (TE), Name),
          TE, Array_Data));

   ----------------
   -- Ptr_To_Int --
   ----------------

   function Ptr_To_Int
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
   is
     (G (Ptr_To_Int (IR_Builder, LLVM_Value (V), Create_Type (TE), Name), TE));

   --------------
   -- Bit_Cast --
   --------------

   function Bit_Cast
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
   is
     (G (Bit_Cast (IR_Builder, LLVM_Value (V), Create_Type (TE), Name), TE));

   ------------------
   -- Pointer_Cast --
   ------------------

   function Pointer_Cast
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
   is
     (G (Pointer_Cast (IR_Builder, LLVM_Value (V), Create_Type (TE), Name),
         TE));

   ----------------
   -- Ptr_To_Ref --
   ----------------

   function Ptr_To_Ref
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
   is
      (G_Ref (Pointer_Cast (IR_Builder, LLVM_Value (V),
                            Pointer_Type (Create_Type (TE), 0), Name),
              TE));

   ----------------------
   -- Ptr_To_Raw_Array --
   ----------------------

   function Ptr_To_Raw_Array
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
   is
      (G (Pointer_Cast (IR_Builder, LLVM_Value (V),
                        Create_Array_Raw_Pointer_Type (TE), Name),
          TE, Array_Data));

   -----------
   -- Trunc --
   -----------

   function Trunc
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
   is
     (G (Trunc (IR_Builder, LLVM_Value (V), Create_Type (TE), Name), TE));

   -----------
   -- S_Ext --
   -----------

   function S_Ext
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
   is
     (G (S_Ext (IR_Builder, LLVM_Value (V), Create_Type (TE), Name), TE));

   -----------
   -- Z_Ext --
   -----------

   function Z_Ext
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
   is
     (G (Z_Ext (IR_Builder, LLVM_Value (V), Create_Type (TE), Name), TE));

   --------------
   -- FP_Trunc --
   --------------

   function FP_Trunc
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
   is
     (G (FP_Trunc (IR_Builder, LLVM_Value (V), Create_Type (TE), Name), TE));

   ------------
   -- FP_Ext --
   ------------

   function FP_Ext
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
   is
     (G (FP_Ext (IR_Builder, LLVM_Value (V), Create_Type (TE), Name), TE));

   --------------
   -- FP_To_SI --
   --------------

   function FP_To_SI
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
   is
     (G (FP_To_SI (IR_Builder, LLVM_Value (V), Create_Type (TE), Name), TE));

   --------------
   -- FP_To_UI --
   --------------

   function FP_To_UI
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
   is
     (G (FP_To_UI (IR_Builder, LLVM_Value (V), Create_Type (TE), Name), TE));

   --------------
   -- UI_To_FP --
   --------------

   function UI_To_FP
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
   is
     (G (UI_To_FP (IR_Builder, LLVM_Value (V), Create_Type (TE), Name), TE));

   --------------
   -- SI_To_FP --
   --------------

   function SI_To_FP
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
   is
     (G (SI_To_FP (IR_Builder, LLVM_Value (V), Create_Type (TE), Name), TE));

   -------------------
   -- Build_Cond_Br --
   -------------------

   procedure Build_Cond_Br (C_If : GL_Value; C_Then, C_Else : Basic_Block_T)
   is
   begin
      Discard (Build_Cond_Br (IR_Builder, LLVM_Value (C_If), C_Then, C_Else));
   end Build_Cond_Br;

   ---------------
   -- Build_Ret --
   ---------------

   procedure Build_Ret (V : GL_Value) is
   begin
      Discard (Build_Ret (IR_Builder, LLVM_Value (V)));
   end Build_Ret;

   --------------------
   -- Build_Ret_Void --
   --------------------

   procedure Build_Ret_Void is
   begin
      Discard (Build_Ret_Void (IR_Builder));
   end Build_Ret_Void;

   -----------------------
   -- Build_Unreachable --
   -----------------------

   procedure Build_Unreachable is
   begin
      Discard (Build_Unreachable (IR_Builder));
   end Build_Unreachable;

   ---------------
   -- Build_Phi --
   ---------------

   function Build_Phi
     (GL_Values : GL_Value_Array;
      BBs       : Basic_Block_Array;
      Name      : String := "") return GL_Value
   is
      Values  : Value_Array (GL_Values'Range);
      Our_Phi : Value_T;

   begin
      for J in Values'Range loop
         Values (J) := LLVM_Value (GL_Values (J));
      end loop;

      Our_Phi := Phi (IR_Builder, Type_Of (GL_Values (GL_Values'First)), Name);
      Add_Incoming (Our_Phi, Values'Address, BBs'Address, Values'Length);
      return G_From (Our_Phi, GL_Values (GL_Values'First));
   end Build_Phi;

   --------------------------
   -- Full_Designated_Type --
   --------------------------

   function Full_Designated_Type (V : GL_Value) return Entity_Id is
     ((if Is_Reference (V) then Get_Fullest_View (V.Typ)
       else Full_Designated_Type (Etype (V))));

   ---------
   -- GEP --
   ---------

   function GEP
     (Result_Type : Entity_Id;
      Ptr         : GL_Value;
      Indices     : GL_Value_Array;
      Name        : String := "") return GL_Value
   is
      Val_Idxs    : Value_Array (Indices'Range);
      Result      : Value_T;

   begin
      for J in Indices'Range loop
         Val_Idxs (J) := LLVM_Value (Indices (J));
      end loop;

      Result := In_Bounds_GEP (IR_Builder, LLVM_Value (Ptr), Val_Idxs'Address,
                               Val_Idxs'Length, Name);
      return G_Ref (Result, Result_Type);
   end GEP;

   ----------
   -- Load --
   ----------

   function Load (Ptr : GL_Value; Name : String := "") return GL_Value is
      New_Relationship : constant GL_Value_Relationship :=
        Relation_Props (Relationship (Ptr)).Deref;

   begin
      --  If this is going to actually be pointing to data of the related
      --  type, indicate that we're loading an object of that type.

      if New_Relationship = Data then
         return G (Load_With_Type (Full_Designated_Type (Ptr),
                                   LLVM_Value (Ptr), Name),
                   Full_Designated_Type (Ptr));
      else
         --  Otherwise, do a load with no type indication.
         --  ??? At some point, we need to deal with TBAA or similar for these.

         return G (Load (IR_Builder, LLVM_Value (Ptr), Name),
                   Related_Type (Ptr), New_Relationship);
      end if;
   end Load;

   -----------
   -- Store --
   -----------

   procedure Store (Expr : GL_Value; Ptr : GL_Value) is
   begin
      if Has_Known_Etype (Expr) then
         Store_With_Type (Etype (Expr), LLVM_Value (Expr), LLVM_Value (Ptr));
      else
         Discard (Build_Store (IR_Builder, LLVM_Value (Expr),
                               LLVM_Value (Ptr)));
      end if;
   end Store;

   -------------------
   -- Call_Internal --
   ------------------

   function Call_Internal
     (Func        : GL_Value;
      Args        : GL_Value_Array;
      Name        : String := "") return Value_T
   is
      Arg_Values  : Value_Array (Args'Range);

   begin
      for J in Args'Range loop
         Arg_Values (J) := LLVM_Value (Args (J));
      end loop;

      return Call (IR_Builder, LLVM_Value (Func),
                   Arg_Values'Address, Arg_Values'Length, Name);
   end Call_Internal;

   ----------
   -- Call --
   ----------

   function Call
     (Func        : GL_Value;
      Result_Type : Entity_Id;
      Args        : GL_Value_Array;
      Name        : String := "") return GL_Value is
   begin
      return G (Call_Internal (Func, Args, Name), Result_Type);
   end Call;

   --------------
   -- Call_Ref --
   --------------

   function Call_Ref
     (Func        : GL_Value;
      Result_Type : Entity_Id;
      Args        : GL_Value_Array;
      Name        : String := "") return GL_Value is
   begin
      return G_Ref (Call_Internal (Func, Args, Name), Result_Type);
   end Call_Ref;

   ----------
   -- Call --
   ----------

   procedure Call
     (Func : GL_Value; Args : GL_Value_Array; Name : String := "") is
   begin
      Discard (Call_Internal (Func, Args, Name));
   end Call;

   ----------------
   -- Inline_Asm --
   ----------------

   function Inline_Asm
     (Args           : GL_Value_Array;
      Output_Value   : Entity_Id;
      Template       : String;
      Constraints    : String;
      Is_Volatile    : Boolean := False;
      Is_Stack_Align : Boolean := False) return GL_Value
   is
      TE        : constant Entity_Id :=
        (if Present (Output_Value) then Full_Etype (Output_Value)
         else Standard_Void_Type);
      T         : constant Type_T :=
        (if Present (Output_Value) then Create_Type (TE) else Void_Type);
      Arg_Types : Type_Array (Args'Range);

   begin
      for J in Args'Range loop
         Arg_Types (J) := Type_Of (Args (J));
      end loop;

      return G (Const_Inline_Asm (Fn_Ty (Arg_Types, T), Template,
                                  Constraints, Is_Volatile, Is_Stack_Align),
                TE, Reference_To_Subprogram);
   end Inline_Asm;

   -------------------
   -- Get_Type_Size --
   -------------------

   function Get_Type_Size (V : GL_Value) return GL_Value is
      (Get_Type_Size (Full_Etype (V), V));

   ------------------------
   -- Get_Type_Alignment --
   ------------------------

   function Get_Type_Alignment (V : GL_Value) return unsigned is
      (Get_Type_Alignment (Full_Etype (V)));

   ----------------
   -- Add_Global --
   ----------------

   function Add_Global
     (TE             : Entity_Id;
      Name           : String;
      Need_Reference : Boolean := False) return GL_Value is
     (G (Add_Global (LLVM_Module,
                     (if Need_Reference then Create_Access_Type (TE)
                      else Create_Type (TE)),
                      Name),
         TE, (if Need_Reference then Double_Reference else Reference)));

   ---------------------
   -- Set_Initializer --
   ---------------------

   procedure Set_Initializer (V, Expr : GL_Value) is
   begin
      Set_Initializer (LLVM_Value (V), LLVM_Value (Expr));
   end Set_Initializer;

   -----------------
   -- Set_Linkage --
   -----------------

   procedure Set_Linkage (V : GL_Value; Linkage : Linkage_T) is
   begin
      Set_Linkage (LLVM_Value (V), Linkage);
   end Set_Linkage;

   ----------------------
   -- Set_Thread_Local --
   ----------------------

   procedure Set_Thread_Local (V : GL_Value; Thread_Local : Boolean) is
   begin
      Set_Thread_Local (LLVM_Value (V), Thread_Local);
   end Set_Thread_Local;

end GNATLLVM.GLValue;
