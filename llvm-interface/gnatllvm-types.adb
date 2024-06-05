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

with Errout;   use Errout;
with Get_Targ; use Get_Targ;
with Nlists;   use Nlists;
with Output;   use Output;
with Restrict; use Restrict;
with Snames;   use Snames;
with Table;    use Table;

with GNATLLVM.Aliasing;     use GNATLLVM.Aliasing;
with GNATLLVM.Arrays;       use GNATLLVM.Arrays;
with GNATLLVM.Blocks;       use GNATLLVM.Blocks;
with GNATLLVM.Builtins;     use GNATLLVM.Builtins;
with GNATLLVM.Codegen;      use GNATLLVM.Codegen;
with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.Conversions;  use GNATLLVM.Conversions;
with GNATLLVM.Environment;  use GNATLLVM.Environment;
with GNATLLVM.Exprs;        use GNATLLVM.Exprs;
with GNATLLVM.GLType;       use GNATLLVM.GLType;
with GNATLLVM.Records;      use GNATLLVM.Records;
with GNATLLVM.Subprograms;  use GNATLLVM.Subprograms;
with GNATLLVM.Types.Create; use GNATLLVM.Types.Create;
with GNATLLVM.Utils;        use GNATLLVM.Utils;
with GNATLLVM.Variables;    use GNATLLVM.Variables;

with CCG; use CCG;

package body GNATLLVM.Types is

   --  We save pairs of GNAT types and LLVM Value_T for each level of
   --  processing of an Emit_LValue so we can find it if we have a
   --  self-referential item (a discriminated record).

   package LValue_Pairs is new Table.Table
     (Table_Component_Type => GL_Value,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "LValue_Pairs");
   --  Table of intermediate results for Emit_LValue

   LValue_Pair_First : Nat := 1;
   --  The current first entry in the above table. See the below table.

   --  In the process of computing an LValue, we may need to compute
   --  another expression, e.g., an index or a bound, which may, in turn,
   --  compute another LValue. So we need to have a stack to save and restore
   --  a starting pointer to the above table.

   package LValue_Stack is new Table.Table
     (Table_Component_Type => Nat,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 3,
      Table_Increment      => 2,
      Table_Name           => "LValue_Stack");

   Var_Idx_For_BA : Int := 1;
   --  Index of variable used for Dynamic_Val in back-annotation.

   function Get_Alloc_Size
     (GT          : GL_Type;
      Alloc_GT    : GL_Type;
      V           : GL_Value;
      Max_Size    : Boolean := False;
      For_Dealloc : Boolean := False) return GL_Value
     with Pre  => Present (GT) and then Present (Alloc_GT),
          Post => Present (Get_Alloc_Size'Result);
   --  Like Get_Type_Size, but used for the size to be allocated, so we
   --  include the size of the bounds in some array cases.

   function Get_Alloc_Alignment
     (GT       : GL_Type;
      Alloc_GT : GL_Type;
      E        : Entity_Id := Empty) return Nat
     with Pre  => Present (GT) and then Present (Alloc_GT);
   --  Like Get_Type_Size, but used for the alignment in an allocator, so we
   --  include the alignment of the bounds in some array cases. It also
   --  may take into account the alignment of E, if present.

   function Move_Into_Memory
     (Temp     : GL_Value;
      V        : GL_Value;
      Expr     : Opt_N_Subexpr_Id;
      GT       : GL_Type;
      Alloc_GT : GL_Type) return GL_Value
     with Pre  => Present (Temp) and then Present (GT)
                  and then Present (Alloc_GT),
          Post => Is_Pointer (Move_Into_Memory'Result);
   --  Temp is memory that was recently allocated. Move V, if Present, or
   --  the evaluation of Expr if Present and V isn't, into that allocated
   --  memory and return the allocated memory as a reference to type GT
   --  This is used by both type of memory allocators. Temp can be of any
   --  type, either an integer or pointer to anything. Alloc_GT is the type
   --  that was used to allocate the memory.

   function GT_To_Use (GT, Alloc_GT : GL_Type) return GL_Type is
     ((if   Is_Unconstrained_Type (Alloc_GT)
            and then not Is_Unconstrained_Type (GT)
            and then not Is_Tagged_Type (GT)
       then GT else Alloc_GT))
     with Pre  => Present (GT) and then Present (Alloc_GT),
          Post => GT_To_Use'Result in GT | Alloc_GT;
   --  When we have a GT for an object and a GT to use for allocating the
   --  object, return the one we're to use. We normally want to use
   --  Alloc_GT, but an exception is if it's constrained and GT isn't
   --  unless this is a tagged type.

   function GL_Value_To_Node_Ref_Or_Val (V : GL_Value) return Node_Ref_Or_Val
     with Pre  => Is_A_Constant_Int (V),
          Post => Present (GL_Value_To_Node_Ref_Or_Val'Result);
   --  Make a Node_Ref_Or_Val from V. Normally this is just the integer
   --  value of V, but if it's negative, we need to build a negation node.

   function Prepare_SM_Copy_Host (V : GL_Value) return GL_Value
     with Pre  => Present (V),
          Post => Get_Type_Kind (Prepare_SM_Copy_Host'Result) =
                    Integer_Type_Kind;
   function Prepare_SM_Copy_Target (V : GL_Value) return GL_Value
     with Pre  => Present (V),
          Post => Get_Type_Kind (Prepare_SM_Copy_Target'Result) =
                    Integer_Type_Kind;
   --  V is an operand that's being passed to a Storage_Model copy from / to
   --  procedure. Convert it to the appropriate integral type for the host
   --  or target part of that call, respectively.

   --  We put the function used to compute sizes into a generic so that we
   --  can instantiate it using various types of sizing. The most common
   --  case is an actual size computation, where we produce a GL_Value.
   --  But we may also instantiate this package to generate the structure
   --  needed for back-annotation.

   generic
      type Result is private;
      Empty_Result : Result;
      with function From_Const (V : GL_Value) return Result;
      with function Get_Record_Type_Size
        (TE         : Type_Kind_Id;
         V          : GL_Value;
         Max_Size   : Boolean := False;
         No_Padding : Boolean := False) return Result;
      with function Get_Unc_Array_Type_Size
        (TE         : Type_Kind_Id;
         V          : GL_Value;
         Max_Size   : Boolean := False) return Result;
      with function Get_Array_Type_Size
        (TE         : Type_Kind_Id;
         V          : GL_Value;
         Max_Size   : Boolean := False) return Result;
      with function  "-" (V1, V2 : Result) return Result;
   package Size is
      function Get_Type_Size
        (GT         : GL_Type;
         V          : GL_Value := No_GL_Value;
         Max_Size   : Boolean  := False;
         No_Padding : Boolean  := False) return Result;
   end Size;

   ----------------
   -- From_Const --
   ----------------

   function From_Const (V : GL_Value) return GL_Value is (V);

   ----------------
   -- From_Const --
   ----------------

   function From_Const (V : GL_Value) return IDS is
     (if Is_A_Constant_Int (V) then (False, V) else Var_IDS)
     with Pre => Is_Constant (V), Post => Present (From_Const'Result);
   --  V is a constant. If it's a constant integer, return that value.
   --  Otherwise, don't treat it as a constant.

   -------------
   -- Convert --
   -------------

   function Convert
     (V              : IDS;
      GT             : GL_Type;
      Float_Truncate : Boolean := False;
      Is_Unchecked   : Boolean := False;
      No_Truncation  : Boolean := False) return IDS
   is
     (if   Is_Const (V)
      then (False,
            Convert (V.Value, GT, Float_Truncate, Is_Unchecked, No_Truncation))
      else Var_IDS);

   -------------
   -- Convert --
   -------------

   function Convert
     (V              : BA_Data;
      GT             : GL_Type;
      Float_Truncate : Boolean := False;
      Is_Unchecked   : Boolean := False;
      No_Truncation  : Boolean := False) return BA_Data
   is
     (if   Is_Const (V)
      then (False, Convert (V.C_Value, GT, Float_Truncate, Is_Unchecked,
                            No_Truncation),
            No_Uint)
      else V);

   ----------------
   -- From_Const --
   ----------------

   function From_Const (V : GL_Value) return BA_Data is
     (if   Is_A_Constant_Int (V) then (False, V, No_Uint) else No_BA)
     with Pre => Is_Constant (V);
   --  Likewise, for back-annotation

   ----------------------
   --  Is_Dynamic_Size --
   ----------------------

   function Is_Dynamic_Size
     (GT             : GL_Type;
      Max_Size       : Boolean := False;
      Allow_Overflow : Boolean := False;
      No_Padding     : Boolean := False) return Boolean
   is
      Size : IDS;
   begin
      --  If this is of elementary type, it's not of dynamic size. We have
      --  to do this test not just for efficiency, but also to avoid
      --  infinite recursion if we are passed Size_Type.

      if Is_Elementary_Type (GT) then
         return False;
      end if;

      --  Otherwise get the size for our purposes. If not a constant or not
      --  something LLVM can use natively as an array bound, this is dynamic.
      --  But we conservatively test for the range of Int to be consistent
      --  with how we create arrays.

      Size := Get_Type_Size (GT, No_GL_Value,
                             Max_Size   => Max_Size,
                             No_Padding => No_Padding);

      --  If the size isn't a constant, this is dynamically-sized. If it's
      --  a constant and we allow overflow, it isn't. Otherwise, we need to
      --  check for overflow.

      if not Is_Const (Size) then
         return True;
      elsif Allow_Overflow then
         return False;
      else
         return Overflowed (Size) or else Const_Int (Size) < 0
           or else Const_Int (Size) > LLI (Int'Last);
      end if;

   end Is_Dynamic_Size;

   ---------------
   -- ULL_Align --
   ---------------

   function ULL_Align (C : ULL) return Nat is
   begin
      if C = 0 or else (C and not (C - 1)) > ULL (Max_Valid_Align) then
         return Max_Valid_Align;
      else
         return Nat (C and (not (C - 1)));
      end if;
   end ULL_Align;

   ----------------
   -- Uint_Align --
   ----------------

   function Uint_Align (U : Uint) return Nat is
   begin
      --  If this is a small positive integer, use the above function.
      --  Otherwise, use the slow approach.

      if UI_Is_In_Int_Range (U) and then U >= 0 then
         return ULL_Align (ULL (+U));
      else
         return Align : Nat := Max_Valid_Align do
            while U mod Align /= 0 loop
               Align := Align / 2;
            end loop;
         end return;
      end if;
   end Uint_Align;

   ----------------------
   -- Is_Loadable_Type --
   ----------------------

   function Is_Loadable_Type (GT : GL_Type) return Boolean is
      T     : constant Type_T := Type_Of (GT);
      Align : constant Nat    := Get_Type_Alignment (GT);

   begin
      --  A type is loadable if it's native, not truncated, and it's not
      --  too large.

      return not Is_Nonnative_Type (GT) and then not Is_Truncated_GL_Type (GT)
        and then Get_Type_Size (T) / Align <= Max_Load_Size;

   end Is_Loadable_Type;

   -----------------------
   -- Build_Struct_Type --
   -----------------------

   function Build_Struct_Type
     (Types       : Type_Array;
      Packed      : Boolean       := False;
      Name        : Name_Id       := No_Name;
      Field_Names : Name_Id_Array := (1 .. 0 => <>)) return Type_T
   is
      T   : Type_T;
      UID : Unique_Id;

   begin
      if No (Name) then
         return Struct_Type (Types'Address, Types'Length, Packed);

      else
         T := Struct_Create_Named (Name);
         Struct_Set_Body (T, Types, Packed);

         if Field_Names'Length > 0 then
            UID := New_Unique_Id;
            for J in Field_Names'Range loop
               C_Set_Field_Info (UID, J - Field_Names'First, Field_Names (J),
                                 Is_Padding => No (Field_Names (J)));
            end loop;

            C_Set_Struct (UID, T);
         end if;

         return T;
      end if;
   end Build_Struct_Type;

   ---------------------
   -- Struct_Set_Body --
   ---------------------

   procedure Struct_Set_Body
     (T : Type_T; Types : Type_Array; Packed : Boolean := False)
   is
   begin
      Struct_Set_Body (T, Types'Address, Types'Length, Packed);
   end Struct_Set_Body;

   ----------------------
   -- Push_LValue_List --
   ----------------------

   procedure Push_LValue_List is
   begin
      LValue_Stack.Append (LValue_Pair_First);
      LValue_Pair_First := LValue_Pairs.Last + 1;
   end Push_LValue_List;

   ---------------------
   -- Pop_LValue_List --
   ---------------------

   procedure Pop_LValue_List is
   begin
      LValue_Pairs.Set_Last (LValue_Pair_First - 1);
      LValue_Pair_First := LValue_Stack.Table (LValue_Stack.Last);
      LValue_Stack.Decrement_Last;
   end Pop_LValue_List;

   ------------------------
   --  Clear_LValue_List --
   ------------------------

   procedure Clear_LValue_List is
   begin
      LValue_Pairs.Set_Last (LValue_Pair_First - 1);
   end Clear_LValue_List;

   -------------------------
   --  Add_To_LValue_List --
   -------------------------

   procedure Add_To_LValue_List (V : GL_Value) is
   begin
      --  Only add to the LValue list if this is a record type. We might
      --  be tempted to do this only if the type has discriminants, but
      --  that doesn't work because a parent might and it's not worth
      --  checking.

      if Is_Record_Type (Related_Type (V)) and then Disable_LV_Append = 0 then
         LValue_Pairs.Append (V);
      end if;
   end Add_To_LValue_List;

   -------------------------
   --  Add_To_LValue_List --
   -------------------------

   function Add_To_LValue_List (V : GL_Value) return GL_Value is
   begin
      Add_To_LValue_List (V);
      return V;
   end Add_To_LValue_List;

   ---------------------
   -- Get_LValue_List --
   ---------------------

   function Get_LValue_List return Access_GL_Value_Array is
      List : constant Access_GL_Value_Array :=
        new GL_Value_Array (1 .. LValue_Pairs.Last);

   begin
      for J in List'Range loop
         List (J) := LValue_Pairs.Table (J);
      end loop;

      return List;
   end Get_LValue_List;

   ---------------------
   -- Put_LValue_List --
   ---------------------

   procedure Put_LValue_List (L : in out Access_GL_Value_Array) is
   begin
      Clear_LValue_List;
      for V of L.all loop
         Add_To_LValue_List (V);
      end loop;

      Free (L);
   end Put_LValue_List;

   ------------------------
   -- Get_Matching_Value --
   ------------------------

   function Get_Matching_Value (TE : Type_Kind_Id) return GL_Value is
   begin
      --  Check in the opposite order of what we push. We may, for example
      --  be finding the size of an object of that size, in which case the
      --  object will have been added last.

      for J in reverse LValue_Pair_First .. LValue_Pairs.Last loop
         if Is_Parent_Of (TE,
                          Full_Etype (Related_Type (LValue_Pairs.Table (J))))
           or else Is_Parent_Of (Full_Etype (Related_Type
                                               (LValue_Pairs.Table (J))),
                                 TE)
         then
            --  ?? It would be more efficient to not convert to a reference
            --  here, but that might be quite a lot of work (see 8802-007).

            return Convert_Ref (Get (LValue_Pairs.Table (J), Any_Reference),
                                Default_GL_Type (TE));
         end if;
      end loop;

      --  Should never get here and postcondition verifies

      return No_GL_Value;
   end Get_Matching_Value;

   ------------------------
   -- Ultimate_Base_Type --
   ------------------------

   function Ultimate_Base_Type (TE : Type_Kind_Id) return Type_Kind_Id is
   begin
      return Typ : Type_Kind_Id := TE do
         while Full_Etype (Typ) /= Typ loop
            Typ := Full_Etype (Typ);
         end loop;
      end return;
   end Ultimate_Base_Type;

   -------------
   -- Type_Of --
   -------------

   function Type_Of (TE : Void_Or_Type_Kind_Id) return Type_T is
      GT    : GL_Type;

   begin
      --  First see if we already have a suitable type

      GT := Default_GL_Type (TE, Create => False);

      --  If we've already made a non-dummy GL_Type for this type, we
      --  can just return its LLVM type.

      if Present (GT) and then not Is_Dummy_Type (GT) then
         return Type_Of (GT);
      end if;

      --  Otherwise, make and return a new type

      return Create_Type (TE);
   end Type_Of;

   ----------------------
   -- Move_Into_Memory --
   ----------------------

   function Move_Into_Memory
     (Temp     : GL_Value;
      V        : GL_Value;
      Expr     : Opt_N_Subexpr_Id;
      GT       : GL_Type;
      Alloc_GT : GL_Type) return GL_Value
   is
      function Same_Memory (V1, V2 : Value_T) return Boolean is
        ((if    Present (Is_A_Bit_Cast_Inst (V1))
          then  Same_Memory (Get_Operand (V1, 0), V2)
          elsif Present (Is_A_Bit_Cast_Inst (V2))
          then  Same_Memory (Get_Operand (V2, 0), V1)
          else  V1 = V2))
        with Pre => Present (V1) and then Present (V2);
      --  Return True iff V1 and V2 are exactly the same memory location

      R        : constant GL_Relationship  := Relationship_For_Alloc (GT);
      New_Expr : constant Opt_N_Subexpr_Id := Strip_Complex_Conversions (Expr);
      Mem_GT   : constant GL_Type          := GT_To_Use (GT, Alloc_GT);
      Memory   : GL_Value                  :=
        (if   Is_Pointer (Temp)
         then Remove_Padding (Ptr_To_Relationship (Temp, Mem_GT, R))
         else Remove_Padding (Int_To_Relationship (Temp, Mem_GT, R)));
      New_V    : GL_Value                 :=
        (if    Present (V) then V
         elsif Present (New_Expr)
         then  Remove_Padding (Emit (New_Expr, LHS => Memory))
         else  No_GL_Value);

   begin
      --  If this is to get bounds and data and we have a value to store
      --  which contains data, convert it to bounds and data and store it.
      --  Otherwise, we have two cases, depending on the reason that we
      --  have bounds because Emit_Assignment only can handle the
      --  nominal type for alias to unconstrained case.

      if R = Reference_To_Bounds_And_Data then
         if Present (New_V) and then Is_Data (New_V)
           and then not Is_Nonnative_Type (Alloc_GT)
         then
            --  If Alloc_GT is an array type and has a different Etype than
            --  New_V, show that New_V is of that type so that we choose
            --  the proper bounds.

            if Is_Array_Type (Alloc_GT)
              and then Full_Etype (New_V) /= Full_Etype (Alloc_GT)
            then
               New_V := G_Is (New_V, Alloc_GT);
            end if;

            New_V  := Get (New_V, Bounds_And_Data);
            Memory := Ptr_To_Relationship (Memory, New_V, R);
         else
            if not Is_Constrained (GT) or else No (New_V)
              or else New_V = Memory
              or else (Is_Constr_Array_Subt_With_Bounds (GT) and then
                         not Is_Constr_Array_Subt_With_Bounds (Alloc_GT))
            then
               declare
                  Bounds : constant GL_Value :=
                    Get_Array_Bounds (Alloc_GT, Alloc_GT, New_V);

               begin
                  Store (Bounds, Get (Memory, Reference_To_Bounds));
               end;
            end if;
         end if;
      end if;

      --  If we have a value to move into memory, move it

      if Present (New_V) and then not Same_Memory (+New_V, +Memory)
        and then not Is_Undef (New_V)
      then
         Emit_Assignment (Memory, Value => New_V);
      end if;

      --  If we're not pointing to the correct type, fix that

      if Related_Type (Memory) /= GT then
         Memory := Convert_Ref (Memory, GT);
      end if;

      return Memory;

   end Move_Into_Memory;

   -----------------------
   -- Allocate_For_Type --
   -----------------------

   function Allocate_For_Type
     (GT       : GL_Type;
      Alloc_GT : GL_Type          := No_GL_Type;
      N        : Node_Id          := Empty;
      V        : GL_Value         := No_GL_Value;
      Expr     : Opt_N_Subexpr_Id := Empty;
      E        : Entity_Id        := Empty;
      Name     : String           := "";
      Max_Size : Boolean          := False) return GL_Value
   is
      Max_Alloc  : constant ULL     := 20_000_000;
      A_GT       : constant GL_Type :=
        (if Present (Alloc_GT) then Alloc_GT else GT);
      Align      : constant Nat     := Get_Alloc_Alignment (GT, A_GT, E);
      Value      : GL_Value         := V;
      Element_GT : GL_Type;
      Num_Elts   : GL_Value;

   begin
      --  Skip this when only processing decls

      if Decls_Only then
         return Get_Undef_Ref (GT);

      --  We have three cases. If the object has a native type, we just do the
      --  alloca and that's all. Test for the size being either an overflow or
      --  an undef, which we'll assume was likely caused by an overflow. If
      --  this is an undef, it likely means that we already said we were
      --  raising constraint error, so if we did, omit this one.

      elsif not Is_Nonnative_Type (A_GT) then
         if Do_Stack_Check
           and then Get_Type_Size (Type_Of (A_GT)) > Max_Alloc * UBPU
         then
            Emit_Raise_Call (N, SE_Object_Too_Large);
            Error_Msg_N ("??Storage_Error will be raised at run time!", N);
            return Get_Undef_Ref (GT);
         else
            declare
               Align_GT : constant GL_Type :=
                 (if   GT_Alignment (A_GT) >= Align then A_GT
                  else Make_GT_Alternative (A_GT, E, Align => +Align));

            begin
               return Move_Into_Memory (Alloca (Align_GT, E, Align, Name),
                                        Value, Expr, GT, A_GT);
            end;
         end if;
      end if;

      --  Otherwise, we probably have to do some sort of dynamic allocation. If
      --  this is an array of a component that's not of dynamic size, we can
      --  allocate an array of the component type corresponding to the array
      --  type and cast it to a pointer to the actual type. If not, we have to
      --  allocate it as an array of bytes. We must use an array of bytes if we
      --  have to include bounds. If this is an unconstrained array, we need to
      --  find the bounds, so evaluate Expr if Present and there's no Value.

      if Is_Unconstrained_Array (A_GT) and then No (Value)
        and then Present (Expr)
      then
         Value := Emit (Expr);
      end if;

      if Is_Array_Type (A_GT)
        and then Is_Native_Component_GT (Full_Component_GL_Type (A_GT))
        and then not Is_Constr_Array_Subt_With_Bounds (GT)
      then
         Element_GT := Full_Component_GL_Type (A_GT);
         Num_Elts   := Get_Array_Elements (Value, Full_Etype (A_GT));
      else
         Element_GT := SSI_GL_Type;
         Num_Elts   :=
           To_Bytes (Get_Alloc_Size (GT, A_GT, Value, Max_Size));
      end if;

      --  If this is an aliased array of nominal constrained type and the
      --  number of elements isn't a constant, make sure that it's at least one
      --  element.

      if Present (E)
        and then Is_Aliased (E)
        and then Is_Array_Type (A_GT)
        and then not Is_Constr_Array_Subt_With_Bounds (GT)
        and then not Is_Constant (Num_Elts)
      then
         Num_Elts := Build_Max (Num_Elts, Size_Const_Int (Uint_1));
      end if;

      --  Check that we aren't trying to allocate too much memory. Raise
      --  Storage_Error if so. We don't try to support local exception
      --  labels and -fstack-check at the same time. The divide below
      --  will constant-fold, but make sure we aren't dividing by zero.

      if Do_Stack_Check and then not Is_Zero_Size (Element_GT) then

         --  If everything is constant, we may know that we unconditionally
         --  overflow.

         if not Is_Dynamic_Size (A_GT, Allow_Overflow => True) then
            if Overflowed (GL_Value'(Get_Type_Size (A_GT)))
              or else +Get_Type_Size (A_GT) > Max_Alloc * UBPU
            then
               Emit_Raise_Call (N, SE_Object_Too_Large);
               Error_Msg_N
                 ("??Storage_Error will be raised at run time!", N);
               return Get_Undef_Ref (GT);
            end if;
         else
            Emit_Raise_Call_If (I_Cmp (Int_UGT, Num_Elts,
                                       U_Div (Size_Const_Int (Max_Alloc),
                                              Get_Type_Size (Element_GT))),
                                N, SE_Object_Too_Large);
         end if;
      end if;

      --  If the number of elements overflowed, raise Storage_Error. But
      --  check for the pathological case of an array of zero-sized elements.

      if Overflowed (Num_Elts) or else Is_Undef (Num_Elts) then
         if Get_Type_Size (Element_GT) = 0 then
            Num_Elts := Size_Const_Int (Uint_1);
         else
            if not Is_Undef (Num_Elts) then
               Error_Msg_N ("??Storage_Error will be raised at run time!",
                            N);
            end if;

            Emit_Raise_Call (N, SE_Object_Too_Large);
            return Get_Undef_Ref (GT);
         end if;
      end if;

      --  If we're emitting C and we have an array of zero-sized objects,
      --  return Undef.

      if Emit_C and then Is_Zero_Size (Element_GT) then
         return Get_Undef_Ref (GT);
      end if;

      --  Otherwise allocate the object and then move any data into it.

      return Move_Into_Memory (Array_Alloca
                                 (Element_GT, Num_Elts, E, Align, Name),
                               Value, Expr, GT, A_GT);

   end Allocate_For_Type;

   ----------------------------
   -- Heap_Allocate_For_Type --
   ----------------------------

   function Heap_Allocate_For_Type
     (GT        : GL_Type;
      Alloc_GT  : GL_Type                 := No_GL_Type;
      V         : GL_Value                := No_GL_Value;
      N         : Node_Id                 := Empty;
      Access_GT : GL_Type                 := No_GL_Type;
      Expr      : Opt_N_Subexpr_Id        := Empty;
      Proc      : Opt_Subprogram_Kind_Id  := Empty;
      Pool      : Entity_Id               := Empty;
      E         : Opt_Allocatable_Kind_Id := Empty;
      Max_Size  : Boolean                 := False) return GL_Value
   is
      A_GT    : constant GL_Type   :=
        (if Present (Alloc_GT) then Alloc_GT else GT);
      Value   : constant GL_Value :=
        (if    Present (V) then V
         elsif Is_Self_Referential_Type (A_GT) and then Present (Expr)
         then  Emit (Expr) else No_GL_Value);
      Align   : constant Nat      := Get_Alloc_Alignment (GT, A_GT, E);
      Align_V : constant GL_Value := Size_Const_Int (ULL (Align));
      Size    : GL_Value          :=
        Get_Alloc_Size (GT, A_GT, Value, Max_Size);
      Result  : GL_Value;

   begin
      --  Check that we aren't violating any restrictions

      if Present (N)
        and then not (Nkind (N) = N_Allocator and then Comes_From_Source (N))
      then
         if No (Pool) then
            Check_No_Implicit_Heap_Alloc (N);
         end if;

         if Has_Task (GT) then
            Check_No_Implicit_Task_Alloc (N);
         end if;

         if Has_Protected (GT) then
            Check_No_Implicit_Protected_Alloc (N);
         end if;
      end if;

      --  If the size overflowed, raise Storage_Error

      if Overflowed (Size) or else Is_Undef (Size) then
         if not Is_Undef (Size) then
            Error_Msg_N ("??Storage_Error will be raised at run time!", N);
         end if;

         Emit_Raise_Call (N, SE_Object_Too_Large);
         return Get_Undef_Ref (GT);

      --  Otherwise, if this is an aliased array of nominal constrained
      --  type and the number of elements isn't a constant, make sure that
      --  it's at least one byte.

      elsif Present (E)
        and then Is_Aliased (E)
        and then Is_Array_Type (A_GT)
        and then not Is_Constr_Array_Subt_With_Bounds (GT)
        and then not Is_Constant (Size)
      then
         Size := Build_Max (Size, Size_Const_Int (Uint_1));
      end if;

      --  If no procedure was specified, use the default memory allocation
      --  function, where we just pass a size. But we can only do this
      --  directly if the requested alignment is a constant and no larger
      --  than the system allocator alignment.

      if No (Proc) and then Align <= Get_System_Allocator_Alignment * BPU then
         Result := Call (Get_Default_Alloc_Fn, (1 => To_Bytes (Size)));

      --  Otherwise, if we can use the default memory allocation
      --  function but have to overalign, increase the size by both
      --  the alignment and the space needed for a pointer, align the
      --  result (leaving space for the pointer) and store the obtained
      --  address immediately before the value.

      elsif No (Proc) then
         declare
            Ptr_Size   : constant GL_Value := Get_Type_Size (A_Char_GL_Type);
            Total_Size : constant GL_Value := Size + Align + Ptr_Size;
            Alloc_R    : constant GL_Value :=
              Call (Get_Default_Alloc_Fn, (1 => To_Bytes (Total_Size)));
            Alloc      : constant GL_Value :=
              (if   Is_Pointer (Alloc_R)
               then Ptr_To_Int (Alloc_R, Size_GL_Type) else Alloc_R);
            Aligned    : constant GL_Value :=
              Align_To (Address_Add (Alloc, To_Bytes (Ptr_Size)),
                        Get_System_Allocator_Alignment, To_Bytes (Align));
            Ptr_Loc    : constant GL_Value :=
              Address_Sub (Aligned, To_Bytes (Ptr_Size));

         begin
            --  It may have been the case that Size didn't oveflow until
            --  we did the computation above. So check again.

            if Overflowed (Total_Size) then
               Error_Msg_N ("??Storage_Error will be raised at run time!",
                            N);
               Emit_Raise_Call (N, SE_Object_Too_Large);
               return Get_Undef_Ref (GT);
            end if;

            Store (Alloc, Int_To_Ref (Ptr_Loc, A_Char_GL_Type));
            Result := Convert (Aligned, A_Char_GL_Type);
         end;

      --  If a procedure was specified (meaning that a pool must also have
      --  been specified) and the pool is a record, then it's a storage
      --  pool and we pass the pool, size, and alignment. Be sure that we
      --  convert the pool to actual type of the formal of the deallocator
      --  function: it may be a derived type.

      elsif Is_Record_Type (Full_Etype (Pool)) then
         Result :=
           Call_Alloc (Proc, N,
                       (1 => Ptr_To_Ref (Emit_Entity (Pool),
                                         Full_GL_Type (First_Formal (Proc))),
                        2 => To_Bytes (Size),
                        3 => To_Bytes (Align_V)));

      --  Otherwise, this is the secondary stack

      else
         Result := Call_Alloc (Proc, N, (1 => To_Bytes (Size),
                                         2 => To_Bytes (Align_V)));
      end if;

      --  If this is for a non-default storage model, indicate that

      if Present (Access_GT)
        and then Has_Designated_Storage_Model_Aspect (Access_GT)
      then
         Set_SM_Object (Result, Storage_Model_Object (Access_GT));
      end if;

      --  Set the known alignment for the address and move any data into it

      Set_Alignment (Result, Align);
      return Move_Into_Memory (Result, Value, Expr, GT, A_GT);
   end Heap_Allocate_For_Type;

   ---------------------
   -- Heap_Deallocate --
   ---------------------

   procedure Heap_Deallocate
     (V        : GL_Value;
      Desig_GT : GL_Type;
      Proc     : Opt_Subprogram_Kind_Id;
      Pool     : Entity_Id)
   is
      Conv_V   : GL_Value := V;
      DT       : GL_Type  := Related_Type (V);
      Alloc_GT : GL_Type  := DT;
      Free_V   : GL_Value;

   begin
      --  If V is an access type, convert it to a reference to the
      --  underlying data. We also want to record the actual designated
      --  type in this case since it may contain bound information and
      --  we need to record the bounds as well as their size.

      if Is_Data (V) and then Is_Access_Type (V) then
         Conv_V   := From_Access (V);
         DT       := Full_Designated_GL_Type (V);
         Alloc_GT := DT;
      end if;

      --  If we have a designated type, that's the type we use for
      --  computing size and alignment.

      if Present (Desig_GT) then
         Alloc_GT := Desig_GT;
      end if;

      --  If V is an unconstrained array, we want a pointer to the bounds
      --  and data. Otherwise just a Reference. We'll then either convert
      --  it to a generic pointer or to an integer (System.Address).

      Free_V := Get (Conv_V, Relationship_For_Alloc (DT));

      declare
         Size    : constant GL_Value :=
           Get_Alloc_Size (DT, Alloc_GT, Conv_V, For_Dealloc => True);
         Align   : constant Nat      :=
           Get_Alloc_Alignment (DT, Alloc_GT, Empty);
         Align_V : constant GL_Value := Size_Const_Int (ULL (Align));

      begin
         --  If no procedure was specified, use the default memory deallocation
         --  procedure, where we just pass a size. But we can only do this
         --  directly if the requested alignment is a constand and no larger
         --  than the system allocator alignment.

         if No (Proc) and then Align <= Get_System_Allocator_Alignment * BPU
         then
            Call (Get_Default_Free_Fn,
                  (1 => (if   Emit_C
                         then Convert_To_Access (Free_V, A_Char_GL_Type)
                         else Ptr_To_Int (Free_V, Size_GL_Type))));

         --  If we have to use the normal deallocation procedure to
         --  deallocate an overaligned value, the actual address of the
         --  memory to deallocate can be found in front of the value we're
         --  passed.

         elsif No (Proc) then
            declare
               Addr       : constant GL_Value :=
                 Ptr_To_Int (Free_V, Size_GL_Type);
               Ptr_Size   : constant GL_Value :=
                 Get_Type_Size (A_Char_GL_Type);
               Ptr_Loc    : constant GL_Value := Addr - To_Bytes (Ptr_Size);
               Ptr_Addr   : constant GL_Value :=
                 Load (Int_To_Ref (Ptr_Loc, A_Char_GL_Type));

            begin
               Call (Get_Default_Free_Fn,
                     (1 => (if   Emit_C
                            then Convert_To_Access (Ptr_Addr, A_Char_GL_Type)
                            else Ptr_To_Int (Ptr_Addr, Size_GL_Type))));
            end;

         --  If a procedure was specified (meaning that a pool must also
         --  have been specified) and the pool is a record, then it's a
         --  storage pool and we pass the pool, size, and alignment. Be
         --  sure that we convert the pool to actual type of the formal of
         --  the deallocator function: it may be a derived type.

         elsif Is_Record_Type (Full_Etype (Pool)) then
            Call_Dealloc (Proc,
                          (1 => Ptr_To_Ref (Emit_Entity (Pool),
                                            Full_GL_Type
                                              (First_Formal (Proc))),
                           2 => Ptr_To_Address_Type (Free_V),
                           3 => To_Bytes (Size),
                           4 => To_Bytes (Align_V)));

            --  Otherwise, this is the secondary stack and we just call
            --  it with the size.

         else
            Call_Dealloc (Proc, (1 => Ptr_To_Address_Type (Free_V),
                                 2 => To_Bytes (Size)));
         end if;
      end;
   end Heap_Deallocate;

   --------------------------
   -- Prepare_SM_Copy_Host --
   --------------------------

   function Prepare_SM_Copy_Host (V : GL_Value) return GL_Value is
      Result : GL_Value := V;

   begin
      --  We may need to change the relationship of V. If it's a
      --  double reference, we need to change it to a single reference
      --  and if it's data, we need to change to a reference.

      if Is_Double_Reference (Result) then
         Result := Get (Result, Deref (Relationship (Result)));
      elsif not Is_Reference (Result) then
         Result := Get (Result, Ref (Relationship (Result)));
      end if;

      --  Then convert to a size_type

      return Ptr_To_Address_Type (Result);
   end Prepare_SM_Copy_Host;

   ----------------------------
   -- Prepare_SM_Copy_Target --
   ----------------------------

   function Prepare_SM_Copy_Target (V : GL_Value) return GL_Value is
      Result : GL_Value := V;

   begin
      --  If V is a double reference, we need to change it to a single
      --  reference.

      if Is_Double_Reference (Result) then
         Result := Get (Result, Deref (Relationship (Result)));
      end if;

      --  We must now have a reference since we can't have target data

      pragma Assert (Is_Reference (Result));

      --  Then convert to the appropriate address type

      return Ptr_To_Int (Result, SM_Address_Type (Result));
   end Prepare_SM_Copy_Target;

   -----------------------
   -- Call_SM_Copy_From --
   -----------------------

   procedure Call_SM_Copy_From (Dest, Src, Size : GL_Value) is
   begin
      Call (Emit_Entity (SM_Copy_From (Src)),
            (1 => Emit_Entity (SM_Object (Src)),
             2 => Prepare_SM_Copy_Host (Dest),
             3 => Prepare_SM_Copy_Target (Src),
             4 => Get (Size, Data)));
   end Call_SM_Copy_From;

   ---------------------
   -- Call_SM_Copy_To --
   ---------------------

   procedure Call_SM_Copy_To (Dest, Src, Size : GL_Value) is
   begin
      Call (Emit_Entity (SM_Copy_To (Dest)),
            (1 => Emit_Entity (SM_Object (Dest)),
             2 => Prepare_SM_Copy_Target (Dest),
             3 => Prepare_SM_Copy_Host (Src),
             4 => Get (Size, Data)));
   end Call_SM_Copy_To;

   ------------------
   -- To_Size_Type --
   ------------------

   function To_Size_Type (V : GL_Value) return GL_Value is
     (Convert (V, Size_GL_Type));

   ------------------------
   -- Get_Type_Alignment --
   ------------------------

   function Get_Type_Alignment
     (GT : GL_Type; Use_Specified : Boolean := True) return Nat
   is
      TE    : constant Type_Kind_Id := Full_Etype (GT);
      Align : constant Nat          := GT_Alignment (GT);
      T     : constant Type_T       := Type_Of (GT);

   begin
      --  If there's a known alignment in this GL_Type, use it

      if Align /= 0 then
         return Align;

      --  If the alignment is specified (or back-annotated) in the tree,
      --  use that value.

      elsif Known_Alignment (TE) and then Use_Specified then
         return +Alignment (TE) * BPU;

      --  If we're only elaborating and back-annotating types, check if
      --  is void.

      elsif Decls_Only and then Get_Type_Kind (T) = Void_Type_Kind then
         return BPU;
      end if;

      case Ekind (TE) is

         when Array_Kind =>
            return Get_Array_Type_Alignment (TE);

         when Record_Kind =>
            return Get_Record_Type_Alignment (TE);

         when Access_Subprogram_Kind =>

            --  These usually use a fat pointer, so force the alignment to
            --  be its size unless the alignment in the tree is smaller.

            return (if   Known_Alignment (TE)
                    then Nat'Min (Nat (ULL'(Get_Type_Size (T))),
                                  +Alignment (TE) * BPU)
                    else Nat (ULL'(Get_Type_Size (T))));

         when E_Subprogram_Type =>

            --  There really isn't an alignment, but indicate code can be
            --  anywhere.

            return BPU;

         when others =>
            --  Otherwise, it must be an elementary type, so get the LLVM
            --  type's alignment

            return Get_Type_Alignment (T);
      end case;

   end Get_Type_Alignment;

   package body Size is

      -------------------
      -- Get_Type_Size --
      -------------------

      function Get_Type_Size
        (GT         : GL_Type;
         V          : GL_Value := No_GL_Value;
         Max_Size   : Boolean  := False;
         No_Padding : Boolean  := False) return Result
      is
         Size_GT      : constant GL_Type  :=
           (if   No_Padding and then Has_Padding (GT)
            then Primitive_GL_Type (GT) else GT);
         Size_In_GT   : constant GL_Value := GT_Size (Size_GT);
         Use_Max_Size : constant Boolean  := Max_Size or else Is_Max_Size (GT);
         Unpad_Record : constant Boolean  :=
           No_Padding and then Is_Record_Type (GT);
         Our_Size     : Result;

      begin
         --  If a value was specified and it's data, then it must be of a
         --  fixed size. That's the size we're looking for.

         if Present (V) and then Is_Data (V)
           and then not Use_Max_Size and then not Unpad_Record
         then
            Our_Size := From_Const (Get_Type_Size (Type_Of (V)));

            --  However, if this is both bounds and data, we have to subtract
            --  the size of the bounds since we define the size of the
            --  type itself to not include the bounds.

            if Relationship (V) = Bounds_And_Data then
               Our_Size :=
                 Our_Size - From_Const (Get_Bound_Size (Related_Type (V)));
            end if;

            return Our_Size;

         --  If there's a size specified in this GT, that's what we want
         --  unless we aren't to remove padding and this is a record type.

         elsif Present (Size_In_GT) and then not Unpad_Record then
            return From_Const (Size_In_GT);

         --  If this is a subprogram type, it doesn't have a size

         elsif Ekind (GT) = E_Subprogram_Type then
            return Empty_Result;

         --  If this isn't a non-native type, then the size is the size of the
         --  LLVM type and unless we aren't to remove padding and this is a
         --  record type.

         elsif not Is_Nonnative_Type (GT) and then not Unpad_Record then
            return From_Const (Get_Type_Size (Type_Of (GT)));

         elsif Is_Record_Type (GT) then
            return Get_Record_Type_Size (Full_Etype (GT), V,
                                         Max_Size   => Use_Max_Size,
                                         No_Padding =>
                                           No_Padding
                                           and then not Strict_Alignment (GT));
         elsif Is_Array_Type (GT) and then not Is_Constrained (GT) then
            return Get_Unc_Array_Type_Size (Full_Etype (GT), V, Use_Max_Size);
         elsif Is_Array_Type (GT) then
            return Get_Array_Type_Size (Full_Etype (GT), V, Use_Max_Size);
         else
            pragma Assert (Standard.False);
            return Empty_Result;
         end if;

      end Get_Type_Size;
   end Size;

   package LLVM_Size is
      new Size (Result                  => GL_Value,
                Empty_Result            => No_GL_Value,
                From_Const              => From_Const,
                Get_Record_Type_Size    => Get_Record_Type_Size,
                Get_Unc_Array_Type_Size => Get_Unc_Array_Type_Size,
                Get_Array_Type_Size     => Get_Array_Type_Size,
                "-"                     => "-");

   function Get_Type_Size
     (GT         : GL_Type;
      V          : GL_Value := No_GL_Value;
      Max_Size   : Boolean  := False;
      No_Padding : Boolean  := False) return GL_Value
     renames LLVM_Size.Get_Type_Size;

   package IDS_Size is
      new Size (Result                  => IDS,
                Empty_Result            => No_IDS,
                From_Const              => From_Const,
                Get_Record_Type_Size    => Get_Record_Type_Size,
                Get_Unc_Array_Type_Size => Get_Unc_Array_Type_Size,
                Get_Array_Type_Size     => Get_Array_Type_Size,
                "-"                     => "-");

   function Get_Type_Size
     (GT         : GL_Type;
      V          : GL_Value := No_GL_Value;
      Max_Size   : Boolean  := False;
      No_Padding : Boolean  := False) return IDS
     renames IDS_Size.Get_Type_Size;

   package BA_Size is
      new Size (Result                  => BA_Data,
                Empty_Result            => No_BA,
                From_Const              => From_Const,
                Get_Record_Type_Size    => Get_Record_Type_Size,
                Get_Unc_Array_Type_Size => Get_Unc_Array_Type_Size,
                Get_Array_Type_Size     => Get_Array_Type_Size,
                "-"                     => "-");

   function Get_Type_Size
     (GT         : GL_Type;
      V          : GL_Value := No_GL_Value;
      Max_Size   : Boolean  := False;
      No_Padding : Boolean  := False) return BA_Data
     renames BA_Size.Get_Type_Size;

   --------------------
   -- Get_Alloc_Size --
   --------------------

   function Get_Alloc_Size
     (GT          : GL_Type;
      Alloc_GT    : GL_Type;
      V           : GL_Value;
      Max_Size    : Boolean := False;
      For_Dealloc : Boolean := False) return GL_Value
   is
      Class_Wide : constant Boolean :=
        Is_Class_Wide_Equivalent_Type (Alloc_GT);
      Size_GT    : constant GL_Type := GT_To_Use (GT, Alloc_GT);
      Size       : GL_Value         :=
        Get_Type_Size (Size_GT,
                       (if   not For_Dealloc and then Class_Wide
                        then No_GL_Value else V),
                       Max_Size => Max_Size);
      --  If we're allocating a classwide equivalent type, we want to ignore
      --  the initial value, but not if deallocating one.

   begin
      --  Adjust size if constrained subtype for aliased unconstrained or
      --  for unconstrained itself.

      if Is_Unconstrained_Array (GT) or else Type_Needs_Bounds (GT) then
         Size := Align_To (Size + Get_Bound_Size (GT),
                           Get_Type_Alignment (GT), Get_Bound_Alignment (GT));
      end if;

      return Size;
   end Get_Alloc_Size;

   -------------------------
   -- Get_Alloc_Alignment --
   -------------------------

   function Get_Alloc_Alignment
     (GT       : GL_Type;
      Alloc_GT : GL_Type;
      E        : Entity_Id := Empty) return Nat
   is
      Align_GT    : constant GL_Type := GT_To_Use (GT, Alloc_GT);
      GT_Align    : constant Nat     := Get_Type_Alignment (Align_GT);
      E_Align     : constant Nat     :=
        (if   Present (E) and then Known_Alignment (E)
         then +Alignment (E) * BPU else BPU);
      Bound_Align : constant Nat     :=
        (if   Is_Unconstrained_Array (GT) or else Type_Needs_Bounds (Alloc_GT)
         then Get_Bound_Alignment (GT) else BPU);

   begin
      return Nat'Max (Nat'Max (GT_Align, Bound_Align), E_Align);
   end Get_Alloc_Alignment;

   ------------------
   -- Compute_Size --
   ------------------

   function Compute_Size
     (Left_GT, Right_GT       : GL_Type;
      Left_Value, Right_Value : GL_Value;
      For_Assignment          : Boolean := False) return GL_Value
   is
      Left_BAT       : constant Boolean := Is_Byte_Array_GL_Type (Left_GT);
      Right_BAT      : constant Boolean := Is_Byte_Array_GL_Type (Right_GT);
      Left_PT        : constant GL_Type :=
        (if Left_BAT then Primitive_GL_Type (Left_GT) else Left_GT);
      Right_PT        : constant GL_Type :=
        (if Right_BAT then Primitive_GL_Type (Right_GT) else Right_GT);
      Left_Complex   : constant Nat     := Get_Type_Size_Complexity (Left_PT);
      Right_Complex  : constant Nat     := Get_Type_Size_Complexity (Right_PT);
      Class_Wide     : constant Boolean :=
        Is_Class_Wide_Equivalent_Type (Left_GT);
      Copy_Padding   : Boolean          := False;
      Use_Right      : Boolean;
      Size_GT        : GL_Type;
      Size_Value     : GL_Value;

   begin
      --  Next, handle some special cases. If this is an assignment to a
      --  classwide equivalent type, we must use the size of that type
      --  since that's the way the type is defined. We include padding in
      --  this case.

      if For_Assignment and then Class_Wide then
         Use_Right    := False;
         Copy_Padding := True;

      --  If the LHS of an assignment is a byte array type, we use the size
      --  of the RHS.

      elsif For_Assignment and then Left_BAT then
         Use_Right := True;

      --  Otherwise, legal code requires that the sizes of both types are
      --  the same, so we can use either size and chose the one that's
      --  easiest to compute.

      else
         Use_Right := Right_Complex <= Left_Complex;
      end if;

      --  Now that we know which side to use, return the size of that type.

      Size_GT    := (if Use_Right then Right_GT    else Left_GT);
      Size_Value := (if Use_Right then Right_Value else Left_Value);

      --  If we're left with computing the size of a byte array type,
      --  we know this isn't the LHS of a comparison. So we want the actual
      --  data size.

      if Is_Byte_Array_GL_Type (Size_GT) then
         Size_GT    := Primitive_GL_Type (Size_GT);
         Size_Value := To_Primitive (Size_Value);
      end if;

      --  Now we have everything we need

      return Get_Type_Size (Size_GT, Size_Value,
                            No_Padding => not Copy_Padding);
   end Compute_Size;

   ------------------------------
   -- Get_Type_Size_Complexity --
   ------------------------------

   function Get_Type_Size_Complexity
     (GT : GL_Type; Max_Size : Boolean := False) return Nat is
   begin
      --  If the size of the GL_Type is known, then it's a constant

      if Present (GT_Size (GT)) then
         return 0;
      elsif Is_Record_Type (GT) then
         return Get_Record_Size_Complexity (Full_Etype (GT), Max_Size);
      elsif Is_Array_Type (GT) then
         return Get_Array_Size_Complexity  (Full_Etype (GT), Max_Size);

      else
         --  All other types are constant size

         return 0;

      end if;
   end Get_Type_Size_Complexity;

   -----------------------------------
   -- Get_Attribute_From_Annotation --
   -----------------------------------

   function Get_Attribute_From_Annotation
     (N : N_Attribute_Reference_Id) return Uint
   is
      Attr   : constant Attribute_Id         :=
        Get_Attribute_Id (Attribute_Name (N));
      TE     : constant Void_Or_Type_Kind_Id := Full_Etype (Prefix (N));
      E      : constant Entity_Id            :=
        (if    Is_Entity_Name (Prefix (N)) then Entity (Prefix (N))
         elsif Nkind (Prefix (N)) = N_Selected_Component
         then  Entity (Selector_Name (Prefix (N)))
         else  Empty);
      Use_E  : constant Entity_Id            :=
         (if Present (E) then E else TE);
      Our_E  : constant Entity_Id            :=
         (if Is_Type (Use_E) then Get_Fullest_View (Use_E) else Use_E);
      Ret    : Uint                          := No_Uint;

   begin
      --  We have to be careful here because even though we don't
      --  usually need to evaluate the Prefix to get its size, we are
      --  required to, so it must be static

      if No (E) and then not Is_No_Elab_Needed (Prefix (N)) then
         return No_Uint;
      end if;

      case Attr is
         when Attribute_Object_Size | Attribute_Size | Attribute_Value_Size =>

            if not Is_Type (Our_E) or else Attr = Attribute_Object_Size then
               if Known_Esize (Our_E) then
                  Ret := Esize (Our_E);
               end if;
            elsif Known_RM_Size (Our_E) then
               Ret := RM_Size (Our_E);
            end if;

         when Attribute_Max_Size_In_Storage_Elements =>
            if Known_Esize (Our_E) and then Is_Static_SO_Ref (Esize (Our_E))
            then
               Ret := Esize (Our_E) / BPU;

               if Is_Unconstrained_Array (TE) then
                  Ret := Ret + UI_From_GL_Value
                    (To_Bytes (Get_Bound_Size (Default_GL_Type (TE))));
               end if;

               return Ret;
            end if;

         when Attribute_Descriptor_Size =>
            return +Get_Bound_Size (Default_GL_Type (TE));

         when Attribute_Component_Size =>
            if Known_Component_Size (TE) then
               Ret := Component_Size (TE);
            end if;

         when Attribute_Alignment =>
            if Ekind (Our_E) not in E_Component | E_Discriminant
              and then Known_Alignment (Our_E)
            then
               return Alignment (Our_E);
            end if;

         when Attribute_Position | Attribute_Bit_Position =>

            Ret := Component_Bit_Offset (Our_E);

            if Present (Ret) and then Is_Static_SO_Ref (Ret)
              and then Attr = Attribute_Position
            then
               Ret := Ret / BPU;
            end if;

         when Attribute_First_Bit | Attribute_Bit =>
            if Ekind (Our_E) in E_Discriminant | E_Component
              and then Known_Normalized_Position (Our_E)
            then
               Ret := Normalized_First_Bit (Our_E);
            end if;

         when Attribute_Last_Bit =>
            if Known_Normalized_First_Bit (Our_E) then
               Ret := Normalized_First_Bit (Our_E);
            end if;

            if Present (Ret) and then Is_Static_SO_Ref (Ret)
              and then Known_Esize (Our_E)
              and then Is_Static_SO_Ref (Esize (Our_E))
            then
               Ret := Ret + Esize (Our_E) - 1;
            end if;

         when others =>
            null;
      end case;

      return (if   Present (Ret) and then Is_Static_SO_Ref (Ret) then Ret
              else No_Uint);
   end Get_Attribute_From_Annotation;

   ------------------------------
   -- Add_Flags_To_Instruction --
   ------------------------------

   procedure Add_Flags_To_Instruction
     (Inst : Value_T; V : GL_Value; Special_Atomic : Boolean := False)
   is
      GT           : constant GL_Type    := Related_Type (V);
      Align        : constant Nat        := Alignment (V);
      Our_Volatile : constant Boolean    := Is_Volatile (V);
      Our_Atomic   : constant Boolean    :=
        Is_Atomic (V)
        and then (Special_Atomic or else Atomic_Kind (Element_Type_Of (V)));

   begin
      --  We always set the alignment, since that's correct for all
      --  references, and add aliasing information unless we have an undef.

      Set_Alignment (Inst, unsigned (To_Bytes (Align)));

      if not Is_Undef (V) then
         Add_Aliasing_To_Instruction (Inst, V);
      end if;

      --  But nothing else is correct unless this is a single reference.

      if not Is_Single_Reference (V) then
         return;
      end if;

      --  If this is Atomic, the alignment must be at least as large as the
      --  size.

      pragma Assert (not Our_Atomic
                       or else ULL (Align) >= Get_Type_Size (Type_Of (GT)));
      Set_Volatile  (Inst, Our_Volatile);
      Set_Ordering  (Inst,
                     (if  Our_Atomic
                      then Atomic_Ordering_Sequentially_Consistent
                      else Atomic_Ordering_Not_Atomic));

   end Add_Flags_To_Instruction;

   ------------------------------
   -- Check_OK_For_Atomic_Type --
   ------------------------------

   procedure Check_OK_For_Atomic_Type
     (GT : GL_Type; E : Entity_Id; Is_Component : Boolean := False)
   is
      T           : constant Type_T          := Type_Of (GT);
      Align       : constant Nat             := Get_Type_Alignment (GT);
      Pragma_Node : constant Opt_N_Pragma_Id :=
        Get_Pragma (E, (if   Is_Component then Pragma_Atomic_Components
                        else Pragma_Atomic));
      Error_Node  : constant Node_Id         :=
        (if   Present (Pragma_Node)
         then First (Pragma_Argument_Associations (Pragma_Node)) else E);

   begin
      --  If we're emitting C and this is in the source, give a warning

      if Emit_C and then Comes_From_Source (E) and then Is_Type (E) then
         if Is_Component then
            Error_Msg_NE ("??type & with atomic components", Error_Node, E);
            Error_Msg_N  ("\\treated as having volatile components",
                          Error_Node);
         else
            Error_Msg_NE ("??atomic type & treated as volatile",
                          Error_Node, E);
         end if;
      end if;

      --  If this is an anonymous base type, nothing to check, the
      --  error will be reported on the source type if need be.

      if not Comes_From_Source (E)

        --  Consider all aligned elementary types as atomic except fat
        --  pointers.

        or else (Is_Elementary_Type (GT)
                   and then Align >= Get_Type_Alignment (T)
                   and then Get_Type_Kind (T) /= Struct_Type_Kind)

        --  Or if it's a fixed size, the size is equal to the alignment,
        --  and the alignment is less than two words.

        or else (not Is_Dynamic_Size (GT)
                   and then Align <= Get_Bits_Per_Word * 2
                   and then ULL (Align) = +Get_Type_Size (GT))
      then
         return;
      end if;

      --  Now give the appropriate error message

      if Is_Component then
         Error_Msg_NE ("atomic access to component of & cannot be guaranteed",
                       Error_Node, E);
      elsif Is_Volatile_Full_Access (E) then
         Error_Msg_NE ("volatile full access to & cannot be guaranteed",
                       Error_Node, E);
      else
         Error_Msg_NE ("atomic access to & cannot be guaranteed",
                       Error_Node, E);
      end if;

   end Check_OK_For_Atomic_Type;

   ---------------------
   -- Field_Error_Msg --
   ---------------------

   function Field_Error_Msg
     (E : Entity_Id; GT : GL_Type; Only_Special : Boolean) return String
   is
      Atomic      : constant Boolean   :=
        Is_Full_Access (E) or else Is_Full_Access (GT);
      Independent : constant Boolean   :=
        Is_Independent (E) or else Is_Independent (GT);
      By_Ref_Type : constant Boolean   := Is_By_Reference_Type (GT);
      Error_Str   : constant String    :=
        (if    Atomic                  then " atomic &"
         elsif Is_Aliased (E)          then " aliased &"
         elsif Independent             then " independent &"
         elsif By_Ref_Type             then "& with by-reference type"
         elsif Strict_Alignment (GT)   then "& with aliased part"
         elsif Only_Special            then "" else "&");

   begin
      if Ekind (E) in E_Component | E_Discriminant then
         return Error_Str;
      elsif Is_Type (E) and then By_Ref_Type then
         return " by-reference type &";
      else
         return "&";
      end if;

   end Field_Error_Msg;

   ---------------
   -- Build_Min --
   ---------------

   function Build_Min (V1, V2 : IDS; Name : String := "") return IDS is
     (if   Is_Const (V1) and then Is_Const (V2)
      then (False, Build_Min (V1.Value, V2.Value, Name)) else Var_IDS);

   ---------------
   -- Build_Max --
   ---------------

   function Build_Max (V1, V2 : IDS; Name : String := "") return IDS is
     (if   Is_Const (V1) and then Is_Const (V2)
      then (False, Build_Max (V1.Value, V2.Value, Name)) else Var_IDS);

   ---------------
   -- Emit_Expr --
   ---------------

   function Emit_Expr (N : N_Subexpr_Id; LHS : IDS := No_IDS) return IDS is
      pragma Unreferenced (LHS);
   begin
      return (if   Is_No_Elab_Needed (N)
              then From_Const (Emit_Expression (N)) else Var_IDS);
   end Emit_Expr;

   ---------------------------------
   -- GL_Value_To_Node_Ref_Or_Val --
   ---------------------------------

   function GL_Value_To_Node_Ref_Or_Val (V : GL_Value) return Node_Ref_Or_Val
   is
      Ret : constant Uint := +V;

   begin
      return (if   Ret < 0 then Create_Node (Negate_Expr, UI_Negate (Ret))
              else Ret);
   end GL_Value_To_Node_Ref_Or_Val;

   ---------------------
   -- Annotated_Value --
   ---------------------

   function Annotated_Value (V : BA_Data) return Node_Ref_Or_Val is
   begin
      --  If this isn't valid, return an invalid value

      if No (V) then
         return No_Uint;

      --  If we already have a Node_Ref, return it.

      elsif Present (V.T_Value) then
         return V.T_Value;

      --  Otherwise, we have a constant. If negative, make a Negate_Expr.

      else
         return GL_Value_To_Node_Ref_Or_Val (V.C_Value);
      end if;
   end Annotated_Value;

   ---------------------------
   -- Annotated_Object_Size --
   ---------------------------

   function Annotated_Object_Size
     (GT       : GL_Type;
      Do_Align : Boolean := False;
      Want_Max : Boolean := True) return Node_Ref_Or_Val
   is
      Use_Max       : constant Boolean :=
        Is_Unconstrained_Record (GT) or else Ekind (GT) = E_Array_Subtype;
      Size          : constant BA_Data :=
        Get_Type_Size (GT, Max_Size => Use_Max and then Want_Max);
      Align : constant BA_Data :=
        Const (if   Do_Align
               then Get_Type_Alignment (GT, Use_Specified => False)
               else UBPU);

   begin
      --  We need to return a size that's a muliple of the alignment or at
      --  least aligned to a byte boundary. It's possible that it already
      --  is and we can't detect it. This isn't a problem for constant
      --  sizes, but may cause an extra computation for dynamic sizes. For
      --  now, at least, we won't worry about that.

      return Annotated_Value
        (Build_And (Size + Align - Const (1), -Align));
   end Annotated_Object_Size;

   ----------
   -- Unop --
   ----------

   function Unop
     (V    : BA_Data;
      F    : Unop_Access;
      C    : TCode;
      Name : String := "") return BA_Data
   is
      Result   : GL_Value;

   begin
      --  If we don't have an input, propagate that to the output.

      if No (V) then
         return V;

      --  If we have a constant, perform the operation on the constant and
      --  return it unless it overflowed.

      elsif Is_Const (V) then
         Result := F (V.C_Value, Name);
         return (if   Overflowed (Result) or else Is_Undef (Result)
                 then No_BA else (False, Result, No_Uint));

      --  Otherwise, create a new representation tree node

      else
         return (False, No_GL_Value, Create_Node (C, V.T_Value));
      end if;

   end Unop;

   -----------
   -- Binop --
   -----------

   function Binop
     (LHS, RHS : BA_Data;
      F        : Binop_Access;
      C        : TCode;
      Name     : String := "") return BA_Data
   is
      Result         : GL_Value;
      LHS_Op, RHS_Op : Node_Ref_Or_Val;

   begin
      --  If both are constants, do the operation as a constant and return
      --  that value unless it overflows. If it overflows, return a Uint.

      if Is_Const (LHS) and then Is_Const (RHS) then
         Result := F (LHS.C_Value, RHS.C_Value, Name);
         return (if    Overflowed (Result)
                 then  (False, No_GL_Value,
                        GL_Value_To_Node_Ref_Or_Val (LHS.C_Value)) *
                       (False, No_GL_Value,
                        GL_Value_To_Node_Ref_Or_Val (RHS.C_Value))
                 elsif Is_Undef (Result)
                 then  No_BA else (False, Result, No_Uint));
      end if;

      --  Otherwise, get our two operands as a node reference or Uint

      LHS_Op :=  Annotated_Value (LHS);
      RHS_Op :=  Annotated_Value (RHS);

      --  If either isn't valid, return invalid

      if No (LHS_Op) or else No (RHS_Op) then
         return No_BA;

      --  Otherwise build and return a node. If there's a constant, it
      --  should be in the RHS.

      else
         return (False, No_GL_Value,
                 Create_Node (C, (if   Is_Static_SO_Ref (LHS_Op)
                                  then RHS_Op else LHS_Op),
                              (if   Is_Static_SO_Ref (LHS_Op)
                               then LHS_Op else RHS_Op)));
      end if;

   end Binop;

   -----------
   -- I_Cmp --
   -----------

   function I_Cmp
     (Op       : Int_Predicate_T;
      LHS, RHS : BA_Data;
      Name     : String := "") return BA_Data
   is
      type C_Map is array (Int_Predicate_T range <>) of TCode;
      Codes          : constant C_Map :=
        (Int_EQ => Eq_Expr, Int_NE => Ne_Expr,
         Int_UGT | Int_SGT => Gt_Expr, Int_UGE | Int_SGE => Ge_Expr,
         Int_ULT | Int_SLT => Lt_Expr, Int_ULE | Int_SLE => Le_Expr);
      LHS_Op, RHS_Op : Node_Ref_Or_Val;

   begin
      --  If both are constants, do the operation as a constant and return
      --  that value.

      if Is_Const (LHS) and then Is_Const (RHS) then
         return (False, I_Cmp (Op, LHS.C_Value, RHS.C_Value, Name), No_Uint);
      end if;

      --  Otherwise, get our two operands as a node reference or Uint

      LHS_Op :=  Annotated_Value (LHS);
      RHS_Op :=  Annotated_Value (RHS);

      --  If either isn't valid, return invalid

      if No (LHS_Op) or else No (RHS_Op) then
         return No_BA;

      --  Otherwise, build and return a node

      else
         return (False, No_GL_Value, Create_Node (Codes (Op), LHS_Op, RHS_Op));
      end if;

   end I_Cmp;

   ---------------
   -- Build_Min --
   ---------------

   function Build_Min (V1, V2 : BA_Data; Name : String := "") return BA_Data is
     (Binop (V1, V2, Build_Min'Access, Min_Expr, Name));

   ---------------
   -- Build_Max --
   ---------------

   function Build_Max (V1, V2 : BA_Data; Name : String := "") return BA_Data is
     (Binop (V1, V2, Build_Max'Access, Max_Expr, Name));

   ------------------
   -- Build_Select --
   ------------------

   function Build_Select
     (V_If, V_Then, V_Else : BA_Data; Name : String := "") return BA_Data
   is
      pragma Unreferenced (Name);
      If_Op, Then_Op, Else_Op : Node_Ref_Or_Val;

   begin
      --  If the first is a constant, return the appropriate leg

      if Is_Const (V_If) then
         return (if Is_Const_0 (V_If) then V_Else else V_Then);
      end if;

      --  Otherwise, get our operands as a node reference or Uint

      If_Op   :=  Annotated_Value (V_If);
      Then_Op :=  Annotated_Value (V_Then);
      Else_Op :=  Annotated_Value (V_Else);

      --  If any isn't valid, return invalid

      if No (If_Op) or else No (Then_Op) or else No (Else_Op) then
         return No_BA;

      --  Otherwise, build and return a node

      else
         return (False, No_GL_Value,
                 Create_Node (Cond_Expr, If_Op, Then_Op, Else_Op));
      end if;

   end Build_Select;

   ----------------
   -- _Emit_Expr --
   ----------------

   function Emit_Expr
     (N : N_Subexpr_Id; LHS : BA_Data := No_BA) return BA_Data
   is
      pragma Unreferenced (LHS);
      SO_Info : Dynamic_SO_Ref := Get_SO_Ref (N);

   begin
      --  If we didn't already get an SO_Ref for this expression, get one

      if No (SO_Info) then
         --  If this expression contains a discriminant, build tree nodes
         --  corresponding to that discriminant. If we have an unsupported
         --  node, return no value.

         if Contains_Discriminant (N) then
            declare
               Result   : BA_Data := No_BA;
               Attr     : Attribute_Id;
               RHS, LHS : BA_Data;

            begin
               case Nkind (N) is
                  when N_Identifier =>
                     if  Ekind (Entity (N)) = E_Discriminant then
                        SO_Info := Create_Discrim_Ref (Entity (N));
                     end if;

                  when N_Attribute_Reference =>

                     --  The only ones we support are 'Range_Length,
                     --  'Min, and 'Max

                     Attr := Get_Attribute_Id (Attribute_Name (N));

                     if Attr = Attribute_Range_Length
                       and then Is_Scalar_Type (Full_Etype (Prefix (N)))
                     then
                        declare
                           PT : constant Type_Kind_Id :=
                             Full_Etype (Prefix (N));
                           LB : constant N_Subexpr_Id := Type_Low_Bound  (PT);
                           HB : constant N_Subexpr_Id := Type_High_Bound (PT);

                        begin

                           LHS    := Emit_Expr (LB);
                           RHS    := Emit_Expr (HB);
                           Result := Bounds_To_Length (LHS, RHS,
                                                       Full_GL_Type (N));
                        end;

                     elsif Attr in Attribute_Min | Attribute_Max then
                        LHS    := Emit_Expr (First (Expressions (N)));
                        RHS    := Emit_Expr (Last  (Expressions (N)));
                        Result := (if   Attr = Attribute_Min
                                   then Build_Min (LHS, RHS)
                                   else Build_Max (LHS, RHS));
                     end if;

                  when N_Op_Minus =>
                     Result := Neg (Emit_Expr (Right_Opnd (N)));

                  when N_Op_Plus =>
                     Result := Emit_Expr (Right_Opnd (N));

                  when N_Op_Add =>
                     LHS    := Emit_Expr (Left_Opnd  (N));
                     RHS    := Emit_Expr (Right_Opnd (N));
                     Result := LHS + RHS;

                  when N_Op_Subtract =>
                     LHS    := Emit_Expr (Left_Opnd  (N));
                     RHS    := Emit_Expr (Right_Opnd (N));
                     Result := LHS - RHS;

                  when N_Op_Multiply =>
                     LHS    := Emit_Expr (Left_Opnd  (N));
                     RHS    := Emit_Expr (Right_Opnd (N));
                     Result := LHS * RHS;

                  when N_Op_Divide =>
                     LHS    := Emit_Expr (Left_Opnd  (N));
                     RHS    := Emit_Expr (Right_Opnd (N));
                     Result := (if   Is_Unsigned_Type (Full_Etype (N))
                                then U_Div (LHS, RHS)
                                else LHS / RHS);

                  when N_Type_Conversion | N_Unchecked_Type_Conversion =>
                     Result := Emit_Convert (Expression (N), Full_GL_Type (N));

                  when others =>
                     null;
               end case;

               if Present (Result) then
                  SO_Info := Annotated_Value (Result);
               end if;
            end;

         --  Otherwise, see if this is a constant

         elsif Is_No_Elab_Needed (N) then
            declare
               Result : constant GL_Value := Emit_Expression (N);

            begin
               if not Overflowed (Result) and then not Is_Undef (Result) then
                  return (False, Emit_Expression (N), No_Uint);
               else
                  SO_Info :=
                    Create_Node (Dynamic_Val, +Var_Idx_For_BA);
                  Var_Idx_For_BA := Var_Idx_For_BA + 1;
               end if;
            end;

         else
            SO_Info := Create_Node (Dynamic_Val, +Var_Idx_For_BA);
            Var_Idx_For_BA := Var_Idx_For_BA + 1;
         end if;

         --  Save the computed value, if any

         if Present (SO_Info) then
            Set_SO_Ref (N, SO_Info);
         end if;
      end if;

      --  And now return the value

      return (No (SO_Info), No_GL_Value, SO_Info);
   end Emit_Expr;

   ------------------
   -- Dump_BA_Data --
   ------------------

   procedure Dump_BA_Data (V : BA_Data) is
   begin
      if No (V) then
         Write_Line ("None");
      elsif Is_Const (V) then
         Dump_LLVM_Value (+V.C_Value);
      else
         lgx (V.T_Value);
      end if;
   end  Dump_BA_Data;

end GNATLLVM.Types;
