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

with Errout;   use Errout;
with Get_Targ; use Get_Targ;
with Nlists;   use Nlists;
with Output;   use Output;
with Restrict; use Restrict;
with Snames;   use Snames;
with Table;    use Table;

with GNATLLVM.Arrays;         use GNATLLVM.Arrays;
with GNATLLVM.Blocks;         use GNATLLVM.Blocks;
with GNATLLVM.Compile;        use GNATLLVM.Compile;
with GNATLLVM.Conversions;    use GNATLLVM.Conversions;
with GNATLLVM.Exprs;          use GNATLLVM.Exprs;
with GNATLLVM.GLType;         use GNATLLVM.GLType;
with GNATLLVM.Records;        use GNATLLVM.Records;
with GNATLLVM.Subprograms;    use GNATLLVM.Subprograms;
with GNATLLVM.Types.Create;   use GNATLLVM.Types.Create;
with GNATLLVM.Variables;      use GNATLLVM.Variables;
with GNATLLVM.Wrapper;        use GNATLLVM.Wrapper;

package body GNATLLVM.Types is

   --  We save pairs of GNAT types and LLVM Value_T for each level of
   --  processing of an Emit_LValue so we can find it if we have a
   --  self-referential item (a discriminated record).

   package LValue_Pair_Table is new Table.Table
     (Table_Component_Type => GL_Value,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "LValue_Pair_Table");
   --  Table of intermediate results for Emit_LValue

   LValue_Pair_First : Nat := 1;
   --  The current first entry in the above table.  See the below table.

   --  In the process of computing an LValue, we may need to compute
   --  another expression, e.g., an index or a bound, which may, in turn,
   --  compute another LValue.  So we need to have a stack to save and restore
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
   --  include the alignment of the bounds in some array cases.  It also
   --  may take into account the alignment of E, if present.

   function Move_Into_Memory
     (Temp     : GL_Value;
      V        : GL_Value;
      Expr     : Node_Id;
      GT       : GL_Type;
      Alloc_GT : GL_Type) return GL_Value
     with Pre  => Present (Temp) and then Present (GT)
                  and then Present (Alloc_GT),
          Post => Is_Access_Type (Move_Into_Memory'Result);
   --  Temp is memory that was recently allocated.  Move V, if Present, or
   --  the evaluation of Expr if Present and V isn't, into that allocated
   --  memory and return the allocated memory as a reference to type GT
   --  This is used by both type of memory allocators.  Temp can be of any
   --  type, either an integer or pointer to anything.  Alloc_GT is the
   --  type that was used to allocate the memory.

   function GT_To_Use (GT, Alloc_GT : GL_Type) return GL_Type is
     ((if   Is_Unconstrained_Type (Alloc_GT)
            and then not Is_Unconstrained_Type (GT)
            and then not Is_Tagged_Type (GT)
       then GT else Alloc_GT))
     with Pre  => Present (GT) and then Present (Alloc_GT),
          Post => GT_To_Use'Result in GT | Alloc_GT;
   --  When we have a GT for an object and a GT to use for allocating the
   --  object, return the one we're to use.  We normally want to use
   --  Alloc_GT, but an exception is if it's constrained and GT isn't
   --  unless this is a tagged type.

   --  We put the function used to compute sizes into a generic so that we
   --  can instantiate it using various types of sizing.  The most common
   --  case is an actual size computation, where we produce a GL_Value.
   --  But we may also instantiate this package to generate the structure
   --  needed for back-annotation.

   generic
      type Result is private;
      Empty_Result : Result;
      with function Sz_From_Const (V : GL_Value) return Result;
      with function Sz_Record_Type_Size
        (TE         : Entity_Id;
         V          : GL_Value;
         Max_Size   : Boolean := False;
         No_Padding : Boolean := False) return Result;
      with function Sz_Unc_Array_Type_Size
        (TE         : Entity_Id;
         V          : GL_Value;
         Max_Size   : Boolean := False) return Result;
      with function Sz_Array_Type_Size
        (TE         : Entity_Id;
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
     (if Is_A_Const_Int (V) then (False, V) else Var_IDS)
     with Pre => Is_Constant (V), Post => Present (From_Const'Result);
   --  V is a constant.  If it's a constant integer, return that value.
   --  Otherwise, don't treat it as a constant.

   -------------
   -- Convert --
   -------------

   function Convert
     (V              : IDS;
      GT             : GL_Type;
      Float_Truncate : Boolean := False;
      Is_Unchecked   : Boolean := False) return IDS
   is
     (if   Is_Const (V)
      then (False, Convert (V.Value, GT, Float_Truncate, Is_Unchecked))
      else Var_IDS);

   -------------
   -- Convert --
   -------------

   function Convert
     (V              : BA_Data;
      GT             : GL_Type;
      Float_Truncate : Boolean := False;
      Is_Unchecked   : Boolean := False) return BA_Data
   is
     (if   Is_Const (V)
      then (False, Convert (V.C_Value, GT, Float_Truncate, Is_Unchecked),
            No_Uint)
      else V);

   ----------------
   -- From_Const --
   ----------------

   function From_Const (V : GL_Value) return BA_Data is
     (if   Is_A_Const_Int (V) then (False, V, No_Uint) else No_BA)
     with Pre => Is_Constant (V);
   --  Likewise, for back-annotation

   ----------------------
   --  Is_Dynamic_Size --
   ----------------------

   function Is_Dynamic_Size
     (GT             : GL_Type;
      Max_Size       : Boolean := False;
      Allow_Overflow : Boolean := False) return Boolean
   is
      Size : IDS;
   begin
      --  If this is of elementary type, it's not of dynamic size.  We have
      --  to do this test not just for efficiency, but also to avoid
      --  infinite recursion if we are passed Size_Type.

      if Is_Elementary_Type (GT) then
         return False;
      end if;

      --  Otherwise get the size for our purposes.  If not a constant or not
      --  something LLVM can use natively as an array bound, this is dynamic.
      --  But we conservatively test for the range of Int to be consistent
      --  with how we create arrays.

      Size := Get_Type_Size (GT, No_GL_Value, Max_Size);
      return not Is_Const (Size)
        or else (not Allow_Overflow
                   and then (Const_Int (Size) < 0
                               or else Const_Int (Size) > LLI (Int'Last)));

   end Is_Dynamic_Size;

   ----------------------
   -- Is_Loadable_Type --
   ----------------------
   function Is_Loadable_Type (GT : GL_Type) return Boolean is
     (not Is_Nonnative_Type (GT) and then not Is_Truncated_GL_Type (GT)
        and then Is_Loadable_Type (Type_Of (GT)));

   -----------------------
   -- Build_Struct_Type --
   -----------------------

   function Build_Struct_Type
     (Types : Type_Array; Packed : Boolean := False) return Type_T is
   begin
      return Struct_Type_In_Context
        (Context, Types'Address, Types'Length, Packed);
   end Build_Struct_Type;

   ----------------------
   -- Is_Loadable_Type --
   ----------------------

   function Is_Loadable_Type (T : Type_T) return Boolean is
   begin
      --  A type isn't loadable if it's too large

      if ULL'(Get_Type_Size (T)) / Get_Type_Alignment (T) > Max_Load_Size then
         return False;

      --  If a structure, it isn't loadable if any component isn't

      elsif Get_Type_Kind (T) = Struct_Type_Kind
        and then Count_Struct_Element_Types (T) /= 0
      then
         for J in 0 .. Count_Struct_Element_Types (T) - 1 loop
            if not Is_Loadable_Type (Struct_Get_Type_At_Index (T, J)) then
               return False;
            end if;
         end loop;

      --  Likewise for an array (its component might be an array that has
      --  a non-loadable type as a component).

      elsif Get_Type_Kind (T) = Array_Type_Kind
        and then not Is_Loadable_Type (Get_Element_Type (T))
      then
         return False;
      end if;

      --  If nothing prevented this from being a loadable type, it is

      return True;
   end Is_Loadable_Type;

   ----------------------
   -- Push_LValue_List --
   ----------------------

   procedure Push_LValue_List is
   begin
      LValue_Stack.Append (LValue_Pair_First);
      LValue_Pair_First := LValue_Pair_Table.Last + 1;
   end Push_LValue_List;

   ---------------------
   -- Pop_LValue_List --
   ---------------------

   procedure Pop_LValue_List is
   begin
      LValue_Pair_Table.Set_Last (LValue_Pair_First - 1);
      LValue_Pair_First := LValue_Stack.Table (LValue_Stack.Last);
      LValue_Stack.Decrement_Last;
   end Pop_LValue_List;

   ------------------------
   --  Clear_LValue_List --
   ------------------------

   procedure Clear_LValue_List is
   begin
      LValue_Pair_Table.Set_Last (LValue_Pair_First - 1);
   end Clear_LValue_List;

   -------------------------
   --  Add_To_LValue_List --
   -------------------------

   procedure Add_To_LValue_List (V : GL_Value) is
   begin
      --  Only add to the LValue list if this is a record type.  We might
      --  be tempted to do this only if the type has discriminants, but
      --  that doesn't work because a parent might and it's not worth
      --  checking.

      if Is_Record_Type (Related_Type (V)) and then Disable_LV_Append = 0 then
         LValue_Pair_Table.Append (V);
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

   ------------------------
   -- Get_Matching_Value --
   ------------------------

   function Get_Matching_Value (TE : Entity_Id) return GL_Value is
   begin
      --  Check in the opposite order of what we push.  We may, for example
      --  be finding the size of an object of that size, in which case the
      --  object will have been added last.

      for J in reverse LValue_Pair_First .. LValue_Pair_Table.Last loop
         if Is_Parent_Of (TE,
                          Full_Etype (Related_Type
                                        (LValue_Pair_Table.Table (J))))
           or else Is_Parent_Of (Full_Etype (Related_Type
                                               (LValue_Pair_Table.Table (J))),
                                 TE)
         then
            --  ?? It would be more efficient to not convert to a reference
            --  here, but that might be quite a lot of work (see 8802-007).

            return Convert_Ref (Get (LValue_Pair_Table.Table (J),
                                     Any_Reference),
                                Default_GL_Type (TE));
         end if;
      end loop;

      --  Should never get here and postcondition verifies

      return No_GL_Value;
   end Get_Matching_Value;

   ------------------------
   -- Ultimate_Base_Type --
   ------------------------

   function Ultimate_Base_Type (TE : Entity_Id) return Entity_Id is
   begin
      return Typ : Entity_Id := TE do
         while Full_Etype (Typ) /= Typ loop
            Typ := Full_Etype (Typ);
         end loop;
      end return;
   end Ultimate_Base_Type;

   ----------------------
   -- Get_Fullest_View --
   ----------------------

   function Get_Fullest_View
     (TE : Entity_Id; Include_PAT : Boolean := True) return Entity_Id is
   begin
      --  Strictly speaking, the recursion below isn't necessary, but
      --  it's both simplest and safest.

      case Ekind (TE) is
         when Incomplete_Kind =>
            if From_Limited_With (TE) then
               return Get_Fullest_View (Non_Limited_View (TE), Include_PAT);
            elsif Present (Full_View (TE)) then
               return Get_Fullest_View (Full_View (TE), Include_PAT);
            elsif Ekind (TE) = E_Incomplete_Subtype then
               return Get_Fullest_View (Etype (TE));
            end if;

         when Private_Kind =>
            if Present (Underlying_Full_View (TE)) then
               return
                 Get_Fullest_View (Underlying_Full_View (TE), Include_PAT);
            elsif Present (Full_View (TE)) then
               return Get_Fullest_View (Full_View (TE), Include_PAT);
            else
               return Get_Fullest_View (Etype (TE), Include_PAT);
            end if;

         when Array_Kind =>
            if Include_PAT and then Present (Packed_Array_Impl_Type (TE)) then
               return Get_Fullest_View (Packed_Array_Impl_Type (TE));
            end if;

         when E_Record_Subtype =>
            if Present (Cloned_Subtype (TE)) then
               return Get_Fullest_View (Cloned_Subtype (TE), Include_PAT);
            end if;

         when E_Class_Wide_Type =>
            return Get_Fullest_View (Root_Type (TE), Include_PAT);

         when  E_Class_Wide_Subtype =>
            if Present (Equivalent_Type (TE)) then
               return Get_Fullest_View (Equivalent_Type (TE), Include_PAT);
            elsif Present (Cloned_Subtype (TE)) then
               return Get_Fullest_View (Cloned_Subtype (TE), Include_PAT);
            end if;

         when E_Protected_Type | E_Protected_Subtype
            | E_Task_Type |  E_Task_Subtype =>
            if Present (Corresponding_Record_Type (TE)) then
               return Get_Fullest_View (Corresponding_Record_Type (TE),
                                        Include_PAT);
            end if;

         when E_Access_Protected_Subprogram_Type
            | E_Anonymous_Access_Protected_Subprogram_Type =>
            if Present (Equivalent_Type (TE)) then
               return Get_Fullest_View (Equivalent_Type (TE), Include_PAT);
            end if;

         when E_Access_Subtype =>
            return Get_Fullest_View (Base_Type (TE), Include_PAT);

         when others =>
            null;
      end case;

      return TE;
   end Get_Fullest_View;

   -------------
   -- Type_Of --
   -------------

   function Type_Of (TE : Entity_Id) return Type_T is
      GT    : GL_Type;

   begin
      --  Before we do anything, see if this isn't a base type and process
      --  the base type if so.  Copy sizes from the base type if a size
      --  clause was present and the corresponding value hasn't already
      --  been set.

      if not Is_Full_Base_Type (TE) then
         declare
            BT : constant Entity_Id := Full_Base_Type (TE);

         begin
            Discard (Type_Of (BT));

            if Has_Size_Clause (BT) and then Unknown_RM_Size (TE) then
               Set_RM_Size (TE, RM_Size (BT));
            end if;

            if Has_Object_Size_Clause (BT) and then Unknown_Esize (TE) then
               Set_Esize (TE, Esize (BT));
            end if;
         end;
      end if;

      --  Next see if we already have a suitable type

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
   -- Bounds_From_Type --
   ----------------------

   procedure Bounds_From_Type (GT : GL_Type; Low, High : out GL_Value)
   is
      SRange : constant Node_Id := Scalar_Range (GT);

   begin
      pragma Assert (Nkind_In (SRange, N_Range,
                               N_Signed_Integer_Type_Definition));

      Low  := Emit_Convert_Value (Low_Bound (SRange), GT);
      High := Emit_Convert_Value (High_Bound (SRange), GT);

   end Bounds_From_Type;

   ----------------------
   -- Move_Into_Memory --
   ----------------------

   function Move_Into_Memory
     (Temp     : GL_Value;
      V        : GL_Value;
      Expr     : Node_Id;
      GT       : GL_Type;
      Alloc_GT : GL_Type) return GL_Value
   is
      R        : constant GL_Relationship := Relationship_For_Alloc (GT);
      New_Expr : constant Node_Id         := Strip_Complex_Conversions (Expr);
      Mem_GT   : constant GL_Type         := GT_To_Use (GT, Alloc_GT);
      Memory   : GL_Value                 :=
        (if   Is_Access_Type (Temp)
         then Ptr_To_Relationship (Temp, Mem_GT, R)
         else Int_To_Relationship (Temp, Mem_GT, R));
      New_V    : GL_Value                 :=
        (if    Present (V) then V
         elsif Present (New_Expr) then Emit (New_Expr, LHS => Memory)
         else  No_GL_Value);

   begin
      --  If this is to get bounds and data and we have a value to store
      --  which contains data, convert it to bounds and data and store it.
      --  Otherwise, we have two cases, depending on the reason that we
      --  have bounds because Emit_Assignment only can handle the
      --  nominal type for alias to unconstrained case.

      if R = Reference_To_Bounds_And_Data then
         if Present (New_V) and then Is_Data (New_V) then
            New_V  := Get (New_V, Bounds_And_Data);
            Memory := Ptr_To_Relationship (Memory, New_V, R);
         else
            if not Is_Constrained (GT) or else No (New_V)
              or else New_V = Memory
              or else (Is_Constr_Subt_For_UN_Aliased (GT) and then
                         not Is_Constr_Subt_For_UN_Aliased (Alloc_GT))
            then
               Store (Get_Array_Bounds (GT, Alloc_GT, New_V),
                      Get (Memory, Reference_To_Bounds));
            end if;
         end if;
      end if;

      --  If we have a value to move into memory, move it

      if Present (New_V) and then New_V /= Memory and then not Is_Undef (New_V)
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
     (GT        : GL_Type;
      Alloc_GT  : GL_Type;
      N         : Node_Id;
      V         : GL_Value  := No_GL_Value;
      Expr      : Node_Id   := Empty;
      Def_Ident : Entity_Id := Empty;
      Name      : String    := "";
      Max_Size  : Boolean   := False) return GL_Value
   is
      Max_Alloc  : constant ULL     := 10_000_000;
      Align      : constant Nat     :=
        Get_Alloc_Alignment (GT, Alloc_GT, Def_Ident);
      Overalign  : constant Boolean := Align > (Get_Stack_Alignment * BPU);
      Value      : GL_Value         := V;
      Element_GT : GL_Type;
      Num_Elts   : GL_Value;
      Result     : GL_Value;

   begin
      --  We have three cases.  If the object has a native type and we're
      --  not trying to over-align it, we just do the alloca and that's
      --  all.  Test for the size being other an overflow or an undef,
      --  which we'll assume was likely caused by an overflow.

      if not Is_Nonnative_Type (Alloc_GT) and then not Overalign then
         if Do_Stack_Check
           and then Get_Type_Size (Type_Of (Alloc_GT)) > Max_Alloc * ULL (BPU)
         then
            Emit_Raise_Call (N, SE_Object_Too_Large);
            return Get_Undef_Ref (GT);
         else
            declare
               Align_GT : constant GL_Type :=
                 (if   GT_Alignment (Alloc_GT) >= Align then Alloc_GT
                  else Make_GT_Alternative (Alloc_GT, N,
                                            Align => UI_From_Int (Align)));

            begin
               return Move_Into_Memory (Alloca (Align_GT, Def_Ident, Name),
                                        Value, Expr, GT, Alloc_GT);
            end;
         end if;
      end if;

      --  Otherwise, we probably have to do some sort of dynamic
      --  allocation.  If this is an array of a component that's not of
      --  dynamic size that we're not overaligning, we can allocate an
      --  array of the component type corresponding to the array type and
      --  cast it to a pointer to the actual type.  If not, we have to
      --  allocate it as an array of bytes.  We must use an array of bytes
      --  if we have to include bounds.  If this is an unconstrained array,
      --  we need to find the bounds, so evaluate Expr if Present and
      --  there's no Value.

      if Is_Unconstrained_Array (Alloc_GT) and then No (Value)
        and then Present (Expr)
      then
         Value := Emit (Expr);
      end if;

      if Is_Array_Type (Alloc_GT)
        and then Is_Native_Component_GT (Full_Component_GL_Type (Alloc_GT))
        and then not Is_Constr_Subt_For_UN_Aliased (GT)
        and then not Overalign
      then
         Element_GT := Full_Component_GL_Type (Alloc_GT);
         Num_Elts   := Get_Array_Elements (Value, Full_Etype (Alloc_GT));
      else
         Element_GT := SSI_GL_Type;
         Num_Elts   :=
           To_Bytes (Get_Alloc_Size (GT, Alloc_GT, Value, Max_Size));
      end if;

      --  Handle overalignment by adding the alignment to the size

      if Overalign then
         Num_Elts := Num_Elts + To_Bytes (Align);
      end if;

      --  Check that we aren't trying to allocate too much memory.  Raise
      --  Storage_Error if so.  We don't try to support local exception
      --  labels and -fstack-check at the same time.  The divide below
      --  will constant-fold, but make sure we aren't dividing by zero.

      if Do_Stack_Check
        and then Get_Type_Size (Element_GT) /= Size_Const_Null
      then
         Emit_Raise_Call_If (I_Cmp (Int_UGT, Num_Elts,
                                    U_Div (Size_Const_Int (Max_Alloc),
                                           Get_Type_Size (Element_GT))),
                             N, SE_Object_Too_Large);
      end if;

      --  If the number of elements overflowed, raise Storage_Error.  Bt
      --  check for the pathalogical case of an array of zero-sized elements.

      if Overflowed (Num_Elts) or else Is_Undef (Num_Elts) then
         if Get_Type_Size (Element_GT) = 0 then
            Num_Elts := Size_Const_Int (Uint_1);
         else
            Emit_Raise_Call (N, SE_Object_Too_Large);
            return Get_Undef_Ref (GT);
         end if;
      end if;

      --  Now allocate the object, align if necessary, and then move
      --  any data into it.

      Result := Array_Alloca (Element_GT, Num_Elts, Def_Ident,
                              (if Overalign then "%%" else Name));
      if Overalign then
         Result := Ptr_To_Int (Result, Size_GL_Type);
         Result := Align_To   (Result, Get_Stack_Alignment, To_Bytes (Align));
         Result := Int_To_Ptr (Result, A_Char_GL_Type);
         Set_Value_Name (Result, Get_Alloca_Name (Def_Ident, Name));
      end if;

      return Move_Into_Memory (Result, Value, Expr, GT, Alloc_GT);

   end Allocate_For_Type;

   ----------------------------
   -- Heap_Allocate_For_Type --
   ----------------------------

   function Heap_Allocate_For_Type
     (GT        : GL_Type;
      Alloc_GT  : GL_Type;
      V         : GL_Value  := No_GL_Value;
      N         : Node_Id   := Empty;
      Expr      : Node_Id   := Empty;
      Proc      : Entity_Id := Empty;
      Pool      : Entity_Id := Empty;
      Def_Ident : Entity_Id := Empty;
      Max_Size  : Boolean   := False) return GL_Value
   is
      Value   : constant GL_Value :=
        (if    Present (V) then V
         elsif Is_Self_Referential_Type (Alloc_GT) and then Present (Expr)
         then  Emit (Expr) else No_GL_Value);
      Size    : constant GL_Value :=
        Get_Alloc_Size (GT, Alloc_GT, Value, Max_Size);
      Align   : constant Nat      :=
        Get_Alloc_Alignment (GT, Alloc_GT, Def_Ident);
      Align_V : constant GL_Value := Size_Const_Int (ULL (Align));
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
         Emit_Raise_Call (N, SE_Object_Too_Large);
         return Get_Undef_Ref (GT);
      end if;

      --  If no procedure was specified, use the default memory allocation
      --  function, where we just pass a size.  But we can only do this
      --  directly if the requested alignment is a constand and no larger
      --  than the system allocator alignment.

      if No (Proc) and then Align <= Get_System_Allocator_Alignment * BPU then
         Result := Call (Get_Default_Alloc_Fn, A_Char_GL_Type,
                         (1 => To_Bytes (Size)));

      --  Otherwise, if we can use the default memory allocation
      --  function but have to overalign, increase the size by both
      --  the alignment and the space needed for a pointer, align the
      --  result (leaving space for the pointer) and store the obtained
      --  address immediately before the value.

      elsif No (Proc) then
         declare
            Ptr_Size   : constant GL_Value := Get_Type_Size (A_Char_GL_Type);
            Total_Size : constant GL_Value := Size + Align + Ptr_Size;
            Alloc      : constant GL_Value :=
              Call (Get_Default_Alloc_Fn, A_Char_GL_Type,
                    (1 => To_Bytes (Total_Size)));
            Alloc_Int  : constant GL_Value := Ptr_To_Int (Alloc, Size_GL_Type);
            Aligned    : constant GL_Value :=
              Align_To (Alloc_Int + To_Bytes (Ptr_Size),
                        Get_System_Allocator_Alignment, To_Bytes (Align));
            Ptr_Loc    : constant GL_Value := Aligned - To_Bytes (Ptr_Size);

         begin
            --  It may have been the case that Size didn't oveflow until
            --  we did the computation above.  So check again.

            if Overflowed (Total_Size) then
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
           Call_Alloc (Proc,
                       (1 => Ptr_To_Ref (Emit_Safe_LValue (Pool),
                                         Full_GL_Type (First_Formal (Proc))),
                        2 => To_Bytes (Size),
                        3 => To_Bytes (Align_V)));

      --  Otherwise, this is the secondary stack and we just call with size

      else
         Result := Call_Alloc (Proc, (1 => To_Bytes (Size)));
      end if;

      --  If we're doing this for an unconstrained array, we have the pointer
      --  to the raw array, not a fat pointer.

      return Move_Into_Memory (Result, Value, Expr, GT, Alloc_GT);
   end Heap_Allocate_For_Type;

   ---------------------
   -- Heap_Deallocate --
   ---------------------

   procedure Heap_Deallocate
     (V        : GL_Value;
      Desig_GT : GL_Type;
      Proc     : Entity_Id;
      Pool     : Entity_Id)
   is
      Conv_V   : GL_Value := V;
      DT       : GL_Type  := Related_Type (V);
      Alloc_GT : GL_Type  := DT;

   begin
      --  If V is an access type, convert it to a reference to the
      --  underlying data.  We also want to record the actual designated
      --  type in this case since it may contain bound information and
      --  we need to record the bounds as well as their size.

      if Is_Access_Type (V) and then Is_Data (V) then
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
      --  and data.  Otherwise just a Reference.  We'll then either convert
      --  it to a generic pointer or to an integer (System.Address).

      Conv_V := Get (Conv_V, Relationship_For_Alloc (DT));

      declare
         Size    : constant GL_Value :=
           Get_Alloc_Size (DT, Alloc_GT, Conv_V, For_Dealloc => True);
         Align   : constant Nat      :=
           Get_Alloc_Alignment (DT, Alloc_GT, Empty);
         Align_V : constant GL_Value := Size_Const_Int (ULL (Align));

      begin
         --  If no procedure was specified, use the default memory deallocation
         --  procedure, where we just pass a size.  But we can only do this
         --  directly if the requested alignment is a constand and no larger
         --  than the system allocator alignment.

         if No (Proc) and then Align <= Get_System_Allocator_Alignment * BPU
         then
            Call (Get_Default_Free_Fn,
                  (1 => Pointer_Cast (Conv_V, A_Char_GL_Type)));

         --  If we have to use the normal deallocation procedure to
         --  deallocate an overaligned value, the actual address of the
         --  memory to deallocate can be found in front of the value we're
         --  passed.

         elsif No (Proc) then
            declare
               Addr       : constant GL_Value :=
                 Ptr_To_Int (Conv_V, Size_GL_Type);
               Ptr_Size   : constant GL_Value :=
                 Get_Type_Size (A_Char_GL_Type);
               Ptr_Loc    : constant GL_Value := Addr - To_Bytes (Ptr_Size);
               Ptr_Ref    : constant GL_Value :=
                 Int_To_Ref (Ptr_Loc, A_Char_GL_Type);

            begin
               Call (Get_Default_Free_Fn, (1 => Load (Ptr_Ref)));
            end;

         --  If a procedure was specified (meaning that a pool must also
         --  have been specified) and the pool is a record, then it's a
         --  storage pool and we pass the pool, size, and alignment.  Be
         --  sure that we convert the pool to actual type of the formal of
         --  the deallocator function: it may be a derived type.

         elsif Is_Record_Type (Full_Etype (Pool)) then
            Call_Dealloc (Proc,
                          (1 => Ptr_To_Ref (Emit_Safe_LValue (Pool),
                                            Full_GL_Type
                                              (First_Formal (Proc))),
                           2 => Ptr_To_Size_Type (Conv_V),
                           3 => To_Bytes (Size),
                           4 => To_Bytes (Align_V)));

            --  Otherwise, this is the secondary stack and we just call
            --  it with the size.

         else
            Call_Dealloc (Proc, (1 => Ptr_To_Size_Type (Conv_V),
                                 2 => To_Bytes (Size)));
         end if;
      end;
   end Heap_Deallocate;

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
      Align         : constant GL_Value  := GT_Alignment (GT);
      TE            : constant Entity_Id := Full_Etype (GT);

   begin
      --  If there's a known alignment in this GL_Type, use it

      if Present (Align) and then Is_A_Const_Int (Align) then
         return Nat (Get_Const_Int_Value (Align));

      --  If the alignment is specified (or back-annotated) in the tree,
      --  use that value.

      elsif Known_Alignment (TE)  and Use_Specified then
         return UI_To_Int (Alignment (TE)) * BPU;

      --  If it's an array, call the specialized function

      elsif Is_Array_Type (TE) then
         return Get_Array_Type_Alignment (TE);

      --  If a record, call the specialized function

      elsif Is_Record_Type (TE) then
         return Get_Record_Type_Alignment (TE);

      --  If it's a subprogram type, there really isn't an alignment, but
      --  indicate that code can be anywhere.

      elsif Ekind (TE) = E_Subprogram_Type then
         return 1;

      --  Otherwise, it must be an elementary type, so get the LLVM type's
      --  alignment

      else
         return Get_Type_Alignment (Type_Of (TE));

      end if;
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
         --  fixed size.  That's the size we're looking for.

         if Present (V) and then Is_Data (V)
           and then not Use_Max_Size and then not Unpad_Record
         then
            Our_Size := Sz_From_Const (Get_Type_Size (Type_Of (V)));

            --  However, if this is both bounds and data, we have to subtract
            --  the size of the bounds since we define the size of the
            --  type itself to not include the bounds.

            if Relationship (V) = Bounds_And_Data then
               Our_Size :=
                 Our_Size - Sz_From_Const (Get_Bound_Size (Related_Type (V)));
            end if;

            return Our_Size;

         --  If there's a size specified in this GT, that's what we want
         --  unless we aren't to remove padding and this is a record type.

         elsif Present (Size_In_GT) and then not Unpad_Record then
            return Sz_From_Const (Size_In_GT);

         --  If this is a subprogram type, it doesn't have a size

         elsif Ekind (GT) = E_Subprogram_Type then
            return Empty_Result;

         --  If this isn't a non-native type, then the size is the size of the
         --  LLVM type and unless we aren't to remove padding and this is a
         --  record type.

         elsif not Is_Nonnative_Type (GT) and then not Unpad_Record then
            return Sz_From_Const (Get_Type_Size (Type_Of (GT)));

         elsif Is_Record_Type (GT) then
            return Sz_Record_Type_Size (Full_Etype (GT), V,
                                        Max_Size   => Use_Max_Size,
                                        No_Padding => No_Padding
                                          and then not Strict_Alignment (GT));
         elsif Is_Array_Type (GT) and then not Is_Constrained (GT) then
            return Sz_Unc_Array_Type_Size (Full_Etype (GT), V, Use_Max_Size);
         elsif Is_Array_Type (GT) then
            return Sz_Array_Type_Size (Full_Etype (GT), V, Use_Max_Size);
         else
            pragma Assert (False);
            return Empty_Result;
         end if;

      end Get_Type_Size;
   end Size;

   package LLVM_Size is
      new Size (Result                 => GL_Value,
                Empty_Result           => No_GL_Value,
                Sz_From_Const          => From_Const,
                Sz_Record_Type_Size    => Get_Record_Type_Size,
                Sz_Unc_Array_Type_Size => Get_Unc_Array_Type_Size,
                Sz_Array_Type_Size     => Get_Array_Type_Size,
                "-"                    => "-");

   function Get_Type_Size
     (GT         : GL_Type;
      V          : GL_Value := No_GL_Value;
      Max_Size   : Boolean  := False;
      No_Padding : Boolean  := False) return GL_Value
     renames LLVM_Size.Get_Type_Size;

   package IDS_Size is
      new Size (Result                 => IDS,
                Empty_Result           => No_IDS,
                Sz_From_Const          => From_Const,
                Sz_Record_Type_Size    => Get_Record_Type_Size,
                Sz_Unc_Array_Type_Size => Get_Unc_Array_Type_Size,
                Sz_Array_Type_Size     => Get_Array_Type_Size,
                "-"                    => "-");

   function Get_Type_Size
     (GT         : GL_Type;
      V          : GL_Value := No_GL_Value;
      Max_Size   : Boolean  := False;
      No_Padding : Boolean  := False) return IDS
     renames IDS_Size.Get_Type_Size;

   package BA_Size is
      new Size (Result                 => BA_Data,
                Empty_Result           => No_BA,
                Sz_From_Const          => From_Const,
                Sz_Record_Type_Size    => Get_Record_Type_Size,
                Sz_Unc_Array_Type_Size => Get_Unc_Array_Type_Size,
                Sz_Array_Type_Size     => Get_Array_Type_Size,
                "-"                    => "-");

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
                           Get_Type_Alignment (GT),
                           Get_Bound_Alignment (Full_Etype (GT)));
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
         then UI_To_Int (Alignment (E)) * BPU else BPU);
      Bound_Align : constant Nat     :=
        (if   Is_Unconstrained_Array (GT) or else Type_Needs_Bounds (Alloc_GT)
         then Get_Bound_Alignment (Full_Etype (GT)) else BPU);

   begin
      return Nat'Max (Nat'Max (GT_Align, Bound_Align), E_Align);
   end Get_Alloc_Alignment;

   ------------------
   -- Compute_Size --
   ------------------

   function Compute_Size
     (Left_GT, Right_GT       : GL_Type;
      Left_Value, Right_Value : GL_Value) return GL_Value
   is
      LHS_Complex : constant Nat     := Get_Type_Size_Complexity (Left_GT);
      RHS_Complex : constant Nat     := Get_Type_Size_Complexity (Right_GT);
      LHS_Unc     : constant Boolean := Is_Unconstrained_Type (Left_GT);
      RHS_Unc     : constant Boolean := Is_Unconstrained_Type (Right_GT);
      Class_Wide  : constant Boolean :=
        Is_Class_Wide_Equivalent_Type (Left_GT);
      Size_GT     : GL_Type;
      Size_Value  : GL_Value;

   begin
      --  In most cases, the two sizes are equal.  However, we can't verify
      --  that.  In most cases, our goal is to just choose the type whose size
      --  is easiest to compute, either in terms of what we need to do the
      --  computation (favoring unconstrained over constrained) or the
      --  amount of work to compute the type.  There are, however, two
      --  exceptions: if the LHS is a class-wide equivalent type, we must
      --  do the copy using that size.  We check for that first.  Conversely,
      --  if the LHS is an unconstrained record, we must use the size of
      --  the RHS.  This case, however, is covered in our general preference
      --  of unconstrained.

      if Class_Wide then
         Size_GT    := Left_GT;
         Size_Value := Left_Value;

      --  If one size is contrained and the other isn't, use the
      --  constrained size.

      elsif LHS_Unc and then not RHS_Unc then
         Size_GT    := Right_GT;
         Size_Value := Right_Value;
      elsif not LHS_Unc and then RHS_Unc then
         Size_GT    := Left_GT;
         Size_Value := Left_Value;

      --  Use the type of right side unless its complexity is more
      --  than that of the size of the type on the left side.

      elsif RHS_Complex > LHS_Complex or else Class_Wide then
         Size_GT    := Left_GT;
         Size_Value := Left_Value;
      else
         Size_GT    := Right_GT;
         Size_Value := Right_Value;
      end if;

      return Get_Type_Size (Size_GT, Size_Value, No_Padding => not Class_Wide);
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

   function Get_Attribute_From_Annotation (N : Node_Id) return Uint is
      Attr   : constant Attribute_Id := Get_Attribute_Id (Attribute_Name (N));
      TE     : constant Entity_Id    := Full_Etype (Prefix (N));
      E      : constant Entity_Id    :=
        (if    Is_Entity_Name (Prefix (N)) then Entity (Prefix (N))
         elsif Nkind (Prefix (N)) = N_Selected_Component
         then  Entity (Selector_Name (Prefix (N)))
         else  Empty);
      Our_E  : constant Entity_Id    := (if Present (E) then E else TE);
      Ret    : Uint                  := No_Uint;

   begin
      --  We have to be careful here because even though we don't
      --  usually need to evaluate the Prefix to get its size, we are
      --  required to, so it must be static

      if No (E) and then not Is_No_Elab_Needed (Prefix (N)) then
         return No_Uint;
      end if;

      case Attr is
         when Attribute_Object_Size =>
            if Known_Esize (Our_E) then
               Ret := Esize (Our_E);
            end if;

         when Attribute_Size | Attribute_Value_Size =>
            if Known_RM_Size (Our_E) then
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
            return UI_From_GL_Value (Get_Bound_Size (Default_GL_Type (TE)));

         when Attribute_Component_Size =>
            if Known_Component_Size (TE) then
               Ret := Component_Size (TE);
            end if;

         when Attribute_Alignment =>
            if Known_Alignment (Our_E) then
               return Alignment (Our_E);
            end if;

         when Attribute_Position | Attribute_Bit_Position =>

            --  ??? Is this correct that both are in bytes?

            Ret := Component_Bit_Offset (Our_E);
            if Ret /= No_Uint and then Is_Static_SO_Ref (Ret)
              and then Attr = Attribute_Position
            then
               Ret := Ret / BPU;
            end if;

         when Attribute_First_Bit | Attribute_Bit =>
            if Ekind_In (Our_E, E_Discriminant, E_Component)
              and then Known_Normalized_Position (Our_E)
            then
               Ret := Normalized_First_Bit (Our_E);
            end if;

         when Attribute_Last_Bit =>
            if Known_Normalized_First_Bit (Our_E) then
               Ret := Normalized_First_Bit (Our_E);
            end if;
            if Ret /= No_Uint and then Is_Static_SO_Ref (Ret)
              and then Known_Esize (Our_E) and then Is_Static_SO_Ref (Ret)
            then
               Ret := Ret + Esize (Our_E) - 1;
            end if;

         when others =>
            null;
      end case;

      return (if Is_Static_SO_Ref (Ret) then Ret else No_Uint);
   end Get_Attribute_From_Annotation;

   ----------------------------------
   -- Add_Type_Data_To_Instruction --
   ----------------------------------

   procedure Add_Type_Data_To_Instruction
     (Inst : Value_T; V : GL_Value; Special_Atomic : Boolean := False)
   is
      GT   : constant GL_Type    := Related_Type (V);
      TBAA : constant Metadata_T := Get_TBAA (Full_Etype (GT));

   begin
      Set_Volatile  (Inst, Is_Volatile (V));
      Set_Ordering  (Inst,
                     (if   Is_Atomic (V)
                           and then (Special_Atomic
                                       or else (Atomic_Kind
                                                  (Get_Element_Type
                                                     (Type_Of (V)))))
                      then Atomic_Ordering_Sequentially_Consistent
                      else Atomic_Ordering_Not_Atomic));
      Set_Alignment (Inst,
                     unsigned (Nat'(To_Bytes (Get_Type_Alignment (GT)))));

      if Present (TBAA) and then not Universal_Aliasing (GT) then
         Add_TBAA_Access
           (Inst, Create_TBAA_Access_Tag (MD_Builder, TBAA, TBAA, 0));
      end if;
   end Add_Type_Data_To_Instruction;

   ------------------------------
   -- Check_OK_For_Atomic_Type --
   ------------------------------

   procedure Check_OK_For_Atomic_Type
     (GT : GL_Type; E : Entity_Id; Is_Component : Boolean := False)
   is
      T           : constant Type_T := Type_Of (GT);
      Align       : constant Nat    := Get_Type_Alignment (GT);
      Error_Node  : Node_Id         := E;
      Pragma_Node : Node_Id;

   begin
      --  If this is an anonymous base type, nothing to check, the
      --  error will be reported on the source type if need be.

      if not Comes_From_Source (E)

      --  Consider all aligned elementary types as atomic

        or else (Is_Elementary_Type (GT)
                   and then Align >= Get_Type_Alignment (T))

      --  Or if it's a fixed size, the size is equal to the alignment,
      --  and the alignment is less than a word.        --  atomic.

        or else (not Is_Dynamic_Size (GT)
                   and then Align <= Get_Bits_Per_Word
                   and then ULL (Align) = Get_Const_Int_Value_ULL
                   (Get_Type_Size (GT)))
      then
         return;
      end if;

      --  We normally give an error at E, but if there's a relevant
      --  pragma, the error point is there.

      Pragma_Node :=
        Get_Pragma (E, (if   Is_Component then Pragma_Atomic_Components
                        else Pragma_Atomic));
      if Present (Pragma_Node) then
         Error_Node := First (Pragma_Argument_Associations (Pragma_Node));
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

   function Emit_Expr (V : Node_Id; LHS : IDS := No_IDS) return IDS is
      pragma Unreferenced (LHS);
   begin
      return (if   Is_No_Elab_Needed (V)
              then From_Const (Emit_Expression (V)) else Var_IDS);
   end Emit_Expr;

   ---------------------
   -- Annotated_Value --
   ---------------------

   function Annotated_Value (V : BA_Data) return Node_Ref_Or_Val is
      Ret : Uint;

   begin
      --  If this isn't valid, return an invalid value

      if No (V) then
         return No_Uint;

      --  If we already have a Node_Ref, return it.

      elsif V.T_Value /= No_Uint then
         return V.T_Value;

      --  Otherwise, we have a constant.  If negative, make a Negate_Expr.

      else
         Ret := UI_From_GL_Value (V.C_Value);
         return (if   Ret < 0 then Create_Node (Negate_Expr, UI_Negate (Ret))
                 else Ret);
      end if;
   end Annotated_Value;

   ---------------------------
   -- Annotated_Object_Size --
   ---------------------------

   function Annotated_Object_Size
     (GT : GL_Type; Do_Align : Boolean := False) return Node_Ref_Or_Val
   is
      Use_Max       : constant Boolean := Is_Unconstrained_Record (GT);
      Size          : constant BA_Data :=
        Get_Type_Size (GT, Max_Size => Use_Max);
      Align         : constant BA_Data :=
        Const (if   Do_Align
               then Get_Type_Alignment (GT, Use_Specified => False)
               else ULL (BPU));

   begin
      --  We need to return a size that's a muliple of the alignment or at
      --  least aligned to a byte boundary.  It's possible that it already
      --  is and we can't detect it.  This isn't a problem for constant
      --  sizes, but may cause an extra computation for dynamic sizes.  For
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
      Name : String := "") return BA_Data is

   begin
      --  If we don't have an input, propagate that to the output.
      if No (V) then
         return V;

      --  If we have a constant, perform the operation on the constant and
      --  return it.

      elsif Is_Const (V) then
         return (False, F (V.C_Value, Name), No_Uint);

      --  Otherwise, create a new representation tree node

      else
         return (False, No_GL_Value, Create_Node (C, V.T_Value));
      end if;

   end Unop;

   -----------
   -- Binop --
   -----------

   function Binop
     (V1, V2 : BA_Data;
      F      : Binop_Access;
      C      : TCode;
      Name   : String := "") return BA_Data
   is
      Result   : GL_Value;
      Op1, Op2 : Node_Ref_Or_Val;

   begin
      --  If both are constants, do the operation as a constant and return
      --  that value unless it overflows.

      if Is_Const (V1) and then Is_Const (V2) then
         Result := F (V1.C_Value, V2.C_Value, Name);
         if not Overflowed (Result) and then not Is_Undef (Result) then
            return (False, Result, No_Uint);
         end if;
      end if;

      --  Otherwise, get our two operands as a node reference or Uint

      Op1 :=  Annotated_Value (V1);
      Op2 :=  Annotated_Value (V2);

      --  If either isn't valid, return invalid

      if Op1 = No_Uint or else Op2 = No_Uint then
         return No_BA;

         --  Otherwise build and return a node.  If there's a constant,
         --  it should be in the second position.

      else
         return (False, No_GL_Value,
                 Create_Node (C, (if Is_Static_SO_Ref (Op1) then Op2 else Op1),
                              (if Is_Static_SO_Ref (Op1) then Op1 else Op2)));
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

      if LHS_Op = No_Uint or else RHS_Op = No_Uint then
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

      if If_Op = No_Uint or else Then_Op = No_Uint
        or else Else_Op = No_Uint
      then
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
     (V : Node_Id; LHS : BA_Data := No_BA) return BA_Data
   is
      pragma Unreferenced (LHS);
      SO_Info : Dynamic_SO_Ref := Get_SO_Ref (V);

   begin
      --  If we didn't already get an SO_Ref for this expression, get one

      if SO_Info = No_Uint then
         --  If this expression contains a discriminant, build tree nodes
         --  corresponding to that discriminant.  If we have an unsupported
         --  node, return no value.

         if Contains_Discriminant (V) then
            declare
               Result   : BA_Data := No_BA;
               Attr     : Attribute_Id;
               RHS, LHS : BA_Data;

            begin
               case Nkind (V) is
                  when N_Identifier =>

                     if  Ekind (Entity (V)) = E_Discriminant then
                        SO_Info := Create_Discrim_Ref (Entity (V));
                     end if;

                  when N_Attribute_Reference =>

                     --  The only ones we support are 'Range_Length,
                     --  'Min, and 'Max

                     Attr := Get_Attribute_Id (Attribute_Name (V));
                     if Attr = Attribute_Range_Length
                       and then Is_Scalar_Type (Full_Etype (Prefix (V)))
                     then
                        declare
                           PT : constant Entity_Id := Full_Etype (Prefix (V));
                           LB : constant Node_Id   := Type_Low_Bound  (PT);
                           UB : constant Node_Id   := Type_High_Bound (PT);

                        begin

                           LHS    := Emit_Expr (LB);
                           RHS    := Emit_Expr (UB);
                           Result := Bounds_To_Length (LHS, RHS,
                                                       Full_GL_Type (V));
                        end;

                     elsif Attr in Attribute_Min | Attribute_Max then
                        LHS    := Emit_Expr (First (Expressions (V)));
                        RHS    := Emit_Expr (Last  (Expressions (V)));
                        Result := (if   Attr = Attribute_Min
                                   then Build_Min (LHS, RHS)
                                   else Build_Max (LHS, RHS));
                     end if;

                  when N_Op_Minus =>
                     Result := Neg (Emit_Expr (Right_Opnd (V)));

                  when N_Op_Plus =>
                     Result := Emit_Expr (Right_Opnd (V));

                  when N_Op_Add =>
                     LHS    := Emit_Expr (Left_Opnd  (V));
                     RHS    := Emit_Expr (Right_Opnd (V));
                     Result := LHS + RHS;

                  when N_Op_Subtract =>
                     LHS    := Emit_Expr (Left_Opnd  (V));
                     RHS    := Emit_Expr (Right_Opnd (V));
                     Result := LHS - RHS;

                  when N_Op_Multiply =>
                     LHS    := Emit_Expr (Left_Opnd  (V));
                     RHS    := Emit_Expr (Right_Opnd (V));
                     Result := LHS * RHS;

                  when N_Op_Divide =>
                     LHS    := Emit_Expr (Left_Opnd  (V));
                     RHS    := Emit_Expr (Right_Opnd (V));
                     Result := (if   Is_Unsigned_Type (Full_Etype (V))
                                then U_Div (LHS, RHS)
                                else LHS / RHS);

                  when N_Type_Conversion | N_Unchecked_Type_Conversion =>
                     Result := Emit_Convert (Expression (V), Full_GL_Type (V));

                  when others =>
                     null;
               end case;

               if Present (Result) then
                  SO_Info := Annotated_Value (Result);
               end if;
            end;

         --  Otherwise, see if this is a constant

         elsif Is_No_Elab_Needed (V) then
            declare
               Result : constant GL_Value := Emit_Expression (V);

            begin
               if not Overflowed (Result) and then not Is_Undef (Result) then
                  return (False, Emit_Expression (V), No_Uint);
               else
                  SO_Info :=
                    Create_Node (Dynamic_Val, UI_From_Int (Var_Idx_For_BA));
                  Var_Idx_For_BA := Var_Idx_For_BA + 1;
               end if;
            end;

         else
            SO_Info := Create_Node (Dynamic_Val, UI_From_Int (Var_Idx_For_BA));
            Var_Idx_For_BA := Var_Idx_For_BA + 1;
         end if;

         --  Save the computed value, if any

         if SO_Info /= No_Uint then
            Set_SO_Ref (V, SO_Info);
         end if;
      end if;

      --  And now return the value

      return (SO_Info = No_Uint, No_GL_Value, SO_Info);
   end Emit_Expr;

   ------------------
   -- Dump_BA_Data --
   ------------------

   procedure Dump_BA_Data (V : BA_Data) is
   begin
      if No (V) then
         Write_Line ("None");
      elsif Is_Const (V) then
         Dump_LLVM_Value (LLVM_Value (V.C_Value));
      else
         lgx (V.T_Value);
      end if;
   end  Dump_BA_Data;

end GNATLLVM.Types;
