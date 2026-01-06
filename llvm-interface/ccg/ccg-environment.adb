------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2025, AdaCore                     --
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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with Atree; use Atree;
with Table; use Table;

with LLVM.Core; use LLVM.Core;

with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.GLValue;     use GNATLLVM.GLValue;
with GNATLLVM.Wrapper;     use GNATLLVM.Wrapper;

with CCG.Helper; use CCG.Helper;
with CCG.Output; use CCG.Output;
with CCG.Utils;  use CCG.Utils;

package body CCG.Environment is

   type Value_Data is record
      C_Value        : Str;
      --  If Present, a string that represents the value of the Value_T

      Is_Decl_Output : Boolean;
      --  True if we wrote any needed decl for this value

      Is_LHS         : Boolean;
      --  True if this value represents an LHS. This is usually either a
      --  global variable or an alloca in the entry block. In that case,
      --  from a C perspective, a use of a value in LLVM IR represents the
      --  address of the value; only "load" or "store" instruction actually
      --  accesses the value. It can also be the result of a GEP
      --  instruction.

      Is_Constant    : Boolean;
      --  True if this value is a constant and was declared that way
      --  in C. We use this to indicate that we have to cast the type
      --  to the non-constant pointer to take the address of the value.

      MDT            : MD_Type;
      --  The MD_Type that was used to create this value, if known.  This
      --  is used to determine the appropriate C type if we need to create
      --  and declare this value as a variable.

      Is_Multi_MD    : Boolean;
      --  True if this value has been shared by multiple MD Types

      Entity         : Entity_Id;
      --  GNAT entity (either object or type) of this value, if known

      Entity_Is_Ref  : Boolean;
      --  True if this value corresponds to a reference to Entity

      Is_Used        : Boolean;
      --  True if this value represents a variable that has been used in an
      --  expression.

      Needs_Nest     : Boolean;
      --  True if this value is a function that needs to have a parameter
      --  added for the static chain. This is the usually the case if its
      --  address is taken and it doesn't already have one.

      Must_Globalize : Boolean;
      --  True if this value is a constant used in a inlined function and
      --  so must be promoted to being a global variable if we have to make
      --  a variable for it.

      Output_Idx     : Nat;
      --  A positive number if we've assigned an ordinal to use as
      --  part of the name for this anonymous value.

   end record;

   type Type_Data is record
      MDT                      : MD_Type;
      --  The MD type that this type  was created from

      Is_Multi_MD              : Boolean;
      --  True if this type has been shared between multiple MD_Types.
      --  This will often be the case for types since LLVM shares types.
      --  So i32 might correspond to both signed and unsigned 32-bit
      --  integers and all pointer types are the same. But in some cases,
      --  such as structs or specific-dimensioned arrays, there may only be
      --  one MD_type that a type came from and we want to take advantage
      --  of those cases.

      Entity                   : Opt_Type_Kind_Id;
      --  GNAT entity of this type, if known

   end record;

   type MD_Type_Data is record

      Entity                   : Opt_Type_Kind_Id;
      --  GNAT entity of this type, if known

      Is_Typedef_Output        : Boolean;
      --  True if this is a type either for which we don't write a typedef
      --  or if it is and we've written that typedef previously.

      Is_Return_Typedef_Output : Boolean;
      --  True if this is an array type and we've written the struct type
      --  that we use for the return type of a function returning this type.

      Is_Incomplete_Output     : Boolean;
      --  True if this is a struct type and we've just written the struct
      --  definition without fields (an incomplete type).

      Are_Outputting_Typedef   : Boolean;
      --  True if we're in the process of outputting a typedef

      Used_In_Struct           : Boolean;
      --  True if this type is the type of a field in a struct

      Cannot_Pack              : Boolean;
      --  True if this is a type that we want to pack, but can't because of
      --  restrictions in our C compiler.

      Output_Idx               : Nat;
      --  A positive number if we've assigned an ordinal to use as
      --  part of the name for this anonymous type.
   end record;

   type BB_Data is record
      Flow        : Flow_Idx;
      --  The Flow corresponding to this block, if any

      Output_Idx  : Nat;
      --  A positive number if we've assigned an ordinal to use as
      --  part of the name for this block.

   end record;

   type Value_Idx   is new Nat;
   type Type_Idx    is new Nat;
   type MD_Type_Idx is new Nat;
   type BB_Idx      is new Nat;

   No_Value_Idx   : constant Value_Idx   := 0;
   No_Type_Idx    : constant Type_Idx    := 0;
   No_MD_Type_Idx : constant MD_Type_Idx := 0;
   No_BB_Idx      : constant BB_Idx      := 0;

   function Present (X : Value_Idx)   return Boolean is (X /= No_Value_Idx);
   function Present (X : Type_Idx)    return Boolean is (X /= No_Type_Idx);
   function Present (X : MD_Type_Idx) return Boolean is (X /= No_MD_Type_Idx);
   function Present (X : BB_Idx)      return Boolean is (X /= No_BB_Idx);

   package Value_Info is new Table.Table
     (Table_Component_Type => Value_Data,
      Table_Index_Type     => Value_Idx,
      Table_Low_Bound      => 1,
      Table_Initial        => 500,
      Table_Increment      => 100,
      Table_Name           => "Value_Info");

   package Type_Info is new Table.Table
     (Table_Component_Type => Type_Data,
      Table_Index_Type     => Type_Idx,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 50,
      Table_Name           => "Type_Info");

   package MD_Type_Info is new Table.Table
     (Table_Component_Type => MD_Type_Data,
      Table_Index_Type     => MD_Type_Idx,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 50,
      Table_Name           => "MD_Type_Info");

   package BB_Info is new Table.Table
     (Table_Component_Type => BB_Data,
      Table_Index_Type     => BB_Idx,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 50,
      Table_Name           => "BB_Info");

   package Value_Info_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Value_T,
      Element_Type    => Value_Idx,
      Hash            => Hash,
      Equivalent_Keys => "=");
   Value_Info_Map : Value_Info_Maps.Map;

   package Type_Info_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Type_T,
      Element_Type    => Type_Idx,
      Hash            => Hash,
      Equivalent_Keys => "=");
   Type_Info_Map : Type_Info_Maps.Map;

   package MD_Type_Info_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => MD_Type,
      Element_Type    => MD_Type_Idx,
      Hash            => Hash,
      Equivalent_Keys => "=");
   MD_Type_Info_Map : MD_Type_Info_Maps.Map;

   package BB_Info_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Basic_Block_T,
      Element_Type    => BB_Idx,
      Hash            => Hash,
      Equivalent_Keys => "=");
   BB_Info_Map : BB_Info_Maps.Map;

   package Ext_Name_Info_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => MD_Type,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   Ext_Name_Info_Map : Ext_Name_Info_Maps.Map;

   Output_Idx : Nat := 1;
   --  The next output index to use for values, types, and basic blocks

   --  Functions to return the corresponding index for a value, type, or
   --  basic block and whether to create one if one isn't present.

   function Value_Info_Idx (V : Value_T; Create : Boolean) return Value_Idx
     with Pre  => Present (V),
          Post => not Create or else Present (Value_Info_Idx'Result);
   function Type_Info_Idx  (T : Type_T; Create : Boolean) return Type_Idx
     with Pre  => Present (T),
          Post => not Create or else Present (Type_Info_Idx'Result);
   function MD_Type_Info_Idx (M : MD_Type; Create : Boolean) return MD_Type_Idx
     with Pre  => Present (M),
          Post => not Create or else Present (MD_Type_Info_Idx'Result);
   function BB_Info_Idx    (B : Basic_Block_T; Create : Boolean) return BB_Idx
     with Pre  => Present (B),
          Post => not Create or else Present (BB_Info_Idx'Result);

   --------------------
   -- Value_Info_Idx --
   --------------------

   function Value_Info_Idx (V : Value_T; Create : Boolean) return Value_Idx
   is
      use Value_Info_Maps;

   begin
      if Value_Info_Map.Contains (V) then
         return Value_Info_Map (V);
      elsif not Create then
         return No_Value_Idx;
      else
         Value_Info.Append ((C_Value        => No_Str,
                             Is_Decl_Output => False,
                             Is_LHS         => False,
                             Is_Constant    => False,
                             MDT            => No_MD_Type,
                             Is_Multi_MD    => False,
                             Entity         => Types.Empty,
                             Entity_Is_Ref  => False,
                             Is_Used        => False,
                             Needs_Nest     => False,
                             Must_Globalize => False,
                             Output_Idx     => 0));
         Insert (Value_Info_Map, V, Value_Info.Last);
         Notify_On_Value_Delete (V, Delete_Value_Info'Access);
         return Value_Info.Last;
      end if;
   end Value_Info_Idx;

   -----------------------
   -- Delete_Value_Info --
   -----------------------

   procedure Delete_Value_Info (V : Value_T) is
      use Value_Info_Maps;
      use BB_Info_Maps;
   begin
      --  If we had an entity associated with this value, show that we
      --  no longer have that assocation.

      if Present (Get_Entity (V)) then
         Set_Value_R (Get_Entity (V), No_GL_Value);
      end if;

      --  If this a function, delete any information we have about the
      --  function's parameters

      if Is_A_Function (V) then
         Delete_Function_Info (V);
      end if;

      --  Likewise if it's a basic block

      if Is_A_Basic_Block (V) then
         Exclude (BB_Info_Map, Value_As_Basic_Block (V));
      end if;

      --  Finally delete all other information we think we know about
      --  this value.

      Exclude (Value_Info_Map, V);
   end Delete_Value_Info;

   -------------------
   -- Type_Info_Idx --
   -------------------

   function Type_Info_Idx (T : Type_T; Create : Boolean) return Type_Idx
   is
      use Type_Info_Maps;

   begin
      if Type_Info_Map.Contains (T) then
         return Type_Info_Map (T);
      elsif not Create then
         return No_Type_Idx;
      else
         Type_Info.Append ((MDT                      => No_MD_Type,
                            Is_Multi_MD              => False,
                            Entity                   => Types.Empty));
         Insert (Type_Info_Map, T, Type_Info.Last);
         return Type_Info.Last;
      end if;
   end Type_Info_Idx;

   ----------------------
   -- MD_Type_Info_Idx --
   ----------------------

   function MD_Type_Info_Idx (M : MD_Type; Create : Boolean) return MD_Type_Idx
   is
      use MD_Type_Info_Maps;

   begin
      if MD_Type_Info_Map.Contains (M) then
         return MD_Type_Info_Map (M);
      elsif not Create then
         return No_MD_Type_Idx;
      else
         MD_Type_Info.Append ((Entity                   => Types.Empty,
                               Is_Typedef_Output        => False,
                               Is_Return_Typedef_Output => False,
                               Is_Incomplete_Output     => False,
                               Are_Outputting_Typedef   => False,
                               Used_In_Struct           => False,
                               Cannot_Pack              => False,
                               Output_Idx               => 0));
         Insert (MD_Type_Info_Map, M, MD_Type_Info.Last);
         return MD_Type_Info.Last;
      end if;
   end MD_Type_Info_Idx;

   -----------------
   -- BB_Info_Idx --
   -----------------

   function BB_Info_Idx (B : Basic_Block_T; Create : Boolean) return BB_Idx
   is
      use BB_Info_Maps;

   begin
      if BB_Info_Map.Contains (B) then
         return BB_Info_Map (B);
      elsif not Create then
         return No_BB_Idx;
      else
         BB_Info.Append ((Flow        => Empty_Flow_Idx,
                          Output_Idx  => 0));
         Insert (BB_Info_Map, B, BB_Info.Last);

         --  Handle the case where a basic block is deleted since it may
         --  be reused.

         Notify_On_Value_Delete
           (Basic_Block_As_Value (B), Delete_Value_Info'Access);
         return BB_Info.Last;
      end if;
   end BB_Info_Idx;

   -----------------
   -- Get_C_Value --
   -----------------

   function Get_C_Value (V : Value_T) return Str is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => False);

   begin
      return (if   Present (Idx) then Value_Info.Table (Idx).C_Value
              else No_Str);
   end Get_C_Value;

   ------------------------
   -- Get_Is_Decl_Output --
   ------------------------

   function Get_Is_Decl_Output (V : Value_T) return Boolean is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => False);

   begin
      return Present (Idx)
        and then Value_Info.Table (Idx).Is_Decl_Output;
   end Get_Is_Decl_Output;

   ----------------
   -- Get_Is_LHS --
   ----------------

   function Get_Is_LHS (V : Value_T) return Boolean is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => False);

   begin
      return Present (Idx) and then Value_Info.Table (Idx).Is_LHS;
   end Get_Is_LHS;

   ---------------------
   -- Get_Is_Constant --
   ---------------------

   function Get_Is_Constant (V : Value_T) return Boolean is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => False);

   begin
      return Present (Idx) and then Value_Info.Table (Idx).Is_Constant;
   end Get_Is_Constant;

   -----------------
   -- Get_MD_Type --
   -----------------

   function Get_MD_Type (V : Value_T) return MD_Type is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => False);

   begin
      return (if  Present (Idx) then Value_Info.Table (Idx).MDT
              else No_MD_Type);
   end Get_MD_Type;

   ---------------------
   -- Get_Is_Multi_MD --
   ---------------------

   function Get_Is_Multi_MD (V : Value_T) return Boolean is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => False);

   begin
      return Present (Idx) and then Value_Info.Table (Idx).Is_Multi_MD;
   end Get_Is_Multi_MD;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity (V : Value_T) return Entity_Id is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => False);

   begin
      return (if  Present (Idx) then Value_Info.Table (Idx).Entity
              else Types.Empty);
   end Get_Entity;

   -----------------------
   -- Get_Entity_Is_Ref --
   -----------------------

   function Get_Entity_Is_Ref (V : Value_T) return Boolean is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => False);

   begin
      return Present (Idx) and then Value_Info.Table (Idx).Entity_Is_Ref;
   end Get_Entity_Is_Ref;

   ------------------
   -- Get_Is_Used --
   ------------------

   function Get_Is_Used (V : Value_T) return Boolean is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => False);

   begin
      return Present (Idx) and then Value_Info.Table (Idx).Is_Used;
   end Get_Is_Used;

   --------------------
   -- Get_Needs_Nest --
   --------------------

   function Get_Needs_Nest (V : Value_T) return Boolean is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => False);

   begin
      return Present (Idx) and then Value_Info.Table (Idx).Needs_Nest;
   end Get_Needs_Nest;

   ------------------------
   -- Get_Must_Globalize --
   ------------------------

   function Get_Must_Globalize (V : Value_T) return Boolean is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => False);

   begin
      return Present (Idx) and then Value_Info.Table (Idx).Must_Globalize;
   end Get_Must_Globalize;

   -----------------
   -- Set_C_Value --
   -----------------

   procedure Set_C_Value (V : Value_T; S : Str) is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => True);

   begin
      Value_Info.Table (Idx).C_Value := S;
   end Set_C_Value;

   ------------------------
   -- Set_Is_Decl_Output --
   ------------------------

   procedure Set_Is_Decl_Output (V : Value_T; B : Boolean := True) is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => True);

   begin
      Value_Info.Table (Idx).Is_Decl_Output := B;
   end Set_Is_Decl_Output;

   ----------------
   -- Set_Is_LHS --
   ----------------

   procedure Set_Is_LHS (V : Value_T; B : Boolean := True) is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => True);

   begin
      Value_Info.Table (Idx).Is_LHS := B;
   end Set_Is_LHS;

   ---------------------
   -- Set_Is_Constant --
   ---------------------

   procedure Set_Is_Constant (V : Value_T; B : Boolean := True) is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => True);

   begin
      Value_Info.Table (Idx).Is_Constant := B;
   end Set_Is_Constant;

   -----------------
   -- Set_MD_Type --
   -----------------

   procedure Set_MD_Type (V : Value_T; M : MD_Type) is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => True);

   begin
      Value_Info.Table (Idx).MDT := M;
   end Set_MD_Type;

   ---------------------
   -- Set_Is_Multi_MD --
   ---------------------

   procedure Set_Is_Multi_MD (V : Value_T; B : Boolean := True) is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => True);

   begin
      Value_Info.Table (Idx).Is_Multi_MD := B;
   end Set_Is_Multi_MD;

   ----------------
   -- Set_Entity --
   ----------------

   procedure Set_Entity (V : Value_T; E : Entity_Id) is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => True);

   begin
      Value_Info.Table (Idx).Entity := E;
   end Set_Entity;

   -----------------------
   -- Set_Entity_Is_Ref --
   -----------------------

   procedure Set_Entity_Is_Ref (V : Value_T; B : Boolean := True) is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => True);

   begin
      Value_Info.Table (Idx).Entity_Is_Ref := B;
   end Set_Entity_Is_Ref;

   -----------------
   -- Set_Is_Used --
   -----------------

   procedure Set_Is_Used (V : Value_T; B : Boolean := True) is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => True);

   begin
      Value_Info.Table (Idx).Is_Used := B;
   end Set_Is_Used;

   --------------------
   -- Set_Needs_Nest --
   --------------------

   procedure Set_Needs_Nest (V : Value_T; B : Boolean := True) is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => True);

   begin
      Value_Info.Table (Idx).Needs_Nest := B;
   end Set_Needs_Nest;

   ------------------------
   -- Set_Must_Globalize --
   ------------------------

   procedure Set_Must_Globalize (V : Value_T; B : Boolean := True) is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => True);

   begin
      Value_Info.Table (Idx).Must_Globalize := B;
   end Set_Must_Globalize;

   -----------------
   -- Get_MD_Type --
   -----------------

   function Get_MD_Type (T : Type_T) return MD_Type is
      Idx : constant Type_Idx := Type_Info_Idx (T, Create => False);

   begin
      return (if  Present (Idx) then Type_Info.Table (Idx).MDT
              else No_MD_Type);
   end Get_MD_Type;

   ---------------------
   -- Get_Is_Multi_MD --
   ---------------------

   function Get_Is_Multi_MD (T : Type_T) return Boolean is
      Idx : constant Type_Idx := Type_Info_Idx (T, Create => False);

   begin
      return Present (Idx) and then Type_Info.Table (Idx).Is_Multi_MD;
   end Get_Is_Multi_MD;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity (T : Type_T) return Opt_Type_Kind_Id is
      Idx : constant Type_Idx := Type_Info_Idx (T, Create => False);

   begin
      return (if   Present (Idx) then Type_Info.Table (Idx).Entity
              else Types.Empty);
   end Get_Entity;

   -----------------
   -- Set_MD_Type --
   -----------------

   procedure Set_MD_Type (T : Type_T; M : MD_Type) is
      Idx : constant Type_Idx := Type_Info_Idx (T, Create => True);

   begin
      Type_Info.Table (Idx).MDT := M;
   end Set_MD_Type;

   ---------------------
   -- Set_Is_Multi_MD --
   ---------------------

   procedure Set_Is_Multi_MD (T : Type_T; B : Boolean := True) is
      Idx : constant Type_Idx := Type_Info_Idx (T, Create => True);

   begin
      Type_Info.Table (Idx).Is_Multi_MD := B;
   end Set_Is_Multi_MD;

   ----------------
   -- Set_Entity --
   ----------------

   procedure Set_Entity (T : Type_T; TE : Type_Kind_Id) is
      Idx : constant Type_Idx := Type_Info_Idx (T, Create => True);

   begin
      Type_Info.Table (Idx).Entity := TE;
   end Set_Entity;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity (M : MD_Type) return Opt_Type_Kind_Id is
      Idx : constant MD_Type_Idx := MD_Type_Info_Idx (M, Create => False);

   begin
      return (if   Present (Idx) then MD_Type_Info.Table (Idx).Entity
              else Types.Empty);
   end Get_Entity;

   ---------------------------
   -- Get_Is_Typedef_Output --
   ---------------------------

   function Get_Is_Typedef_Output (M : MD_Type) return Boolean is
      Idx : constant MD_Type_Idx := MD_Type_Info_Idx (M, Create => False);

   begin
      return Present (Idx) and then MD_Type_Info.Table (Idx).Is_Typedef_Output;
   end Get_Is_Typedef_Output;

   ----------------------------------
   -- Get_Is_Return_Typedef_Output --
   ----------------------------------

   function Get_Is_Return_Typedef_Output (M : MD_Type) return Boolean is
      Idx : constant MD_Type_Idx := MD_Type_Info_Idx (M, Create => False);

   begin
      return Present (Idx)
        and then MD_Type_Info.Table (Idx).Is_Return_Typedef_Output;
   end Get_Is_Return_Typedef_Output;

   ------------------------------
   -- Get_Is_Incomplete_Output --
   ------------------------------

   function Get_Is_Incomplete_Output (M : MD_Type) return Boolean is
      Idx : constant MD_Type_Idx := MD_Type_Info_Idx (M, Create => False);

   begin
      return Present (Idx)
        and then MD_Type_Info.Table (Idx).Is_Incomplete_Output;
   end Get_Is_Incomplete_Output;

   --------------------------------
   -- Get_Are_Outputting_Typedef --
   --------------------------------

   function Get_Are_Outputting_Typedef (M : MD_Type) return Boolean is
      Idx : constant MD_Type_Idx := MD_Type_Info_Idx (M, Create => False);

   begin
      return Present (Idx)
        and then MD_Type_Info.Table (Idx).Are_Outputting_Typedef;
   end Get_Are_Outputting_Typedef;

   ---------------------
   -- Get_Cannot_Pack --
   ---------------------

   function Get_Cannot_Pack (M : MD_Type) return Boolean is
      Idx : constant MD_Type_Idx := MD_Type_Info_Idx (M, Create => False);

   begin
      return Present (Idx) and then MD_Type_Info.Table (Idx).Cannot_Pack;
   end Get_Cannot_Pack;

   ------------------------
   -- Get_Used_In_Struct --
   ------------------------

   function Get_Used_In_Struct (M : MD_Type) return Boolean is
      Idx : constant MD_Type_Idx := MD_Type_Info_Idx (M, Create => False);

   begin
      return Present (Idx) and then MD_Type_Info.Table (Idx).Used_In_Struct;
   end Get_Used_In_Struct;

   ----------------
   -- Set_Entity --
   ----------------

   procedure Set_Entity (M : MD_Type; TE : Type_Kind_Id) is
      Idx : constant MD_Type_Idx := MD_Type_Info_Idx (M, Create => True);

   begin
      MD_Type_Info.Table (Idx).Entity := TE;
   end Set_Entity;

   --------------------------
   -- Set_Is_Typedef_Output --
   --------------------------

   procedure Set_Is_Typedef_Output (M : MD_Type; B : Boolean := True) is
      Idx : constant MD_Type_Idx := MD_Type_Info_Idx (M, Create => True);

   begin
      MD_Type_Info.Table (Idx).Is_Typedef_Output := B;
   end Set_Is_Typedef_Output;

   ----------------------------------
   -- Set_Is_Return_Typedef_Output --
   ----------------------------------

   procedure Set_Is_Return_Typedef_Output (M : MD_Type; B : Boolean := True) is
      Idx : constant MD_Type_Idx := MD_Type_Info_Idx (M, Create => True);

   begin
      MD_Type_Info.Table (Idx).Is_Return_Typedef_Output := B;
   end Set_Is_Return_Typedef_Output;

   ------------------------------
   -- Set_Is_Incomplete_Output --
   ------------------------------

   procedure Set_Is_Incomplete_Output (M : MD_Type; B : Boolean := True) is
      Idx : constant MD_Type_Idx := MD_Type_Info_Idx (M, Create => True);

   begin
      MD_Type_Info.Table (Idx).Is_Incomplete_Output := B;
   end Set_Is_Incomplete_Output;

   --------------------------------
   -- Set_Are_Outputting_Typedef --
   --------------------------------

   procedure Set_Are_Outputting_Typedef (M : MD_Type; B : Boolean := True) is
      Idx : constant MD_Type_Idx := MD_Type_Info_Idx (M, Create => True);

   begin
      MD_Type_Info.Table (Idx).Are_Outputting_Typedef := B;
   end Set_Are_Outputting_Typedef;

   ------------------------
   -- Set_Used_In_Struct --
   ------------------------

   procedure Set_Used_In_Struct (M : MD_Type; B : Boolean := True) is
      Idx : constant MD_Type_Idx := MD_Type_Info_Idx (M, Create => True);

   begin
      MD_Type_Info.Table (Idx).Used_In_Struct := B;
   end Set_Used_In_Struct;

   ---------------------
   -- Set_Cannot_Pack --
   ---------------------

   procedure Set_Cannot_Pack (M : MD_Type; B : Boolean := True) is
      Idx : constant MD_Type_Idx := MD_Type_Info_Idx (M, Create => True);

   begin
      MD_Type_Info.Table (Idx).Cannot_Pack := B;
   end Set_Cannot_Pack;

   --------------
   -- Get_Flow --
   --------------

   function Get_Flow (BB : Basic_Block_T) return Flow_Idx is
      Idx : constant BB_Idx := BB_Info_Idx (BB, Create => False);

   begin
      return (if   Present (Idx) then BB_Info.Table (Idx).Flow
              else Empty_Flow_Idx);
   end Get_Flow;

   --------------
   -- Set_Flow --
   --------------

   procedure Set_Flow (BB : Basic_Block_T; Fidx : Flow_Idx) is
      Idx : constant BB_Idx := BB_Info_Idx (BB, Create => True);

   begin
      BB_Info.Table (Idx).Flow := Fidx;
   end Set_Flow;

   --------------------------
   -- Maybe_Output_Typedef --
   --------------------------

   procedure Maybe_Output_Typedef (MD : MD_Type; Incomplete : Boolean := False)
   is
   begin
      --  If we're outputting this, have output it, or are just looking for
      --  an incomplete definition and have already output one, we don't
      --  need to do anything. Otherwise, output the typedef.

      if Get_Are_Outputting_Typedef (MD)
        or else Get_Is_Typedef_Output (MD)
        or else (Incomplete and then Get_Is_Incomplete_Output (MD))
      then
         null;
      else
         Output_Typedef (MD, Incomplete => Incomplete);
      end if;
   end Maybe_Output_Typedef;

   --------------------
   -- Get_Output_Idx --
   --------------------

   function Get_Output_Idx (V : Value_T) return Nat is
      Idx : constant Value_Idx := Value_Info_Idx (V, Create => True);
      VD  : Value_Data renames Value_Info.Table (Idx);

   begin
      if VD.Output_Idx = 0 then
         VD.Output_Idx := Output_Idx;
         Output_Idx    := Output_Idx + 1;
      end if;

      return VD.Output_Idx;
   end Get_Output_Idx;

   --------------------
   -- Get_Output_Idx --
   --------------------

   function Get_Output_Idx (M : MD_Type) return Nat is
      Idx : constant MD_Type_Idx := MD_Type_Info_Idx (M, Create => True);
      MTD : MD_Type_Data renames MD_Type_Info.Table (Idx);

   begin
      if MTD.Output_Idx = 0 then
         MTD.Output_Idx := Output_Idx;
         Output_Idx     := Output_Idx + 1;
      end if;

      return MTD.Output_Idx;
   end Get_Output_Idx;

   --------------------
   -- Get_Output_Idx --
   --------------------

   function Get_Output_Idx (BB : Basic_Block_T) return Nat is
      Idx : constant BB_Idx := BB_Info_Idx (BB, Create => True);
      BBD : BB_Data renames BB_Info.Table (Idx);

   begin
      if BBD.Output_Idx = 0 then
         BBD.Output_Idx := Output_Idx;
         Output_Idx     := Output_Idx + 1;
      end if;

      return BBD.Output_Idx;
   end Get_Output_Idx;

   --------------------
   -- Get_Output_Idx --
   --------------------

   function Get_Output_Idx return Nat is
      Result : constant Nat := Output_Idx;

   begin
      Output_Idx := Output_Idx + 1;
      return Result;
   end Get_Output_Idx;

   -----------------
   -- Get_MD_Type --
   -----------------

   function Get_MD_Type (S : String) return MD_Type is
      use Ext_Name_Info_Maps;
   begin
      return (if   Ext_Name_Info_Map.Contains (S) then Ext_Name_Info_Map (S)
              else No_MD_Type);
   end Get_MD_Type;

   -----------------
   -- Set_MD_Type --
   -----------------

   procedure Set_MD_Type (S : String; MD : MD_Type) is
      use Ext_Name_Info_Maps;
   begin
      Ext_Name_Info_Map.Include (S, MD);
   end Set_MD_Type;

end CCG.Environment;
