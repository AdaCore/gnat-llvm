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

with GNATLLVM.Arrays;      use GNATLLVM.Arrays;
with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.Types;  use GNATLLVM.Types;
with GNATLLVM.Utils;  use GNATLLVM.Utils;

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
            --  ?? Keep the above test until we see if we can use Fat_Pointer
            --  consistently for this.
                          or else (Ekind (V.Typ) = E_Subprogram_Type
                                     and then Needs_Activation_Record
                                     (V.Typ)));

         when Double_Reference =>
            return Is_Type (V.Typ)
              and then Get_Type_Kind (Type_Of (V.Value)) = Pointer_Type_Kind;

         when Fat_Pointer | Bounds =>
            return Is_Unconstrained_Array (V.Typ);

         when Array_Data | Reference_To_Bounds =>
            return Is_Type (V.Typ)
              and then Get_Type_Kind (Type_Of (V.Value)) = Pointer_Type_Kind
              and then Is_Unconstrained_Array (V.Typ);

         when Reference_To_Subprogram =>
            return Get_Type_Kind (Type_Of (V.Value)) = Pointer_Type_Kind;

         when Invalid =>
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
     (TE          : Entity_Id;
      N           : unsigned_long_long;
      Sign_Extend : Boolean := False) return GL_Value
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
         --  ?? At some point, we need to deal with TBAA or similar for these.

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
                TE);
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
