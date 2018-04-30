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

with GNATLLVM.Arrays; use GNATLLVM.Arrays;
with GNATLLVM.Types;  use GNATLLVM.Types;
with GNATLLVM.Utils;  use GNATLLVM.Utils;

package body GNATLLVM.GLValue is

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
     (G (Get_Undef (Create_Access_Type (TE)), TE, Is_Reference => True));

   ----------------
   -- Const_Null --
   ----------------

   function Const_Null (TE : Entity_Id) return GL_Value is
     (G (Const_Null (Create_Type (TE)), TE));

   --------------------
   -- Const_Null_Ptr --
   --------------------

   function Const_Null_Ptr (TE : Entity_Id) return GL_Value is
     (G (Const_Null (Pointer_Type (Create_Type (TE), 0)), TE,
         Is_Reference => True));

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
     (G (Const_Int (Create_Type (TE), N, Sign_Extend => Sign_Extend),
         TE));

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
     (G (Int_To_Ptr (IR_Builder, LLVM_Value (V), Create_Type (TE), Name),
         TE));

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
          TE, Is_Reference => True, Is_Raw_Array => True));

   ----------------
   -- Ptr_To_Int --
   ----------------

   function Ptr_To_Int
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
   is
     (G (Ptr_To_Int (IR_Builder, LLVM_Value (V), Create_Type (TE), Name),
         TE));

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
          TE, Is_Reference => True, Is_Raw_Array => True));

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
   begin
      if Is_Double_Reference (Ptr) then
         return G_Ref (Load (IR_Builder, LLVM_Value (Ptr), Name), Ptr.Typ);
      else
         return G (Load_With_Type (Full_Designated_Type (Ptr),
                                   LLVM_Value (Ptr), Name),
                   Full_Designated_Type (Ptr));
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

   ----------
   -- Call --
   ----------

   function Call
     (Func        : GL_Value;
      Result_Type : Entity_Id;
      Args        : GL_Value_Array;
      Name        : String := "") return GL_Value
   is
      Arg_Values  : Value_Array (Args'Range);
      Result      : Value_T;

   begin
      for J in Args'Range loop
         Arg_Values (J) := LLVM_Value (Args (J));
      end loop;

      Result := Call (IR_Builder, LLVM_Value (Func),
                      Arg_Values'Address, Arg_Values'Length, Name);
      return G (Result, Result_Type);
   end Call;

   --------------
   -- Call_Ref --
   --------------

   function Call_Ref
     (Func        : GL_Value;
      Result_Type : Entity_Id;
      Args        : GL_Value_Array;
      Name        : String := "") return GL_Value
   is
      Result      : constant GL_Value := Call (Func, Result_Type, Args, Name);

   begin
      return G_Ref (LLVM_Value (Result), Result_Type);
   end Call_Ref;

   ----------
   -- Call --
   ----------

   procedure Call
     (Func : GL_Value; Args : GL_Value_Array; Name : String := "") is
   begin
      Discard (Call (Func, Standard_Void_Type, Args, Name));
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
                     Name), TE,
         Is_Reference => True, Is_Double_Reference => Need_Reference));

   ---------------------
   -- Set_Initializer --
   ---------------------

   procedure Set_Initializer (Var, Expr : GL_Value) is
   begin
      Set_Initializer (LLVM_Value (Var), LLVM_Value (Expr));
   end Set_Initializer;

   -----------------
   -- Set_Linkage --
   -----------------

   procedure Set_Linkage (Var : GL_Value; Linkage : Linkage_T) is
   begin
      Set_Linkage (LLVM_Value (Var), Linkage);
   end Set_Linkage;

end GNATLLVM.GLValue;
