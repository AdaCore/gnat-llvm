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

with GNATLLVM.Types; use GNATLLVM.Types;
with GNATLLVM.Utils; use GNATLLVM.Utils;

package body GNATLLVM.GLValue is

   ----------------
   -- Need_Value --
   ----------------

   function Need_Value
     (Env : Environ; V : GL_Value; TE : Entity_Id) return GL_Value
   is
   begin

      --  If V is of dynamic size, the "value" we use is the
      --  reference, so return it.  Similarly for subprograms.
      --  Likewise if it's not a reference.  Otherwise, load the
      --  value.

      if Ekind (TE) = E_Subprogram_Type
        or else not Is_Reference (V)
        or else (Is_Reference (V)
                   and then Is_Dynamic_Size (Env, Full_Designated_Type (V)))
      then
         return V;
      else
         return Load (Env, V);
      end if;
   end Need_Value;

   -------------
   -- Discard --
   -------------

   procedure Discard (G : GL_Value) is
      pragma Unreferenced (G);
   begin
      null;
   end Discard;

   ------------
   -- Alloca --
   ------------

   function Alloca
      (Env : Environ; TE : Entity_Id; Name : String := "") return GL_Value
   is
     (G_Ref (Alloca (Env.Bld, Create_Type (Env, TE), Name), TE));

   ------------------
   -- Array_Alloca --
   ------------------

   function Array_Alloca
     (Env      : Environ;
      TE       : Entity_Id;
      Num_Elts : GL_Value;
      Name     : String := "") return GL_Value
   is
     (G_Ref (Array_Alloca (Env.Bld, Create_Type (Env, TE),
                           LLVM_Value (Num_Elts), Name),
             TE));

   ----------------
   --  Get_Undef --
   ----------------

   function Get_Undef (Env : Environ; TE : Entity_Id) return GL_Value is
     (G (Get_Undef (Create_Type (Env, TE)), TE));

   ----------------
   -- Const_Null --
   ----------------

   function Const_Null (Env : Environ; TE : Entity_Id) return GL_Value is
     (G (Const_Null (Create_Type (Env, TE)), TE));

   ---------------
   -- Const_Int --
   ---------------

   function Const_Int
     (Env : Environ; TE : Entity_Id; N : Uint) return GL_Value
   is
     (G (Const_Int (Create_Type (Env, TE), N), TE));

   ---------------
   -- Const_Int --
   ---------------

   function Const_Int
     (Env         : Environ;
      TE          : Entity_Id;
      N           : unsigned_long_long;
      Sign_Extend : Boolean := False) return GL_Value
   is
     (G (Const_Int (Create_Type (Env, TE), N, Sign_Extend => Sign_Extend),
         TE));

   ----------------
   -- Const_Real --
   ----------------

   function Const_Real
     (Env : Environ; TE : Entity_Id; V : double) return GL_Value
   is
     (G (Const_Real (Create_Type (Env, TE), V), TE));

   ----------------
   -- Int_To_Ptr --
   ----------------

   function Int_To_Ptr
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
   is
     (G (Int_To_Ptr (Env.Bld, LLVM_Value (V), Create_Type (Env, TE), Name),
         TE));

   ----------------
   -- Int_To_Ref --
   ----------------

   function Int_To_Ref
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
   is
      (G_Ref (Int_To_Ptr (Env.Bld, LLVM_Value (V),
                          Pointer_Type (Create_Type (Env, TE), 0),
                          Name),
              TE));

   ----------------
   -- Ptr_To_Int --
   ----------------

   function Ptr_To_Int
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
   is
     (G (Ptr_To_Int (Env.Bld, LLVM_Value (V), Create_Type (Env, TE), Name),
         TE));

   --------------
   -- Bit_Cast --
   --------------

   function Bit_Cast
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
   is
     (G (Bit_Cast (Env.Bld, LLVM_Value (V), Create_Type (Env, TE), Name), TE));

   ------------------
   -- Pointer_Cast --
   ------------------

   function Pointer_Cast
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
   is
     (G (Pointer_Cast (Env.Bld, LLVM_Value (V), Create_Type (Env, TE), Name),
         TE));

   ----------------
   -- Ptr_To_Ref --
   ----------------

   function Ptr_To_Ref
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
   is
      (G_Ref (Pointer_Cast (Env.Bld, LLVM_Value (V),
                            Pointer_Type (Create_Type (Env, TE), 0),
                            Name),
              TE));

   -----------
   -- Trunc --
   -----------

   function Trunc
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
   is
     (G (Trunc (Env.Bld, LLVM_Value (V), Create_Type (Env, TE), Name), TE));

   -----------
   -- S_Ext --
   -----------

   function S_Ext
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
   is
     (G (S_Ext (Env.Bld, LLVM_Value (V), Create_Type (Env, TE), Name), TE));

   -----------
   -- Z_Ext --
   -----------

   function Z_Ext
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
   is
     (G (Z_Ext (Env.Bld, LLVM_Value (V), Create_Type (Env, TE), Name), TE));

   --------------
   -- FP_Trunc --
   --------------

   function FP_Trunc
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
   is
     (G (FP_Trunc (Env.Bld, LLVM_Value (V), Create_Type (Env, TE), Name), TE));

   ------------
   -- FP_Ext --
   ------------

   function FP_Ext
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
   is
     (G (FP_Ext (Env.Bld, LLVM_Value (V), Create_Type (Env, TE), Name), TE));

   --------------
   -- FP_To_SI --
   --------------

   function FP_To_SI
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
   is
     (G (FP_To_SI (Env.Bld, LLVM_Value (V), Create_Type (Env, TE), Name), TE));

   --------------
   -- FP_To_UI --
   --------------

   function FP_To_UI
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
   is
     (G (FP_To_UI (Env.Bld, LLVM_Value (V), Create_Type (Env, TE), Name), TE));

   --------------
   -- UI_To_FP --
   --------------

   function UI_To_FP
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
   is
     (G (UI_To_FP (Env.Bld, LLVM_Value (V), Create_Type (Env, TE), Name), TE));

   --------------
   -- SI_To_FP --
   --------------

   function SI_To_FP
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
   is
     (G (SI_To_FP (Env.Bld, LLVM_Value (V), Create_Type (Env, TE), Name), TE));

   -------------------
   -- Build_Cond_Br --
   -------------------

   procedure Build_Cond_Br
     (Env : Environ; C_If : GL_Value; C_Then, C_Else : Basic_Block_T)
   is
   begin
      Discard (Build_Cond_Br (Env.Bld, LLVM_Value (C_If), C_Then, C_Else));
   end Build_Cond_Br;

   ---------------
   -- Build_Phi --
   ---------------

   function Build_Phi
     (Env       : Environ;
      GL_Values : GL_Value_Array;
      BBs       : Basic_Block_Array;
      Name      : String := "") return GL_Value
   is
      Values  : Value_Array (GL_Values'Range);
      Our_Phi : Value_T;
   begin
      for I in Values'Range loop
         Values (I) := LLVM_Value (GL_Values (I));
      end loop;

      Our_Phi := Phi (Env.Bld, Type_Of (GL_Values (GL_Values'First)), Name);
      Add_Incoming (Our_Phi, Values'Address, BBs'Address, Values'Length);
      return G_From (Our_Phi, GL_Values (GL_Values'First));
   end Build_Phi;

   --------------------------
   -- Full_Designated_Type --
   --------------------------

   function Full_Designated_Type (G : GL_Value) return Entity_Id is
     ((if Is_Reference (G) then Get_Fullest_View (G.Typ)
       else Full_Designated_Type (Etype (G))));

   ---------
   -- GEP --
   ---------

   function GEP
     (Env         : Environ;
      Result_Type : Entity_Id;
      Ptr         : GL_Value;
      Indices     : GL_Value_Array;
      Name        : String := "") return GL_Value
   is
      Val_Idxs    : Value_Array (Indices'Range);
      Result      : Value_T;
   begin
      for I in Indices'Range loop
         Val_Idxs (I) := LLVM_Value (Indices (I));
      end loop;

      Result := GEP (Env.Bld, LLVM_Value (Ptr), Val_Idxs'Address,
                     Val_Idxs'Length, Name);
      return G_Ref (Result, Result_Type);
   end GEP;

   ----------
   -- Load --
   ----------

   function Load (Env : Environ; Ptr : GL_Value; Name : String := "")
                 return GL_Value is
     (G (Load_With_Type (Env, Full_Designated_Type (Ptr),
                         LLVM_Value (Ptr), Name),
         Full_Designated_Type (Ptr)));

   -----------
   -- Store --
   -----------

   procedure Store (Env : Environ; Expr : GL_Value; Ptr : GL_Value) is
   begin
      if Has_Known_Etype (Expr) then
         Store_With_Type (Env, Etype (Expr), LLVM_Value (Expr),
                          LLVM_Value (Ptr));
      else
         Discard (Build_Store (Env.Bld, LLVM_Value (Expr), LLVM_Value (Ptr)));
      end if;
   end Store;

end GNATLLVM.GLValue;
