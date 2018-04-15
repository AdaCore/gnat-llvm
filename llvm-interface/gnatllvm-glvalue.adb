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

   function Access_Depth (TE : Entity_Id) return Natural
     with Pre => Present (TE);
   --  If TE is not an access type, return zero.  Otherwise, return how deep
   --  we have to go down Full_Designated_Type to find something that's
   --  not an access type.

   function Access_Depth (G : GL_Value) return Natural
     with Pre => Present (G);
   --  Similarly, but for a GL_Value, which might be a reference

   function Is_LValue_Of (G : GL_Value; TE : Entity_Id) return Boolean
     with Pre => Present (G) and then Is_Type_Or_Void (TE);
   --  Return True if G is the LValue of an object of type TE

   ------------------
   -- Access_Depth --
   ------------------

   function Access_Depth (TE : Entity_Id) return Natural is
   begin
      if not Is_Access_Type (TE) then
         return 0;
      else
         return Access_Depth (Full_Designated_Type (TE)) + 1;
      end if;
   end Access_Depth;

   ------------------
   -- Access_Depth --
   ------------------

   function Access_Depth (G : GL_Value) return Natural is
   begin
      if not Is_Access_Type (G) then
         return 0;
      else
         return Access_Depth (Full_Designated_Type (G)) + 1;
      end if;
   end Access_Depth;

   ------------------
   -- Is_LValue_Of --
   ------------------

   function Is_LValue_Of (G : GL_Value; TE : Entity_Id) return Boolean is
   begin
      --  If Value is a reference and its designated type is that of our
      --  type, we know we're OK.  If not, we may still if OK if access
      --  types are involved, so check that the "access type" depth of
      --  Value is one greater than that of our type.  That's also OK.
      --  And if what we have is a subprogram, we're also OK.

      return Ekind (TE) = E_Void
        or else (Is_Reference (G)
                   and then (Full_Designated_Type (G) = TE
                               or else (Ekind (Full_Designated_Type (G))
                                          = E_Subprogram_Type)))
        or else Access_Depth (G) = Access_Depth (TE) + 1;
   end Is_LValue_Of;

   ----------------
   -- Need_Value --
   ----------------

   function Need_Value
     (Env : Environ; V : GL_Value; TE : Entity_Id) return GL_Value is
   begin
      --  If V is of dynamic size, the "value" we use is the
      --  reference, so return it.  Similarly for subprograms.
      --  Likewise if it's not a reference.  Otherwise, load the
      --  value.

      if Ekind (TE) = E_Subprogram_Type or else not Is_LValue_Of (V, TE)
        or else (Is_Reference (V)
                   and then Is_Dynamic_Size (Env, Full_Designated_Type (V)))
      then
         return V;
      else
         return Load (Env, V);
      end if;
   end Need_Value;

   -----------------
   -- Need_LValue --
   -----------------

   function Need_LValue
     (Env : Environ; V : GL_Value; TE : Entity_Id) return GL_Value is
   begin

      --  If we already have an LValue, return it.  Otherwise, allocate memory
      --  for the value (which we know to be of fixed size or else we'd have
      --  had a reference), store the data, and return the address of that
      --  temporary.

      if Is_LValue_Of (V, TE) then
         return V;
      else
         pragma Assert (not Library_Level (Env));

         declare
            Temp : constant GL_Value := Allocate_For_Type (Env, TE, "lvalue");
         begin
            Store (Env, V, Temp);
            return Temp;
         end;
      end if;
   end Need_LValue;

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

   ---------------
   -- Get_Undef --
   ---------------

   function Get_Undef (Env : Environ; TE : Entity_Id) return GL_Value is
     (G (Get_Undef (Create_Type (Env, TE)), TE));

   -------------------
   -- Get_Undef_Ref --
   -------------------

   function Get_Undef_Ref (Env : Environ; TE : Entity_Id) return GL_Value is
     (G (Get_Undef (Create_Access_Type (Env, TE)), TE, Is_Reference => True));

   ----------------
   -- Const_Null --
   ----------------

   function Const_Null (Env : Environ; TE : Entity_Id) return GL_Value is
     (G (Const_Null (Create_Type (Env, TE)), TE));

   --------------------
   -- Const_Null_Ptr --
   --------------------

   function Const_Null_Ptr (Env : Environ; TE : Entity_Id) return GL_Value is
     (G (Const_Null (Pointer_Type (Create_Type (Env, TE), 0)), TE,
         Is_Reference => True));

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
   -- Build_Ret --
   ---------------

   procedure Build_Ret (Env : Environ; G : GL_Value) is
   begin
      Discard (Build_Ret (Env.Bld, LLVM_Value (G)));
   end Build_Ret;

   --------------------
   -- Build_Ret_Void --
   --------------------

   procedure Build_Ret_Void (Env : Environ) is
   begin
      Discard (Build_Ret_Void (Env.Bld));
   end Build_Ret_Void;

   -----------------------
   -- Build_Unreachable --
   -----------------------

   procedure Build_Unreachable (Env : Environ) is
   begin
      Discard (Build_Unreachable (Env.Bld));
   end Build_Unreachable;

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
      for J in Values'Range loop
         Values (J) := LLVM_Value (GL_Values (J));
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
      for J in Indices'Range loop
         Val_Idxs (J) := LLVM_Value (Indices (J));
      end loop;

      Result := In_Bounds_GEP (Env.Bld, LLVM_Value (Ptr), Val_Idxs'Address,
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

   ----------
   -- Call --
   ----------

   function Call
     (Env         : Environ;
      Func        : GL_Value;
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

      Result := Call (Env.Bld, LLVM_Value (Func),
                      Arg_Values'Address, Arg_Values'Length, Name);
      return G (Result, Result_Type);
   end Call;

end GNATLLVM.GLValue;
