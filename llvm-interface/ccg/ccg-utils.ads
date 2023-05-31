------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2023, AdaCore                     --
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
with Ada.Containers; use Ada.Containers;

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with Atree; use Atree;

with LLVM.Core;   use LLVM.Core;
with LLVM.Target; use LLVM.Target;

with GNATLLVM.Types; use GNATLLVM.Types;

with CCG.Helper; use CCG.Helper;
with CCG.Strs;   use CCG.Strs;
with CCG.Target; use CCG.Target;

package CCG.Utils is

   --  When creating LLVM structs, we record what each field in the
   --  struct is for. We first say what each field is for and then
   --  say what struct it was for. We specify the type so that we can
   --  link those two. Doing it in the opposite order would make things
   --  simpler for us, but complicate the record creation process.

   procedure Set_Field_C_Info
     (UID         : Unique_Id;
      Idx         : Nat;
      Name        : Name_Id   := No_Name;
      Entity      : Entity_Id := Empty;
      Is_Padding  : Boolean   := False;
      Is_Bitfield : Boolean   := False);
   --  Say what field Idx in the struct temporarily denoted by UID is used for

   procedure Set_Struct (UID : Unique_Id; T : Type_T)
     with Pre => Present (T);
   --  Indicate that the previous calls to Set_Field_C_Info for UID
   --  were for LLVM type T.

   function Get_Field_Name (T : Type_T; Idx : Nat) return Str
     with Pre => Is_Struct_Type (T), Post => Present (Get_Field_Name'Result);
   --  Return a name to use for field Idx of LLVM struct T

   function Get_Field_Entity (T : Type_T; Idx : Nat) return Entity_Id
     with Pre => Present (T);
   --  Return the entity corresponding to for field Idx of LLVM type T

   function Is_Field_Padding (T : Type_T; Idx : Nat) return Boolean
     with Pre => Present (T);
   --  Indicate whether field Idx of LLVM type T is added for padding

   --  We do similarly for the parameters of a function

   procedure Set_Parameter (UID : Unique_Id; Idx : Nat; Entity : Entity_Id)
     with Pre => Present (Entity);
   --  Give the entity corresponding to parameter Idx of the function that
   --  will be denoted by UID

   procedure Set_Function (UID : Unique_Id; V : Value_T)
     with Pre => Present (V);
   --  Indicate that the previous calls to Set_Parameter_Info for UID
   --  were for LLVM value V.

   procedure Delete_Function_Info (V : Value_T)
     with Pre => Is_A_Function (V);
   --  Delete any mention in our tables of V

   function Get_Parameter_Entity (V : Value_T; Idx : Nat) return Entity_Id
     with Pre  => Present (V);
   --  Return the entity corresponding to for parameter Idx of LLVM value V

   function TP
     (S           : String;
      Op1         : Value_T := No_Value_T;
      Op2         : Value_T := No_Value_T;
      Op3         : Value_T := No_Value_T) return Str
     with Post => Present (TP'Result);
   --  This provides a simple template facility for insertion of operands.
   --  Every character up to '#' in S is placed in Str. '#' is followed
   --  optionally by an 'A', 'B', 'I', 'L', 'P', or 'T' and then a number.  The
   --  operand of that number is inserted into Str at that point. If 'B' is
   --  present, the operand is interpreted as a basic block. If 'L' is
   --  present, then we want the operand always written as a LHS. If 'I' is
   --  present, this is for the context of an initializer. If 'A' is
   --  present, we take the address of the operand and deference it if 'D'
   --  is present. If 'T' is present, we output the type of that operand.
   --  If 'P' is present, we use the value of the Phi temporary.

   function Is_Ref_To_Volatile (Op : Value_T) return Boolean
     with Pre => Present (Op);
   --  True if Op represents a value that we can determine to be volatile

   function Num_Uses (V : Value_T) return Nat
     with Pre => Present (V);
   --  Returns the number of uses of V

   function Is_Integral_Type (T : Type_T) return Boolean is
     (Get_Type_Kind (T) = Integer_Type_Kind)
     with Pre => Present (T);
   function Is_Integral_Type (V : Value_T) return Boolean is
     (Get_Type_Kind (Type_Of (V)) = Integer_Type_Kind)
     with Pre => Present (V);
   function Is_Function_Type (T : Type_T) return Boolean is
     (Get_Type_Kind (T) = Function_Type_Kind)
     with Pre => Present (T);
   function Is_Function_Type (V : Value_T) return Boolean is
     (Get_Type_Kind (V) = Function_Type_Kind)
     with Pre => Present (V);
   function Is_Pointer_Type (T : Type_T) return Boolean is
     (Get_Type_Kind (T) = Pointer_Type_Kind)
     with Pre => Present (T);
   function Is_Pointer_Type (V : Value_T) return Boolean is
     (Get_Type_Kind (V) = Pointer_Type_Kind)
     with Pre => Present (V);
   function Is_Simple_Type (T : Type_T) return Boolean is
     (Get_Type_Kind (T)
        in Half_Type_Kind .. Integer_Type_Kind | Pointer_Type_Kind)
     with Pre => Present (T);
   function Is_Simple_Type (V : Value_T) return Boolean is
     (Is_Simple_Type (Type_Of (V)))
     with Pre => Present (V);
   function Is_Struct_Type (T : Type_T) return Boolean is
     (Get_Type_Kind (T) = Struct_Type_Kind)
     with Pre => Present (T);
   function Is_Struct_Type (V : Value_T) return Boolean is
     (Get_Type_Kind (V) = Struct_Type_Kind)
     with Pre => Present (V);
   function Is_Array_Type (T : Type_T) return Boolean is
     (Get_Type_Kind (T) = Array_Type_Kind)
     with Pre => Present (T);
   function Is_Array_Type (V : Value_T) return Boolean is
     (Get_Type_Kind (V) = Array_Type_Kind)
     with Pre => Present (V);

   function Is_Aggregate_Type (T : Type_T) return Boolean is
     (Is_Struct_Type (T) or else Is_Array_Type (T))
     with Pre => Present (T);
   function Is_Aggregate_Type (V : Value_T) return Boolean is
     (Is_Aggregate_Type (Type_Of (V)))
     with Pre => Present (V);

   function Is_Metadata (V : Value_T) return Boolean is
     (Get_Type_Kind (V) = Metadata_Type_Kind)
     with Pre => Present (V);

   function Is_Zero_Length_Array (T : Type_T) return Boolean is
     (Is_Array_Type (T) and then Get_Array_Length (T) = Nat (0))
     with Pre => Present (T);

   function Is_Simple_Constant (V : Value_T) return Boolean is
     (Get_Value_Kind (V)
         in Constant_Int_Value_Kind | Constant_Pointer_Null_Value_Kind
            | Constant_FP_Value_Kind | Constant_Expr_Value_Kind
      or else (Is_Undef (V) and then Is_Simple_Type (Type_Of (V))))
     with Pre => Present (V);
   --  True if this is a simple enough constant that we output it in C
   --  source as a constant.
   --  ??? Strings are also simple constants, but we don't support them just
   --  yet.

   function GNAT_Type (V : Value_T) return Opt_Type_Kind_Id
     with Pre => Present (V), Inline;
   --  Get the GNAT type of V, if known

   function Is_Unsigned (V : Value_T) return Boolean
     with Pre => Present (V), Inline;
   --  True if V is known from sources to be unsigned

   function Is_Access_Subprogram (V : Value_T) return Boolean
     with Pre => Present (V), Inline;
   --  True if V is known from sources to be an access to a subprogram

   function Is_Variable
     (V : Value_T; Need_From_Source : Boolean := True) return Boolean
     with Pre => Present (V), Inline;
   --  True if V is a variable. If Need_From_Source, it also must be
   --  present in the source code.

   function Is_Volatile (V : Value_T) return Boolean
     with Pre => Present (V), Inline;
   --  True if V is a variable that's to be treated as volatile

   function Has_Side_Effects (V : Value_T) return Boolean
     with Pre => Present (V);
   --  True if V may have side effects. We take a very conservative view

   function Has_Operands (V : Value_T) return Boolean is
     (Is_A_Instruction (V) or else Is_A_Constant_Expr (V))
     with Pre => Present (V);

   function Is_Unc_Br (V : Value_T) return Boolean is
     (Is_A_Branch_Inst (V) and then not Is_Conditional (V))
     with Pre => Present (V);

   function Is_Cond_Br (V : Value_T) return Boolean is
     (Is_A_Branch_Inst (V) and then Is_Conditional (V))
     with Pre => Present (V);

   function Opt_Full_Base_Type
     (TE : Opt_Void_Or_Type_Kind_Id) return Opt_Void_Or_Type_Kind_Id
   is
     (if Present (TE) then Full_Base_Type (TE) else Empty);
   --  Version of Full_Base_Type that handles an Empty input

   function Opt_Full_Etype
     (E : Opt_N_Has_Etype_Id) return Opt_Void_Or_Type_Kind_Id
   is
     (if Present (E) then Full_Etype (E) else Empty);
   --  Version of Full_Etype that handles an Empty input

   function Opt_Is_Unsigned_Type
    (TE : Opt_Void_Or_Type_Kind_Id) return Boolean
   is
     (Present (TE) and then Is_Unsigned_Type (TE));
   --  Likewise for Is_Unsigned_Type

   --  LLVM uses a zero-length array to indicate a variable-length array.
   --  Versions of C older that C99 don't permit zero-element arrays. It's
   --  tempting to use a pointer to the element type instead of a pointer
   --  to the array. In this LLVM usage, we never have any objects of that
   --  array type. However, Ada can have arrays of zero length and can have
   --  objects of that length and doing the above conversion will cause
   --  confusion there. So we instead interpret an array of length zero as
   --  an array of length one.

   function Effective_Array_Length (T : Type_T) return Nat is
     (if   C_Version < 1999 and then Get_Array_Length (T) = 0
      then 1 else Get_Array_Length (T))
      with Pre => Present (T);

   function UC_V is new Ada.Unchecked_Conversion (Value_T, System.Address);
   function UC_T is new Ada.Unchecked_Conversion (Type_T, System.Address);
   function UC_B is new Ada.Unchecked_Conversion (Basic_Block_T,
                                                  System.Address);

   function Hash (V : Value_T)       return Hash_Type is
     (Hash_Type'Mod (To_Integer (UC_V (V)) / (V'Size / 8)));
   function Hash (T : Type_T)        return Hash_Type is
     (Hash_Type'Mod (To_Integer (UC_T (T)) / (T'Size / 8)))
     with Pre => Present (T);
   function Hash (B : Basic_Block_T) return Hash_Type is
     (Hash_Type'Mod (To_Integer (UC_B (B)) / (B'Size / 8)))
     with Pre => Present (B);
   --  Hash functions for LLVM values, types, and basic blocks

   --  We want to compute a hash code for a Str_Component_Array that will be
   --  the same no matter how we break up a concatentation of strings
   --  that do not involve a Value_T, so we don't want to use Ada.Strings.Hash
   --  but instead accumulate the hash value piece by piece.

   procedure Update_Hash (H : in out Hash_Type; Key : Hash_Type) with Inline;
   --  Update H by including the value of Key

   procedure Update_Hash (H : in out Hash_Type; S : String)      with Inline;
   --  Update H taking into account the characters in S

   procedure Update_Hash (H : in out Hash_Type; B : Boolean)      with Inline;
   --  Update H taking into account the value of B

   procedure Update_Hash (H : in out Hash_Type; V : Value_T)
     with Pre => Present (V), Inline;
   --  Update H taking into account the value V

   procedure Update_Hash (H : in out Hash_Type; T : Type_T)
     with Pre => Present (T), Inline;
   --  Update H taking into account the type T

   procedure Update_Hash (H : in out Hash_Type; B : Basic_Block_T)
     with Pre => Present (B), Inline;
   --  Update H taking into account the type T

   function Get_Scalar_Bit_Size (T : Type_T) return Nat is
     (Nat (Size_Of_Type_In_Bits (Module_Data_Layout, T)))
     with Pre => Present (T);

   function Get_Scalar_Bit_Size (V : Value_T) return Nat is
     (Get_Scalar_Bit_Size (Type_Of (V)))
     with Pre => Present (V);

   function Int_Ty (Num_Bits : ULL) return Type_T is
     (Int_Type (unsigned (Num_Bits)))
     with Post => Is_Integral_Type (Int_Ty'Result);

   function Single_User (V : Value_T) return Value_T is
     ((if Num_Uses (V) = 1 then Get_User (Get_First_Use (V)) else No_Value_T))
     with Pre => Present (V);
   --  If V has only one user, return it

   function Safe_Single_User (V : Value_T) return Value_T
     with Pre => Present (V);
   --  Likewise, but only return it if V is an instruction, the single
   --  user is an instruction, both are in the same basic block, and
   --  there are no instructions with side effects between them.

   procedure Error_Msg (Msg : String; V : Value_T);
   --  Post an error message via the GNAT errout mechanism. If V
   --  corresponds to an entity from the front end, post it on
   --  that. Otherwise, posted it on the main unit start sloc and if V has
   --  an associated debug location, append this location to the error
   --  message as " at file:line".

   procedure Error_Msg (Msg : String; T : Type_T);
   --  Similarly, but for a type

   generic
      with procedure Process (V : Value_T) is <>;
   procedure Walk_Object (V : Value_T)
     with Pre => Present (V);
   --  Call Process for each value within V

   function Int_Type_String (Size : Pos; Unsigned_P : Boolean) return Str;
   --  Return the string corresponding to the C name of an integer type of
   --  Size bits and the specified signedness.

   function NULL_String return String is
     (if Have_Includes then "NULL" else "(void *) 0");

end CCG.Utils;
