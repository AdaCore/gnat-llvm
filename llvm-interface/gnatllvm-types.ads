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

with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

with Atree; use Atree;
with Einfo; use Einfo;
with Sinfo; use Sinfo;
with Types; use Types;

with LLVM.Core;   use LLVM.Core;
with LLVM.Target; use LLVM.Target;
with LLVM.Types;  use LLVM.Types;

with GNATLLVM.Environment;  use GNATLLVM.Environment;
with GNATLLVM.Utils;        use GNATLLVM.Utils;

with Get_Targ; use Get_Targ;

package GNATLLVM.Types is

   pragma Annotate (Xcov, Exempt_On, "Defensive programming");

   function Get_Fullest_View (E : Entity_Id) return Entity_Id
     with Pre => Is_Type (E), Post => Is_Type (Get_Fullest_View'Result);
   --  Get the fullest possible view of E, looking through private,
   --  limited, and packed array implementation types.

   function Full_Etype (N : Node_Id) return Entity_Id is
      (if Ekind (Etype (N)) = E_Void then Etype (N)
       else Get_Fullest_View (Etype (N)));

   function Create_Access_Type
     (Env : Environ; TE : Entity_Id) return Type_T
     with Pre  => Env /= null and then Is_Type (TE),
          Post => Present (Create_Access_Type'Result);

   --  Function that creates the access type for a corresponding type. Since
   --  access types are not just pointers, this is the abstraction bridge
   --  between the two. For the moment, it handles array accesses and thin
   --  (normal) accesses.

   function Create_Subprogram_Type_From_Spec
     (Env       : Environ;
      Subp_Spec : Node_Id) return Type_T
     with Pre  => Env /= null and then Present (Subp_Spec),
          Post => (Get_Type_Kind (Create_Subprogram_Type_From_Spec'Result) =
                   Function_Type_Kind);

   function Create_Subprogram_Type_From_Entity
     (Env           : Environ;
      Subp_Type_Ent : Entity_Id;
      Takes_S_Link  : Boolean) return Type_T
     with Pre  => Env /= null
                  and then Ekind (Subp_Type_Ent) = E_Subprogram_Type,
          Post => (Get_Type_Kind (Create_Subprogram_Type_From_Entity'Result) =
                   Function_Type_Kind);

   function GNAT_To_LLVM_Type
     (Env : Environ; TE : Entity_Id; Definition : Boolean) return Type_T
     with Pre  => Env /= null and then Is_Type (TE),
          Post => Present (GNAT_To_LLVM_Type'Result);

   function Create_Type (Env : Environ; TE : Entity_Id) return Type_T is
      (GNAT_To_LLVM_Type (Env, TE, False));

   function Create_TBAA (Env : Environ; TE : Entity_Id) return Metadata_T
     with Pre => Env /= null and then Is_Type (TE);

   procedure Create_Discrete_Type
     (Env : Environ; TE : Entity_Id; TL : out Type_T; Low, High : out GL_Value)
     with Pre  => Env /= null and then Ekind (TE) in Discrete_Kind,
          Post => Present (TL) and then Present (Low) and then Present (High);

   function Int_Ty (Num_Bits : Natural) return Type_T
     with Post => Get_Type_Kind (Int_Ty'Result) = Integer_Type_Kind;
   function Fn_Ty (Param_Ty : Type_Array; Ret_Ty : Type_T) return Type_T
     with Pre  => Present (Ret_Ty),
          Post => Get_Type_Kind (Fn_Ty'Result) = Function_Type_Kind;

   function Int_Ptr_Type return Type_T is
      (Int_Type (unsigned (Get_Pointer_Size)));

   function Get_LLVM_Type_Size
     (Env : Environ; T : Type_T) return unsigned_long_long
   is
     ((Size_Of_Type_In_Bits (Env.Module_Data_Layout, T) + 7) / 8)
     with Pre => Env /= null and then Present (T);
   --  Return the size of an LLVM type, in bytes

   function Get_LLVM_Type_Size
     (Env : Environ; T : Type_T) return GL_Value
   is
     (Const_Int (Env, Env.Size_Type, Get_LLVM_Type_Size (Env, T), False));
   --  Return the size of an LLVM type, in bytes, as an LLVM constant

   function Get_LLVM_Type_Size_In_Bits
     (Env : Environ; T : Type_T) return unsigned_long_long
   is
     (Size_Of_Type_In_Bits (Env.Module_Data_Layout, T))
     with Pre => Env /= null and then Present (T);
   --  Return the size of an LLVM type, in bits

   function Get_LLVM_Type_Size_In_Bits
     (Env : Environ; G : GL_Value) return unsigned_long_long
   is
     (Size_Of_Type_In_Bits (Env.Module_Data_Layout, Type_Of (G.Value)))
     with Pre => Env /= null and then Present (G);
   --  Return the size of an LLVM type, in bits

   function Get_LLVM_Type_Size_In_Bits
     (Env : Environ; T : Type_T) return GL_Value
   is
     (Const_Int (Env, Env.Size_Type,
                 Get_LLVM_Type_Size_In_Bits (Env, T), False))
     with Pre  => Env /= null and Present (T),
          Post => Present (Get_LLVM_Type_Size_In_Bits'Result);
   --  Return the size of an LLVM type, in bits, as an LLVM constant

   function Get_LLVM_Type_Size_In_Bits
     (Env : Environ; TE : Entity_Id) return GL_Value
     with Pre  => Env /= null and then Present (TE),
          Post => Present (Get_LLVM_Type_Size_In_Bits'Result);
   --  Likewise, but convert from a GNAT type

   function Get_LLVM_Type_Size_In_Bits
     (Env : Environ; G : GL_Value) return GL_Value
   is
     (Get_LLVM_Type_Size_In_Bits (Env, G.Typ))
     with Pre  => Env /= null and then Present (G),
          Post => Present (Get_LLVM_Type_Size_In_Bits'Result);
   --  Variant of above to get type from a GL_Value

   function Convert_To_Size_Type (Env : Environ; V : GL_Value) return GL_Value
     with Pre  => Env /= null and then Present (V),
          Post => Type_Of (Convert_To_Size_Type'Result) = Env.LLVM_Size_Type
                  and then Convert_To_Size_Type'Result.Typ = Env.Size_Type;
   --  Convert V to Size_Type

   function Get_Type_Alignment
     (Env : Environ;
      T   : Type_T) return unsigned is
     (ABI_Alignment_Of_Type (Env.Module_Data_Layout, T))
     with Pre => Env /= null and then Present (T);
   --  Return the size of an LLVM type, in bits

   function Get_Type_Size
     (Env      : Environ;
      TE       : Entity_Id;
      V        : GL_Value;
      For_Type : Boolean := False) return GL_Value
     with Pre  => Env /= null and then Is_Type (TE)
                  and then (not For_Type or else No (V)),
          Post => Present (Get_Type_Size'Result);
   --  Return the size of a type, in bytes, as a GL_Value.  If TE is
   --  an unconstrained array type, V must be the value of the array.

   function Record_Field_Offset
     (Env          : Environ;
      Record_Ptr   : Value_T;
      Record_Field : Node_Id) return Value_T
     with Pre  => Env /= null and then Present (Record_Ptr)
                  and then Present (Record_Field),
          Post => Present (Record_Field_Offset'Result);
   --  Compute the offset of a given record field

   function Record_With_Dynamic_Size
     (Env : Environ; T : Entity_Id) return Boolean
     with Pre => Env /= null and then Is_Type (T);
   --  Return True is T denotes a record type with a dynamic size

end GNATLLVM.Types;
