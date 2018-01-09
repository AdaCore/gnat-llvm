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

with Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

with Types; use Types;

with LLVM.Core; use LLVM.Core;
with LLVM.Types; use LLVM.Types;

with Atree; use Atree;
with Einfo; use Einfo;

with GNATLLVM.Environment;  use GNATLLVM.Environment;
with GNATLLVM.Nested_Subps; use GNATLLVM.Nested_Subps;
with GNATLLVM.Utils; use GNATLLVM.Utils;
with Get_Targ; use Get_Targ;

package GNATLLVM.Types is

   pragma Annotate (Xcov, Exempt_On, "Defensive programming");
   procedure Register_Builtin_Types (Env : Environ);

   function Create_Access_Type
     (Env : Environ; TE : Entity_Id) return Type_T
     with Pre => Is_Type (TE);
   --  Function that creates the access type for a corresponding type. Since
   --  access types are not just pointers, this is the abstraction bridge
   --  between the two. For the moment, it handles array accesses and thin
   --  (normal) accesses.

   function Create_Array_Thin_Pointer_Type
     (Env        : Environ;
      Array_Type : Entity_Id) return Type_T;
   --  Return the type used to store thin pointers to Array_Type

   function Create_Array_Fat_Pointer_Type
     (Env        : Environ;
      Array_Type : Entity_Id) return Type_T;
   --  Return the type used to store fat pointers to Array_Type

   function Create_Array_Bounds_Type
     (Env             : Environ;
      Array_Type_Node : Entity_Id) return Type_T;
   --  Helper that returns the type used to store array bounds. This is a
   --  structure that that follows the following pattern: { LB0, UB0, LB1,
   --  UB1, ... }

   function Create_Subprogram_Type_From_Spec
     (Env       : Environ;
      Subp_Spec : Node_Id) return Type_T;

   function Create_Subprogram_Type_From_Entity
     (Env           : Environ;
      Subp_Type_Ent : Entity_Id;
      Takes_S_Link  : Boolean) return Type_T;

   function Create_Type (Env : Environ; TE : Entity_Id) return Type_T
     with Pre => Is_Type (TE);

   procedure Create_Discrete_Type
     (Env       : Environ;
      TE        : Entity_Id;
      TL        : out Type_T;
      Low, High : out Value_T)
     with Pre => Ekind (TE) in Discrete_Kind;

   function Create_Static_Link_Type
     (Env         : Environ;
      S_Link_Desc : Static_Link_Descriptor) return Type_T;
   --  Return an LLVM type for the structure used to implement the statick
   --  link.

   function Int_Ty (Num_Bits : Natural) return Type_T;
   function Fn_Ty (Param_Ty : Type_Array; Ret_Ty : Type_T) return Type_T;

   function Get_Innermost_Component_Type
     (Env : Environ; N : Entity_Id) return Type_T;

   function Get_Address_Type return Type_T;
   pragma Annotate (Xcov, Exempt_Off, "Defensive programming");

   function Int_Ptr_Type return Type_T is
      (Int_Type (Interfaces.C.unsigned (Get_Pointer_Size)));

   function Get_Type_Size_In_Bits
     (Env : Environ;
      T   : Type_T) return unsigned_long_long;
   --  Return the size of an LLVM type, in bits

   function Get_Type_Alignment
     (Env : Environ;
      T   : Type_T) return Interfaces.C.unsigned;
   --  Return the size of an LLVM type, in bits

   function Get_Type_Size
     (Env : Environ;
      T   : Type_T) return Value_T;
   --  Return the size of an LLVM type, in bytes

   function Emit_Type_Size
     (Env                   : Environ;
      T                     : Entity_Id;
      Array_Descr           : Value_T;
      Containing_Record_Ptr : Value_T) return Value_T;
   --  Emit code to compute the size of type T, getting information from
   --  Containing_Record_Ptr for types that are constrained by a discriminant
   --  record (in such case, this parameter should be a pointer to the
   --  corresponding record). If T is an unconstrained array, Array_Descr must
   --  be the corresponding fat pointer. Return the computed size as value.

   function Record_Field_Offset
     (Env : Environ;
      Record_Ptr : Value_T;
      Record_Field : Node_Id) return Value_T;
   --  Compute the offset of a given record field

end GNATLLVM.Types;
