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

with Einfo; use Einfo;
with Types; use Types;

with LLVM.Types; use LLVM.Types;

with GNATLLVM.Environment; use GNATLLVM.Environment;

package GNATLLVM.Arrays is

   procedure Extract_Array_Info
     (Env         : Environ;
      Array_Node  : Node_Id;
      Array_Descr : out Value_T;
      Array_Type  : out Entity_Id);
   --  Set Array_Type to the type of Array_Node. If it is a constrained array,
   --  set Array_Descr to No_Value_T, or emit the value corresponding to
   --  Array_Node if it is unconstrained.

   function Array_Size
     (Env                        : Environ;
      Array_Descr                : Value_T;
      Array_Type                 : Entity_Id;
      Containing_Record_Instance : Value_T := No_Value_T) return Value_T;
   --  Return the number of elements contained in an Array_Type object as an
   --  integer as large as a pointer for the target architecture. If it is an
   --  unconstrained array, Array_Descr must be an expression that evaluates
   --  to the array. If Array_Type is constrained by record discriminants,
   --  use Containing_Record_Instance to get its bounds.

   type Bound_T is (Low, High);

   function Array_Bound
     (Env         : Environ;
      Array_Descr : Value_T;
      Array_Type  : Entity_Id;
      Bound       : Bound_T;
      Dim         : Natural := 1) return Value_T;
   --  Compute the bound for the array corresponding to Array_Descr, whose type
   --  is Array_Type. If Array_Type is a constrained array, Array_Descr will
   --  not be used, and can thus then be No_Value_T. Otherwise, it will be
   --  used to compute the bound at runtime.

   function Array_Length
     (Env         : Environ;
      Array_Descr : Value_T;
      Array_Type  : Entity_Id) return Value_T;
   --  Emit code to compute the length for the array corresponding to
   --  Array_Descr, whose type is Array_Type. If Array_Type is a constrained
   --  array, Array_Descr will not be used, and can thus then be No_Value_T.
   --  Otherwise, it will be used to compute the length at runtime.

   function Array_Data
     (Env         : Environ;
      Array_Descr : Value_T;
      Array_Type  : Entity_Id) return Value_T;
   --  Emit code to compute the address of the array data and return the
   --  corresponding value. Handle both constrained and unconstrained arrays,
   --  depending on Array_Type. If this is a constrained array, Array_Descr
   --  must already be a pointer to the array data, otherwise, it must be a
   --  fat pointer.

   function Array_Fat_Pointer
     (Env        : Environ;
      Array_Data : Value_T;
      Array_Node : Node_Id;
      Array_Type : Entity_Id) return Value_T
     with Pre => Is_Constrained (Array_Type);
   --  Wrap a fat pointer around Array_Data according to its type Array_Type
   --  and return the created fat pointer.

   function Array_Address
     (Env        : Environ;
      Array_Data : Value_T;
      Array_Type : Entity_Id) return Value_T;
   --  Return the pointer to the first element of Array_Data

end GNATLLVM.Arrays;
