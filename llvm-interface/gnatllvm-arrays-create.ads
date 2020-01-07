------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2020, AdaCore                     --
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

package GNATLLVM.Arrays.Create is

   function Create_Array_Type
     (TE : Entity_Id; For_Orig : Boolean := False) return Type_T
     with Pre  => (if   For_Orig then Is_Packed_Array_Impl_Type (TE)
                   else Is_Array_Type (TE)),
          Post => Present (Create_Array_Type'Result);
   --  Return the type used to represent Array_Type_Node.  This will be
   --  an opaque type if LLVM can't represent it directly.  If For_Orig
   --  is True, set the array info for the Original_Record_Type of TE.

   function Create_Array_Fat_Pointer_Type (GT : GL_Type) return Type_T
     with Pre  => Is_Array_Or_Packed_Array_Type (GT),
          Post => Present (Create_Array_Fat_Pointer_Type'Result);
   function Create_Array_Fat_Pointer_Type (TE : Entity_Id) return Type_T
     with Pre  => Is_Array_Or_Packed_Array_Type (TE),
          Post => Present (Create_Array_Fat_Pointer_Type'Result);
   --  Return the type used to store fat pointers to Array_Type

   function Create_Array_Bounds_Type (TE : Entity_Id) return Type_T
     with Pre  => Is_Array_Or_Packed_Array_Type (TE),
          Post => Present (Create_Array_Bounds_Type'Result);
   --  Return the type used to store array bounds. This is a structure
   --  that that follows the following pattern: { LB0, UB0, LB1, UB1, ... }

   function Create_Array_Bounds_And_Data_Type
     (TE : Entity_Id; T : Type_T) return Type_T
     with Pre  => Is_Array_Or_Packed_Array_Type (TE) and then Present (T),
     Post => Present (Create_Array_Bounds_And_Data_Type'Result);
   --  Return the type used to store the bounds and data of an array

end GNATLLVM.Arrays.Create;
