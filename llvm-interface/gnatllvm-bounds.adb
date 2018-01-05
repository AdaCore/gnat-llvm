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
with LLVM.Core; use LLVM.Core;

package body GNATLLVM.Bounds is

   ----------------------
   -- Bounds_To_Length --
   ----------------------

   function Bounds_To_Length
     (Env                   : Environ;
      Low_Bound, High_Bound : Value_T;
      Bounds_Type           : Entity_Id) return Value_T
   is
      Result_Type : constant Type_T := Type_Of (Low_Bound);

      Is_Bound_Unsigned  : constant Boolean :=
        Is_Unsigned_Type (Bounds_Type);
      Is_Empty         : constant Value_T :=
        I_Cmp
          (Env.Bld,
           (if Is_Bound_Unsigned then Int_UGT else Int_SGT),
           Low_Bound, High_Bound, "is-array-empty");

   begin
      return Build_Select
        (Env.Bld,
         C_If   => Is_Empty,
         C_Then => Const_Null (Result_Type),
         C_Else =>
           Add
             (Env.Bld,
              Sub (Env.Bld, High_Bound, Low_Bound, ""),
              Const_Int (Result_Type, 1, Sign_Extend => False),
              ""),
         Name   => "");
   end Bounds_To_Length;

end GNATLLVM.Bounds;
