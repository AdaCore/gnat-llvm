------------------------------------------------------------------------------
--                              C C G                                       --
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

with Interfaces.C;

with LLVM.Core;  use LLVM.Core;

with GNATLLVM; use GNATLLVM;

package CCG.Helper is

   subtype Double   is Interfaces.C.double;
   subtype unsigned is Interfaces.C.unsigned;

   --  This package contains helper subprograms of the same name as LLVM
   --  API subprograms, but with different parameter or return types or
   --  other minor changes.

   function Const_Real_Get_Double
     (V : Value_T; Loses_Info : out Boolean) return Double
     with Pre => Present (V);

   function Count_Params (V : Value_T) return Nat is
     (Nat (unsigned'(Count_Params (V))))
     with Pre => Present (V);

   function Count_Param_Types (T : Type_T) return Nat is
     (Nat (unsigned'(Count_Param_Types (T))))
     with Pre => Present (T);

   function Get_Param (V : Value_T; P_Num : Nat) return Value_T is
     (Get_Param (V, unsigned (P_Num)))
     with Pre => Present (V), Post => Present (Get_Param'Result);

   function Get_Num_Operands (V : Value_T) return Int is
      (Int (Interfaces.C.int'(Get_Num_Operands (V))))
      with Pre => Present (V);

   function Is_A_Constant (V : Value_T) return Boolean is
     (Present (Is_A_Constant (V)))
     with Pre => Present (V);

   function Is_A_Constant_Int (V : Value_T) return Boolean is
     (Present (Is_A_Constant_Int (V)))
     with Pre => Present (V);

   function Is_A_Constant_FP (V : Value_T) return Boolean is
     (Present (Is_A_Constant_FP (V)))
     with Pre => Present (V);

   function Is_A_Function (V : Value_T) return Boolean is
     (Present (Is_A_Function (V)))
     with Pre => Present (V);

   function Is_A_Argument (V : Value_T) return Boolean is
     (Present (Is_A_Argument (V)))
     with Pre => Present (V);

   function Is_A_Instruction (V : Value_T) return Boolean is
     (Present (Is_A_Instruction (V)))
     with Pre => Present (V);

   function Is_A_Global_Variable (V : Value_T) return Boolean is
     (Present (Is_A_Global_Variable (V)))
     with Pre => Present (V);

end CCG.Helper;
