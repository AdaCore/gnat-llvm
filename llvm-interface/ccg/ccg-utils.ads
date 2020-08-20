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

with Ada.Unchecked_Conversion;
with Ada.Containers; use Ada.Containers;

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with GNATLLVM; use GNATLLVM;

package CCG.Utils is

   function UC_V is new Ada.Unchecked_Conversion (Value_T, System.Address);
   function UC_T is new Ada.Unchecked_Conversion (Type_T, System.Address);
   function UC_B is new Ada.Unchecked_Conversion (Basic_Block_T,
                                                  System.Address);

   function Hash (V : Value_T)       return Hash_Type is
     (Hash_Type'Mod (To_Integer (UC_V (V)) / (V'Size / 8)))
     with Pre => Present (V);
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

   procedure Update_Hash (H : in out Hash_Type; V : Value_T)
     with Pre => Present (V), Inline;
   --  Update H taking into account the value V

   procedure Update_Hash (H : in out Hash_Type; T : Type_T)
     with Pre => Present (T), Inline;
   --  Update H taking into account the type T

   procedure Update_Hash (H : in out Hash_Type; B : Basic_Block_T)
     with Pre => Present (B), Inline;
   --  Update H taking into account the type T

end CCG.Utils;
