pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package stdint_h is

   INT8_MIN : constant := (-128);  --  /usr/include/stdint.h:155
   INT16_MIN : constant := (-32767-1);  --  /usr/include/stdint.h:156
   INT32_MIN : constant := (-2147483647-1);  --  /usr/include/stdint.h:157
   --  unsupported macro: INT64_MIN (-__INT64_C(9223372036854775807)-1)

   INT8_MAX : constant := (127);  --  /usr/include/stdint.h:160
   INT16_MAX : constant := (32767);  --  /usr/include/stdint.h:161
   INT32_MAX : constant := (2147483647);  --  /usr/include/stdint.h:162
   --  unsupported macro: INT64_MAX (__INT64_C(9223372036854775807))

   UINT8_MAX : constant := (255);  --  /usr/include/stdint.h:166
   UINT16_MAX : constant := (65535);  --  /usr/include/stdint.h:167
   UINT32_MAX : constant := (4294967295);  --  /usr/include/stdint.h:168
   --  unsupported macro: UINT64_MAX (__UINT64_C(18446744073709551615))

   INT_LEAST8_MIN : constant := (-128);  --  /usr/include/stdint.h:173
   INT_LEAST16_MIN : constant := (-32767-1);  --  /usr/include/stdint.h:174
   INT_LEAST32_MIN : constant := (-2147483647-1);  --  /usr/include/stdint.h:175
   --  unsupported macro: INT_LEAST64_MIN (-__INT64_C(9223372036854775807)-1)

   INT_LEAST8_MAX : constant := (127);  --  /usr/include/stdint.h:178
   INT_LEAST16_MAX : constant := (32767);  --  /usr/include/stdint.h:179
   INT_LEAST32_MAX : constant := (2147483647);  --  /usr/include/stdint.h:180
   --  unsupported macro: INT_LEAST64_MAX (__INT64_C(9223372036854775807))

   UINT_LEAST8_MAX : constant := (255);  --  /usr/include/stdint.h:184
   UINT_LEAST16_MAX : constant := (65535);  --  /usr/include/stdint.h:185
   UINT_LEAST32_MAX : constant := (4294967295);  --  /usr/include/stdint.h:186
   --  unsupported macro: UINT_LEAST64_MAX (__UINT64_C(18446744073709551615))

   INT_FAST8_MIN : constant := (-128);  --  /usr/include/stdint.h:191

   INT_FAST16_MIN : constant := (-9223372036854775807-1);  --  /usr/include/stdint.h:193
   INT_FAST32_MIN : constant := (-9223372036854775807-1);  --  /usr/include/stdint.h:194
   --  unsupported macro: INT_FAST64_MIN (-__INT64_C(9223372036854775807)-1)

   INT_FAST8_MAX : constant := (127);  --  /usr/include/stdint.h:201

   INT_FAST16_MAX : constant := (9223372036854775807);  --  /usr/include/stdint.h:203
   INT_FAST32_MAX : constant := (9223372036854775807);  --  /usr/include/stdint.h:204
   --  unsupported macro: INT_FAST64_MAX (__INT64_C(9223372036854775807))

   UINT_FAST8_MAX : constant := (255);  --  /usr/include/stdint.h:212

   UINT_FAST16_MAX : constant := (18446744073709551615);  --  /usr/include/stdint.h:214
   UINT_FAST32_MAX : constant := (18446744073709551615);  --  /usr/include/stdint.h:215
   --  unsupported macro: UINT_FAST64_MAX (__UINT64_C(18446744073709551615))

   INTPTR_MIN : constant := (-9223372036854775807-1);  --  /usr/include/stdint.h:225
   INTPTR_MAX : constant := (9223372036854775807);  --  /usr/include/stdint.h:226
   UINTPTR_MAX : constant := (18446744073709551615);  --  /usr/include/stdint.h:227
   --  unsupported macro: INTMAX_MIN (-__INT64_C(9223372036854775807)-1)
   --  unsupported macro: INTMAX_MAX (__INT64_C(9223372036854775807))
   --  unsupported macro: UINTMAX_MAX (__UINT64_C(18446744073709551615))

   PTRDIFF_MIN : constant := (-9223372036854775807-1);  --  /usr/include/stdint.h:248
   PTRDIFF_MAX : constant := (9223372036854775807);  --  /usr/include/stdint.h:249

   SIG_ATOMIC_MIN : constant := (-2147483647-1);  --  /usr/include/stdint.h:256
   SIG_ATOMIC_MAX : constant := (2147483647);  --  /usr/include/stdint.h:257

   SIZE_MAX : constant := (18446744073709551615);  --  /usr/include/stdint.h:261
   --  unsupported macro: WCHAR_MIN __WCHAR_MIN
   --  unsupported macro: WCHAR_MAX __WCHAR_MAX

   WINT_MIN : constant := (0);  --  /usr/include/stdint.h:278
   WINT_MAX : constant := (4294967295);  --  /usr/include/stdint.h:279
   --  arg-macro: procedure INT8_C (c)
   --    c
   --  arg-macro: procedure INT16_C (c)
   --    c
   --  arg-macro: procedure INT32_C (c)
   --    c
   --  unsupported macro: INT64_C(c) c ## L
   --  arg-macro: procedure UINT8_C (c)
   --    c
   --  arg-macro: procedure UINT16_C (c)
   --    c
   --  unsupported macro: UINT32_C(c) c ## U
   --  unsupported macro: UINT64_C(c) c ## UL
   --  unsupported macro: INTMAX_C(c) c ## L
   --  unsupported macro: UINTMAX_C(c) c ## UL

  -- Copyright (C) 1997-2016 Free Software Foundation, Inc.
  --   This file is part of the GNU C Library.
  --   The GNU C Library is free software; you can redistribute it and/or
  --   modify it under the terms of the GNU Lesser General Public
  --   License as published by the Free Software Foundation; either
  --   version 2.1 of the License, or (at your option) any later version.
  --   The GNU C Library is distributed in the hope that it will be useful,
  --   but WITHOUT ANY WARRANTY; without even the implied warranty of
  --   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  --   Lesser General Public License for more details.
  --   You should have received a copy of the GNU Lesser General Public
  --   License along with the GNU C Library; if not, see
  --   <http://www.gnu.org/licenses/>.   

  -- *	ISO C99: 7.18 Integer types <stdint.h>
  --  

  -- Exact integral types.   
  -- Signed.   
  -- There is some amount of overlap with <sys/types.h> as known by inet code  
   subtype int8_t is signed_char;  -- /usr/include/stdint.h:36

   subtype int16_t is short;  -- /usr/include/stdint.h:37

   subtype int32_t is int;  -- /usr/include/stdint.h:38

   subtype int64_t is long;  -- /usr/include/stdint.h:40

  -- Unsigned.   
   subtype uint8_t is unsigned_char;  -- /usr/include/stdint.h:48

   subtype uint16_t is unsigned_short;  -- /usr/include/stdint.h:49

   subtype uint32_t is unsigned;  -- /usr/include/stdint.h:51

   subtype uint64_t is unsigned_long;  -- /usr/include/stdint.h:55

  -- Small types.   
  -- Signed.   
   subtype int_least8_t is signed_char;  -- /usr/include/stdint.h:65

   subtype int_least16_t is short;  -- /usr/include/stdint.h:66

   subtype int_least32_t is int;  -- /usr/include/stdint.h:67

   subtype int_least64_t is long;  -- /usr/include/stdint.h:69

  -- Unsigned.   
   subtype uint_least8_t is unsigned_char;  -- /usr/include/stdint.h:76

   subtype uint_least16_t is unsigned_short;  -- /usr/include/stdint.h:77

   subtype uint_least32_t is unsigned;  -- /usr/include/stdint.h:78

   subtype uint_least64_t is unsigned_long;  -- /usr/include/stdint.h:80

  -- Fast types.   
  -- Signed.   
   subtype int_fast8_t is signed_char;  -- /usr/include/stdint.h:90

   subtype int_fast16_t is long;  -- /usr/include/stdint.h:92

   subtype int_fast32_t is long;  -- /usr/include/stdint.h:93

   subtype int_fast64_t is long;  -- /usr/include/stdint.h:94

  -- Unsigned.   
   subtype uint_fast8_t is unsigned_char;  -- /usr/include/stdint.h:103

   subtype uint_fast16_t is unsigned_long;  -- /usr/include/stdint.h:105

   subtype uint_fast32_t is unsigned_long;  -- /usr/include/stdint.h:106

   subtype uint_fast64_t is unsigned_long;  -- /usr/include/stdint.h:107

  -- Types for `void *' pointers.   
   subtype intptr_t is long;  -- /usr/include/stdint.h:119

   subtype uintptr_t is unsigned_long;  -- /usr/include/stdint.h:122

  -- Largest integral types.   
   subtype intmax_t is long;  -- /usr/include/stdint.h:134

   subtype uintmax_t is unsigned_long;  -- /usr/include/stdint.h:135

  -- Limits of integral types.   
  -- Minimum of signed integral types.   
  -- Maximum of signed integral types.   
  -- Maximum of unsigned integral types.   
  -- Minimum of signed integral types having a minimum size.   
  -- Maximum of signed integral types having a minimum size.   
  -- Maximum of unsigned integral types having a minimum size.   
  -- Minimum of fast signed integral types having a minimum size.   
  -- Maximum of fast signed integral types having a minimum size.   
  -- Maximum of fast unsigned integral types having a minimum size.   
  -- Values to test for integral types holding `void *' pointer.   
  -- Minimum for largest signed integral type.   
  -- Maximum for largest signed integral type.   
  -- Maximum for largest unsigned integral type.   
  -- Limits of other integer types.   
  -- Limits of `ptrdiff_t' type.   
  -- Limits of `sig_atomic_t'.   
  -- Limit of `size_t' type.   
  -- Limits of `wchar_t'.   
  -- These constants might also be defined in <wchar.h>.   
  -- Limits of `wint_t'.   
  -- Signed.   
  -- Unsigned.   
  -- Maximal type.   
end stdint_h;
