pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;

package stdint_h is

   INT8_MIN : constant := (-128);  --  /usr/include/stdint.h:126
   INT16_MIN : constant := (-32767-1);  --  /usr/include/stdint.h:127
   INT32_MIN : constant := (-2147483647-1);  --  /usr/include/stdint.h:128
   --  unsupported macro: INT64_MIN (-__INT64_C(9223372036854775807)-1)

   INT8_MAX : constant := (127);  --  /usr/include/stdint.h:131
   INT16_MAX : constant := (32767);  --  /usr/include/stdint.h:132
   INT32_MAX : constant := (2147483647);  --  /usr/include/stdint.h:133
   --  unsupported macro: INT64_MAX (__INT64_C(9223372036854775807))

   UINT8_MAX : constant := (255);  --  /usr/include/stdint.h:137
   UINT16_MAX : constant := (65535);  --  /usr/include/stdint.h:138
   UINT32_MAX : constant := (4294967295);  --  /usr/include/stdint.h:139
   --  unsupported macro: UINT64_MAX (__UINT64_C(18446744073709551615))

   INT_LEAST8_MIN : constant := (-128);  --  /usr/include/stdint.h:144
   INT_LEAST16_MIN : constant := (-32767-1);  --  /usr/include/stdint.h:145
   INT_LEAST32_MIN : constant := (-2147483647-1);  --  /usr/include/stdint.h:146
   --  unsupported macro: INT_LEAST64_MIN (-__INT64_C(9223372036854775807)-1)

   INT_LEAST8_MAX : constant := (127);  --  /usr/include/stdint.h:149
   INT_LEAST16_MAX : constant := (32767);  --  /usr/include/stdint.h:150
   INT_LEAST32_MAX : constant := (2147483647);  --  /usr/include/stdint.h:151
   --  unsupported macro: INT_LEAST64_MAX (__INT64_C(9223372036854775807))

   UINT_LEAST8_MAX : constant := (255);  --  /usr/include/stdint.h:155
   UINT_LEAST16_MAX : constant := (65535);  --  /usr/include/stdint.h:156
   UINT_LEAST32_MAX : constant := (4294967295);  --  /usr/include/stdint.h:157
   --  unsupported macro: UINT_LEAST64_MAX (__UINT64_C(18446744073709551615))

   INT_FAST8_MIN : constant := (-128);  --  /usr/include/stdint.h:162

   INT_FAST16_MIN : constant := (-9223372036854775807-1);  --  /usr/include/stdint.h:164
   INT_FAST32_MIN : constant := (-9223372036854775807-1);  --  /usr/include/stdint.h:165
   --  unsupported macro: INT_FAST64_MIN (-__INT64_C(9223372036854775807)-1)

   INT_FAST8_MAX : constant := (127);  --  /usr/include/stdint.h:172

   INT_FAST16_MAX : constant := (9223372036854775807);  --  /usr/include/stdint.h:174
   INT_FAST32_MAX : constant := (9223372036854775807);  --  /usr/include/stdint.h:175
   --  unsupported macro: INT_FAST64_MAX (__INT64_C(9223372036854775807))

   UINT_FAST8_MAX : constant := (255);  --  /usr/include/stdint.h:183

   UINT_FAST16_MAX : constant := (18446744073709551615);  --  /usr/include/stdint.h:185
   UINT_FAST32_MAX : constant := (18446744073709551615);  --  /usr/include/stdint.h:186
   --  unsupported macro: UINT_FAST64_MAX (__UINT64_C(18446744073709551615))

   INTPTR_MIN : constant := (-9223372036854775807-1);  --  /usr/include/stdint.h:196
   INTPTR_MAX : constant := (9223372036854775807);  --  /usr/include/stdint.h:197
   UINTPTR_MAX : constant := (18446744073709551615);  --  /usr/include/stdint.h:198
   --  unsupported macro: INTMAX_MIN (-__INT64_C(9223372036854775807)-1)
   --  unsupported macro: INTMAX_MAX (__INT64_C(9223372036854775807))
   --  unsupported macro: UINTMAX_MAX (__UINT64_C(18446744073709551615))

   PTRDIFF_MIN : constant := (-9223372036854775807-1);  --  /usr/include/stdint.h:219
   PTRDIFF_MAX : constant := (9223372036854775807);  --  /usr/include/stdint.h:220

   SIG_ATOMIC_MIN : constant := (-2147483647-1);  --  /usr/include/stdint.h:232
   SIG_ATOMIC_MAX : constant := (2147483647);  --  /usr/include/stdint.h:233

   SIZE_MAX : constant := (18446744073709551615);  --  /usr/include/stdint.h:237
   --  unsupported macro: WCHAR_MIN __WCHAR_MIN
   --  unsupported macro: WCHAR_MAX __WCHAR_MAX

   WINT_MIN : constant := (0);  --  /usr/include/stdint.h:254
   WINT_MAX : constant := (4294967295);  --  /usr/include/stdint.h:255
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

   INT8_WIDTH : constant := 8;  --  /usr/include/stdint.h:288
   UINT8_WIDTH : constant := 8;  --  /usr/include/stdint.h:289
   INT16_WIDTH : constant := 16;  --  /usr/include/stdint.h:290
   UINT16_WIDTH : constant := 16;  --  /usr/include/stdint.h:291
   INT32_WIDTH : constant := 32;  --  /usr/include/stdint.h:292
   UINT32_WIDTH : constant := 32;  --  /usr/include/stdint.h:293
   INT64_WIDTH : constant := 64;  --  /usr/include/stdint.h:294
   UINT64_WIDTH : constant := 64;  --  /usr/include/stdint.h:295

   INT_LEAST8_WIDTH : constant := 8;  --  /usr/include/stdint.h:297
   UINT_LEAST8_WIDTH : constant := 8;  --  /usr/include/stdint.h:298
   INT_LEAST16_WIDTH : constant := 16;  --  /usr/include/stdint.h:299
   UINT_LEAST16_WIDTH : constant := 16;  --  /usr/include/stdint.h:300
   INT_LEAST32_WIDTH : constant := 32;  --  /usr/include/stdint.h:301
   UINT_LEAST32_WIDTH : constant := 32;  --  /usr/include/stdint.h:302
   INT_LEAST64_WIDTH : constant := 64;  --  /usr/include/stdint.h:303
   UINT_LEAST64_WIDTH : constant := 64;  --  /usr/include/stdint.h:304

   INT_FAST8_WIDTH : constant := 8;  --  /usr/include/stdint.h:306
   UINT_FAST8_WIDTH : constant := 8;  --  /usr/include/stdint.h:307
   --  unsupported macro: INT_FAST16_WIDTH __WORDSIZE
   --  unsupported macro: UINT_FAST16_WIDTH __WORDSIZE
   --  unsupported macro: INT_FAST32_WIDTH __WORDSIZE
   --  unsupported macro: UINT_FAST32_WIDTH __WORDSIZE

   INT_FAST64_WIDTH : constant := 64;  --  /usr/include/stdint.h:312
   UINT_FAST64_WIDTH : constant := 64;  --  /usr/include/stdint.h:313
   --  unsupported macro: INTPTR_WIDTH __WORDSIZE
   --  unsupported macro: UINTPTR_WIDTH __WORDSIZE

   INTMAX_WIDTH : constant := 64;  --  /usr/include/stdint.h:318
   UINTMAX_WIDTH : constant := 64;  --  /usr/include/stdint.h:319
   --  unsupported macro: PTRDIFF_WIDTH __WORDSIZE

   SIG_ATOMIC_WIDTH : constant := 32;  --  /usr/include/stdint.h:322
   --  unsupported macro: SIZE_WIDTH __WORDSIZE

   WCHAR_WIDTH : constant := 32;  --  /usr/include/stdint.h:324
   WINT_WIDTH : constant := 32;  --  /usr/include/stdint.h:325

  -- Copyright (C) 1997-2018 Free Software Foundation, Inc.
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
  -- Unsigned.   
  -- Small types.   
  -- Signed.   
   subtype int_least8_t is signed_char;  -- /usr/include/stdint.h:43

   subtype int_least16_t is short;  -- /usr/include/stdint.h:44

   subtype int_least32_t is int;  -- /usr/include/stdint.h:45

   subtype int_least64_t is long;  -- /usr/include/stdint.h:47

  -- Unsigned.   
   subtype uint_least8_t is unsigned_char;  -- /usr/include/stdint.h:54

   subtype uint_least16_t is unsigned_short;  -- /usr/include/stdint.h:55

   subtype uint_least32_t is unsigned;  -- /usr/include/stdint.h:56

   subtype uint_least64_t is unsigned_long;  -- /usr/include/stdint.h:58

  -- Fast types.   
  -- Signed.   
   subtype int_fast8_t is signed_char;  -- /usr/include/stdint.h:68

   subtype int_fast16_t is long;  -- /usr/include/stdint.h:70

   subtype int_fast32_t is long;  -- /usr/include/stdint.h:71

   subtype int_fast64_t is long;  -- /usr/include/stdint.h:72

  -- Unsigned.   
   subtype uint_fast8_t is unsigned_char;  -- /usr/include/stdint.h:81

   subtype uint_fast16_t is unsigned_long;  -- /usr/include/stdint.h:83

   subtype uint_fast32_t is unsigned_long;  -- /usr/include/stdint.h:84

   subtype uint_fast64_t is unsigned_long;  -- /usr/include/stdint.h:85

  -- Types for `void *' pointers.   
   subtype intptr_t is long;  -- /usr/include/stdint.h:97

   subtype uintptr_t is unsigned_long;  -- /usr/include/stdint.h:100

  -- Largest integral types.   
   subtype intmax_t is x86_64_linux_gnu_bits_types_h.uu_intmax_t;  -- /usr/include/stdint.h:111

   subtype uintmax_t is x86_64_linux_gnu_bits_types_h.uu_uintmax_t;  -- /usr/include/stdint.h:112

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
