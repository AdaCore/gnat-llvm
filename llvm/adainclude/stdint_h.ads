pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package stdint_h is

   INT8_MIN : constant := (-128);  --  /usr/include/stdint.h:160
   INT16_MIN : constant := (-32767-1);  --  /usr/include/stdint.h:161
   INT32_MIN : constant := (-2147483647-1);  --  /usr/include/stdint.h:162
   --  unsupported macro: INT64_MIN (-__INT64_C(9223372036854775807)-1)

   INT8_MAX : constant := (127);  --  /usr/include/stdint.h:165
   INT16_MAX : constant := (32767);  --  /usr/include/stdint.h:166
   INT32_MAX : constant := (2147483647);  --  /usr/include/stdint.h:167
   --  unsupported macro: INT64_MAX (__INT64_C(9223372036854775807))

   UINT8_MAX : constant := (255);  --  /usr/include/stdint.h:171
   UINT16_MAX : constant := (65535);  --  /usr/include/stdint.h:172
   UINT32_MAX : constant := (4294967295);  --  /usr/include/stdint.h:173
   --  unsupported macro: UINT64_MAX (__UINT64_C(18446744073709551615))

   INT_LEAST8_MIN : constant := (-128);  --  /usr/include/stdint.h:178
   INT_LEAST16_MIN : constant := (-32767-1);  --  /usr/include/stdint.h:179
   INT_LEAST32_MIN : constant := (-2147483647-1);  --  /usr/include/stdint.h:180
   --  unsupported macro: INT_LEAST64_MIN (-__INT64_C(9223372036854775807)-1)

   INT_LEAST8_MAX : constant := (127);  --  /usr/include/stdint.h:183
   INT_LEAST16_MAX : constant := (32767);  --  /usr/include/stdint.h:184
   INT_LEAST32_MAX : constant := (2147483647);  --  /usr/include/stdint.h:185
   --  unsupported macro: INT_LEAST64_MAX (__INT64_C(9223372036854775807))

   UINT_LEAST8_MAX : constant := (255);  --  /usr/include/stdint.h:189
   UINT_LEAST16_MAX : constant := (65535);  --  /usr/include/stdint.h:190
   UINT_LEAST32_MAX : constant := (4294967295);  --  /usr/include/stdint.h:191
   --  unsupported macro: UINT_LEAST64_MAX (__UINT64_C(18446744073709551615))

   INT_FAST8_MIN : constant := (-128);  --  /usr/include/stdint.h:196

   INT_FAST16_MIN : constant := (-9223372036854775807-1);  --  /usr/include/stdint.h:198
   INT_FAST32_MIN : constant := (-9223372036854775807-1);  --  /usr/include/stdint.h:199
   --  unsupported macro: INT_FAST64_MIN (-__INT64_C(9223372036854775807)-1)

   INT_FAST8_MAX : constant := (127);  --  /usr/include/stdint.h:206

   INT_FAST16_MAX : constant := (9223372036854775807);  --  /usr/include/stdint.h:208
   INT_FAST32_MAX : constant := (9223372036854775807);  --  /usr/include/stdint.h:209
   --  unsupported macro: INT_FAST64_MAX (__INT64_C(9223372036854775807))

   UINT_FAST8_MAX : constant := (255);  --  /usr/include/stdint.h:217

   UINT_FAST16_MAX : constant := (18446744073709551615);  --  /usr/include/stdint.h:219
   UINT_FAST32_MAX : constant := (18446744073709551615);  --  /usr/include/stdint.h:220
   --  unsupported macro: UINT_FAST64_MAX (__UINT64_C(18446744073709551615))

   INTPTR_MIN : constant := (-9223372036854775807-1);  --  /usr/include/stdint.h:230
   INTPTR_MAX : constant := (9223372036854775807);  --  /usr/include/stdint.h:231
   UINTPTR_MAX : constant := (18446744073709551615);  --  /usr/include/stdint.h:232
   --  unsupported macro: INTMAX_MIN (-__INT64_C(9223372036854775807)-1)
   --  unsupported macro: INTMAX_MAX (__INT64_C(9223372036854775807))
   --  unsupported macro: UINTMAX_MAX (__UINT64_C(18446744073709551615))

   PTRDIFF_MIN : constant := (-9223372036854775807-1);  --  /usr/include/stdint.h:253
   PTRDIFF_MAX : constant := (9223372036854775807);  --  /usr/include/stdint.h:254

   SIG_ATOMIC_MIN : constant := (-2147483647-1);  --  /usr/include/stdint.h:261
   SIG_ATOMIC_MAX : constant := (2147483647);  --  /usr/include/stdint.h:262

   SIZE_MAX : constant := (18446744073709551615);  --  /usr/include/stdint.h:266
   --  unsupported macro: WCHAR_MIN __WCHAR_MIN
   --  unsupported macro: WCHAR_MAX __WCHAR_MAX

   WINT_MIN : constant := (0);  --  /usr/include/stdint.h:279
   WINT_MAX : constant := (4294967295);  --  /usr/include/stdint.h:280
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

   subtype int8_t is signed_char;  -- /usr/include/stdint.h:37

   subtype int16_t is short;  -- /usr/include/stdint.h:38

   subtype int32_t is int;  -- /usr/include/stdint.h:39

   subtype int64_t is long;  -- /usr/include/stdint.h:41

   subtype uint8_t is unsigned_char;  -- /usr/include/stdint.h:49

   subtype uint16_t is unsigned_short;  -- /usr/include/stdint.h:50

   subtype uint32_t is unsigned;  -- /usr/include/stdint.h:52

   subtype uint64_t is unsigned_long;  -- /usr/include/stdint.h:56

   subtype int_least8_t is signed_char;  -- /usr/include/stdint.h:66

   subtype int_least16_t is short;  -- /usr/include/stdint.h:67

   subtype int_least32_t is int;  -- /usr/include/stdint.h:68

   subtype int_least64_t is long;  -- /usr/include/stdint.h:70

   subtype uint_least8_t is unsigned_char;  -- /usr/include/stdint.h:77

   subtype uint_least16_t is unsigned_short;  -- /usr/include/stdint.h:78

   subtype uint_least32_t is unsigned;  -- /usr/include/stdint.h:79

   subtype uint_least64_t is unsigned_long;  -- /usr/include/stdint.h:81

   subtype int_fast8_t is signed_char;  -- /usr/include/stdint.h:91

   subtype int_fast16_t is long;  -- /usr/include/stdint.h:93

   subtype int_fast32_t is long;  -- /usr/include/stdint.h:94

   subtype int_fast64_t is long;  -- /usr/include/stdint.h:95

   subtype uint_fast8_t is unsigned_char;  -- /usr/include/stdint.h:104

   subtype uint_fast16_t is unsigned_long;  -- /usr/include/stdint.h:106

   subtype uint_fast32_t is unsigned_long;  -- /usr/include/stdint.h:107

   subtype uint_fast64_t is unsigned_long;  -- /usr/include/stdint.h:108

   subtype intptr_t is long;  -- /usr/include/stdint.h:120

   subtype uintptr_t is unsigned_long;  -- /usr/include/stdint.h:123

   subtype intmax_t is long;  -- /usr/include/stdint.h:135

   subtype uintmax_t is unsigned_long;  -- /usr/include/stdint.h:136

end stdint_h;
