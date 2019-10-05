pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package stdint_h is

   subtype int8_t is signed_char;  -- /usr/include/stdint.h:36

   subtype int16_t is short;  -- /usr/include/stdint.h:37

   subtype int32_t is int;  -- /usr/include/stdint.h:38

   type int64_t is range -2 ** 63 .. 2 ** 63 - 1;  -- /usr/include/stdint.h:40

   subtype uint8_t is unsigned_char;  -- /usr/include/stdint.h:48

   subtype uint16_t is unsigned_short;  -- /usr/include/stdint.h:49

   subtype uint32_t is unsigned;  -- /usr/include/stdint.h:51

   type uint64_t is mod 2 ** 64;  -- /usr/include/stdint.h:55

   type intptr_t is range
     -2 ** (Standard'Address_Size - 1) .. 2 ** (Standard'Address_Size - 1) - 1;
   -- /usr/include/stdint.h:119

   type uintptr_t is mod 2 ** Standard'Address_Size;
   -- /usr/include/stdint.h:122

end stdint_h;
