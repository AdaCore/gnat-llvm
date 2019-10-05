pragma Style_Checks (Off);

package stdint_h is

   type int8_t is range -2 ** 7 .. 2 ** 7 - 1;  -- /usr/include/stdint.h:36

   type int16_t is range -2 ** 15 .. 2 ** 15 - 1; -- /usr/include/stdint.h:37

   type int32_t is range -2 ** 31 .. 2 ** 31 - 1; -- /usr/include/stdint.h:38

   type int64_t is range -2 ** 63 .. 2 ** 63 - 1;  -- /usr/include/stdint.h:40

   type uint8_t is mod 2 ** 8;  -- /usr/include/stdint.h:48

   type uint16_t is mod 2 ** 16;  -- /usr/include/stdint.h:49

   type uint32_t is mod 2 ** 32;  -- /usr/include/stdint.h:51

   type uint64_t is mod 2 ** 64;  -- /usr/include/stdint.h:55

   type intptr_t is range
     -2 ** (Standard'Address_Size - 1) .. 2 ** (Standard'Address_Size - 1) - 1;
   -- /usr/include/stdint.h:119

   type uintptr_t is mod 2 ** Standard'Address_Size;
   -- /usr/include/stdint.h:122

end stdint_h;
