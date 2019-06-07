pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package stdint_h is

   subtype int8_t is signed_char;  -- /usr/include/stdint.h:36

   subtype int16_t is short;  -- /usr/include/stdint.h:37

   subtype int32_t is int;  -- /usr/include/stdint.h:38

   subtype int64_t is long;  -- /usr/include/stdint.h:40

   subtype uint8_t is unsigned_char;  -- /usr/include/stdint.h:48

   subtype uint16_t is unsigned_short;  -- /usr/include/stdint.h:49

   subtype uint32_t is unsigned;  -- /usr/include/stdint.h:51

   subtype uint64_t is unsigned_long;  -- /usr/include/stdint.h:55

   subtype intptr_t is long;  -- /usr/include/stdint.h:119

   subtype uintptr_t is unsigned_long;  -- /usr/include/stdint.h:122

end stdint_h;
