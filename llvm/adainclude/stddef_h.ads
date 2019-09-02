pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package stddef_h is

   subtype size_t is unsigned_long;
   subtype off_t is unsigned_long;

end stddef_h;
