pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package stddef_h is

   subtype size_t is Interfaces.C.size_t;
   subtype off_t is long;

end stddef_h;
