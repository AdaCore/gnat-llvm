pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package stddef_h is

   type size_t is mod 2 ** Standard'Address_Size;
   subtype off_t is long;

end stddef_h;
