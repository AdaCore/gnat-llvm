pragma Ada_2005;
pragma Style_Checks (Off);

with x86_64_linux_gnu_bits_types_h;

package x86_64_linux_gnu_bits_stdint_intn_h is

  -- Define intN_t types.
  --   Copyright (C) 2017-2018 Free Software Foundation, Inc.
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

   subtype int8_t is x86_64_linux_gnu_bits_types_h.uu_int8_t;  -- /usr/include/x86_64-linux-gnu/bits/stdint-intn.h:24

   subtype int16_t is x86_64_linux_gnu_bits_types_h.uu_int16_t;  -- /usr/include/x86_64-linux-gnu/bits/stdint-intn.h:25

   subtype int32_t is x86_64_linux_gnu_bits_types_h.uu_int32_t;  -- /usr/include/x86_64-linux-gnu/bits/stdint-intn.h:26

   subtype int64_t is x86_64_linux_gnu_bits_types_h.uu_int64_t;  -- /usr/include/x86_64-linux-gnu/bits/stdint-intn.h:27

end x86_64_linux_gnu_bits_stdint_intn_h;
