pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;

package x86_64_linux_gnu_sys_types_h is

  -- Copyright (C) 1991-2018 Free Software Foundation, Inc.
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

  -- *	POSIX Standard: 2.6 Primitive System Data Types	<sys/types.h>
  --  

   subtype u_char is x86_64_linux_gnu_bits_types_h.uu_u_char;  -- /usr/include/x86_64-linux-gnu/sys/types.h:33

   subtype u_short is x86_64_linux_gnu_bits_types_h.uu_u_short;  -- /usr/include/x86_64-linux-gnu/sys/types.h:34

   subtype u_int is x86_64_linux_gnu_bits_types_h.uu_u_int;  -- /usr/include/x86_64-linux-gnu/sys/types.h:35

   subtype u_long is x86_64_linux_gnu_bits_types_h.uu_u_long;  -- /usr/include/x86_64-linux-gnu/sys/types.h:36

   subtype quad_t is x86_64_linux_gnu_bits_types_h.uu_quad_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:37

   subtype u_quad_t is x86_64_linux_gnu_bits_types_h.uu_u_quad_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:38

   subtype fsid_t is x86_64_linux_gnu_bits_types_h.uu_fsid_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:39

   subtype loff_t is x86_64_linux_gnu_bits_types_h.uu_loff_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:44

   subtype ino_t is x86_64_linux_gnu_bits_types_h.uu_ino_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:48

   subtype ino64_t is x86_64_linux_gnu_bits_types_h.uu_ino64_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:55

   subtype dev_t is x86_64_linux_gnu_bits_types_h.uu_dev_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:60

   subtype gid_t is x86_64_linux_gnu_bits_types_h.uu_gid_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:65

   subtype mode_t is x86_64_linux_gnu_bits_types_h.uu_mode_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:70

   subtype nlink_t is x86_64_linux_gnu_bits_types_h.uu_nlink_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:75

   subtype uid_t is x86_64_linux_gnu_bits_types_h.uu_uid_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:80

   subtype off_t is x86_64_linux_gnu_bits_types_h.uu_off_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:86

   subtype off64_t is x86_64_linux_gnu_bits_types_h.uu_off64_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:93

   subtype pid_t is x86_64_linux_gnu_bits_types_h.uu_pid_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:98

   subtype id_t is x86_64_linux_gnu_bits_types_h.uu_id_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:104

   subtype ssize_t is x86_64_linux_gnu_bits_types_h.uu_ssize_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:109

   subtype daddr_t is x86_64_linux_gnu_bits_types_h.uu_daddr_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:115

   subtype caddr_t is x86_64_linux_gnu_bits_types_h.uu_caddr_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:116

   subtype key_t is x86_64_linux_gnu_bits_types_h.uu_key_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:122

   subtype useconds_t is x86_64_linux_gnu_bits_types_h.uu_useconds_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:135

   subtype suseconds_t is x86_64_linux_gnu_bits_types_h.uu_suseconds_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:139

  -- Old compatibility names for C types.   
   subtype ulong is unsigned_long;  -- /usr/include/x86_64-linux-gnu/sys/types.h:149

   subtype ushort is unsigned_short;  -- /usr/include/x86_64-linux-gnu/sys/types.h:150

   subtype uint is unsigned;  -- /usr/include/x86_64-linux-gnu/sys/types.h:151

  -- These size-specific names are used by some of the inet code.   
  -- These were defined by ISO C without the first `_'.   
  -- For GCC 2.7 and later, we can use specific type-size attributes.   
   subtype u_int8_t is unsigned_char;  -- /usr/include/x86_64-linux-gnu/sys/types.h:178

   subtype u_int16_t is unsigned_short;  -- /usr/include/x86_64-linux-gnu/sys/types.h:179

   subtype u_int32_t is unsigned;  -- /usr/include/x86_64-linux-gnu/sys/types.h:180

   subtype u_int64_t is unsigned_long;  -- /usr/include/x86_64-linux-gnu/sys/types.h:181

   subtype register_t is long;  -- /usr/include/x86_64-linux-gnu/sys/types.h:183

  -- Some code from BIND tests this macro to see if the types above are
  --   defined.   

  -- In BSD <sys/types.h> is expected to define BYTE_ORDER.   
  -- It also defines `fd_set' and the FD_* macros for `select'.   
  -- BSD defines `major', `minor', and `makedev' in this header.
  --   However, these symbols are likely to collide with user code, so we are
  --   going to stop defining them here in an upcoming release.  Code that needs
  --   these macros should include <sys/sysmacros.h> directly.  Code that does
  --   not need these macros should #undef them after including this header.   

   subtype blksize_t is x86_64_linux_gnu_bits_types_h.uu_blksize_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:212

  -- Types from the Large File Support interface.   
  -- Type to count number of disk blocks.   
   subtype blkcnt_t is x86_64_linux_gnu_bits_types_h.uu_blkcnt_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:219

  -- Type to count file system blocks.   
   subtype fsblkcnt_t is x86_64_linux_gnu_bits_types_h.uu_fsblkcnt_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:223

  -- Type to count file system inodes.   
   subtype fsfilcnt_t is x86_64_linux_gnu_bits_types_h.uu_fsfilcnt_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:227

  -- Type to count number of disk blocks.   
  -- Type to count file system blocks.   
  -- Type to count file system inodes.   
  -- Type to count number of disk blocks.  
   subtype blkcnt64_t is x86_64_linux_gnu_bits_types_h.uu_blkcnt64_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:246

  -- Type to count file system blocks.   
   subtype fsblkcnt64_t is x86_64_linux_gnu_bits_types_h.uu_fsblkcnt64_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:247

  -- Type to count file system inodes.   
   subtype fsfilcnt64_t is x86_64_linux_gnu_bits_types_h.uu_fsfilcnt64_t;  -- /usr/include/x86_64-linux-gnu/sys/types.h:248

  -- Now add the thread types.   
end x86_64_linux_gnu_sys_types_h;
