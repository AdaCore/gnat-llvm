/****************************************************************************
 *                                                                          *
 *                        GNAAMP COMPILER COMPONENTS                        *
 *                                                                          *
 *                              J T A R G E X T                             *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *                     Copyright (C) 2006-2017, AdaCore                     *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 ****************************************************************************/

/* This is the LLVM substitute for "targext.c" that is needed for building
   certain GNAT tools (such as llvm-gnatmake). */

/*  This file contains target-specific parameters describing the file       */
/*  extension for object and executable files. It is used by the compiler,  */
/*  binder and tools.                                                       */

#define TARGET_OBJECT_SUFFIX ".bc"
#define TARGET_EXECUTABLE_SUFFIX ""
unsigned char __gnat_objlist_file_supported = 0; /* TODO??? */

const char *__gnat_target_object_extension = TARGET_OBJECT_SUFFIX;
const char *__gnat_target_executable_extension = TARGET_EXECUTABLE_SUFFIX;
const char *__gnat_target_debuggable_extension = TARGET_EXECUTABLE_SUFFIX;

#define SHARED 'H'
#define STATIC 'T'

const char *__gnat_run_path_option = "";
const char *__gnat_object_file_option = "@";
char __gnat_shared_libgnat_default = SHARED;
int __gnat_link_max = 12000;
unsigned char __gnat_using_gnu_linker = 0;
const char *__gnat_object_library_extension = "";
char __gnat_shared_libgcc_default = SHARED;
