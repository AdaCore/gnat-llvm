/*****************************************************************************
 *                                                                           *
 *                          GNAT COMPILER COMPONENTS                         *
 *                                                                           *
 *                              j m i s s i n g                              *
 *                                                                           *
 *                      Copyright (C) 1998-2013, AdaCore                     *
 *                                                                           *
 *  GNAT is free software;  you can  redistribute it  and/or modify it under *
 *  terms of the  GNU General Public License as published  by the Free Soft- *
 *  ware  Foundation;  either version 2,  or (at your option) any later ver- *
 *  sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 *  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 *  for  more details.  You should have  received  a copy of the GNU General *
 *  Public License  distributed with GNAT;  see file COPYING.  If not, write *
 *  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 *  MA 02111-1307, USA.                                                      *
 *                                                                           *
 * The GNAT Ada tool chain for the JVM and .NET platforms is  maintained by  *
 * AdaCore - http://www.adacore.com                                          *
 *                                                                           *
 *****************************************************************************/

/* This file contains the C routines or variables which are either
   defined in toplev.c or some other GCC source (and hence not
   available when compiling for JVM/.NET). */

/* Originally defined in GCC's toplev.c. GNAT uses this flag to
   determine whether stack checking is enabled on the target (controls
   allocation strategy for large objects in certain cases). */
int flag_stack_check = 0;

/*  Originally defined in GCC's toplev.c. */
int optimize = 0;
int optimize_size = 0;
int flag_preserve_control_flow = 0;

/* Originally defined in toplev.c, used in exp_cg.adb. */
void *callgraph_info_file = (void *)0;

/* Originally defined in misc.c.  */
unsigned int save_argc = 0;
const char **save_argv = (const char **)0;

/* Originally defined in GCC's prefix.c. We need a dummy
   update_path and set_std_prefix for osint.adb. */
void
set_std_prefix (char *path, int len)
{
}

char *
update_path (char *path, char *key)
{
  return path;
}

/* Originally defined in link.c */
unsigned char __gnat_separate_run_path_options = 0;
const char *__gnat_default_libgcc_subdir = "lib";

