/****************************************************************************
 *                                                                          *
 *                          GNAT COMPILER COMPONENTS                        *
 *                                                                          *
 *                              j m i s s i n g                             *
 *                                                                          *
 *                      Copyright (C) 1998-2026, AdaCore                    *
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

/* This file contains the C routines or variables which are defined in
   some GCC source (and hence not available when compiling here). */

#include <stdio.h>

/* Originally defined in GCC's toplev.c. GNAT uses this flag to
   determine whether stack checking is enabled on the target (controls
   allocation strategy for large objects in certain cases). */
int flag_stack_check = 0;

/* Originally defined in GCC's common.opt. Controls the balance between GNAT
   encodings and standard DWARF to emit in the debug infomation.  Useful for
   DWARF debugging information generation only so not used in CodePeer.  */
int gnat_encodings = 0;

/*  Originally defined in GCC's toplev.c. */
int optimize = 0;
int optimize_size = 0;

/* Originally defined in toplev.c, used in exp_cg.adb. GCC's toplev opens
   this FILE* before invoking gnat1, and Exp_CG.Generate_CG_Output writes the
   dispatching-call edges and the tagged-type "class{}" declarations to it.
   gnat-llvm has no toplev, so it opens the stream itself, calls
   Generate_CG_Output from the back end and reads the text back, to put it
   into an object section (see GNATLLVM.Codegen.Generate_Code). The two
   routines that do so are gnatllvm_open_callgraph_info_file and
   gnatllvm_take_callgraph_info_text in llvm_wrapper.cc.
   The definition stays here because this is where GCC's
   missing definitions belong, and because any tool whose closure reaches
   exp_cg would need it. */
FILE *callgraph_info_file = NULL;

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

/* Originally defined in version.c */
const char gnat_version_string[] = "1.0";

