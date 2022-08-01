GNAT LLVM
=========

This is an Ada compiler based on LLVM, connecting the GNAT front-end to the
LLVM code generator to generate LLVM bitcode for Ada and to open the LLVM
ecosystem to Ada.

Note that we are not planning on replacing any existing GNAT port that's
based on GCC: this project is meant to provide additional, not replacement,
GNAT ports.

You are welcome to experiment with this technology and provide
feedback on successes, usages, limitations, pull requests, etc.

- For more information on LLVM, see [llvm.org](https://llvm.org).
- For more information on GNAT, see [adacore.com](https://www.adacore.com).

Building
--------

To build GNAT LLVM from sources, follow these steps:

- First do a checkout of this repository and go to this directory:

      $ git clone https://github.com/AdaCore/gnat-llvm.git
      $ cd gnat-llvm

- Then obtain a check out of the latest GNAT sources from gcc.gnu.org under
  the llvm-interface directory:

      $ git clone git://gcc.gnu.org/git/gcc.git llvm-interface/gcc

  then under non Windows systems:

      $ ln -s gcc/gcc/ada llvm-interface/gnat_src

  under Windows systems:

      $ mv llvm-interface/gcc/gcc/ada llvm-interface/gnat_src

- Install (and put in your PATH) a recent GNAT, e.g GNAT Community 2021
  or GCC 11.

- Install LLVM and Clang 14.0.x

  The recommended way to build GNAT LLVM is to use an existing LLVM and
  clang package installed via e.g.  "brew install llvm" on Mac OS or "sudo
  apt-get install llvm-dev" on Ubuntu. You can also build llvm yourself with
  the options that suit your needs. After installing/building, make sure the
  llvm bin directory containing llvm-config and clang is in your PATH.

  An alternative only suitable for core GNAT LLVM development on x86 native
  configurations is to use the following command, assuming you have cmake
  version >= 3.13.4 in your path:

      $ make llvm

  Note that there's currently a bug in LLVM's aliasing handling. We check
  for it and generate slightly pessimized code in that case, but a patch
  to be applied to LLVM's lib/Analyze directory is in the file
  llvm/patches/LLVMStructTBAAPatch.diff.

- Finally build GNAT LLVM:

      $ make

This creates a "ready to use" set of directories "bin" and "lib" under
llvm-interface which you can put in your PATH:

    PATH=$PWD/llvm-interface/bin:$PATH

- If you want in addition to generate bitcode for the GNAT runtime, you can do:

      $ make gnatlib-bc

  This will generate libgnat.bc and libgnarl.bc in the adalib directory, along
  with libgnat.a and libgnarl.a.

Usage
-----

- To run the compiler and produce a native object file:

      $ llvm-gcc -c file.adb

- To debug the compiler:

      $ gdb -args llvm-gnat1 -c file.adb

- To build a complete native executable:

      $ llvm-gnatmake main.adb

- To build a whole project:

      $ gprbuild --target=llvm -Pprj ...

- To generate LLVM bitcode (will generate a .bc file):

      $ llvm-gcc -c -emit-llvm file.adb

- To generate LLVM assembly (will generate a .ll file):

      $ llvm-gcc -c -S -emit-llvm file.adb

- To generate native assembly file (will generate a .s file):

      $ llvm-gcc -S file.adb

License
-------

The GNAT LLVM tool is licensed under the GNU General Public License version 3
or later; see file `COPYING3` for details.
