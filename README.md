GNAT LLVM
=========

This is an experimental Ada compiler based on LLVM, bridging the GNAT
front-end with the LLVM code generator.

This is a work in progress research project and should not be used for
industrial usage. It is meant to show the feasibility of generating LLVM
byte code for Ada, and open the LLVM ecosystem to Ada, including tools such as
KLEE.

GNAT LLVM is licensed under the GNU General Public License version 3 or later,
see file COPYING3 for details.

Earlier users are welcome to experiment with this technology and provide
feedback on success, usage, limitations, pull requests, etc...

For more information on LLVM, see http://llvm.org
For more information on GNAT, see http://www.adacore.com

In order to build GNAT LLVM from sources, you need to follow these steps:

- Get a check out of the GNAT sources from gcc.gnu.org under the
  llvm-interface directory:

  $ svn co svn://gcc.gnu.org/svn/gcc/trunk/gcc/ada llvm-interface/gnat_src

- Install (and put in your PATH) a recent GNAT, e.g GNAT Community 2019

- Build/install LLVM and Clang 8.0.x
  You can use an existing LLVM and clang 8.0.x package install via e.g.
  "brew install llvm" on Mac OS or "sudo apt-get install llvm-dev" on Ubuntu
  and make sure the llvm bin directory containing llvm-config and
  clang is in your PATH.

  Alternatively you will need to build LLVM and Clang 8.0.x from sources.
  One possible way assuming you have cmake version >= 3.7.2 in your
  environment is to do:

  $ make llvm

- Finally build GNAT LLVM:

  $ make

This will create a "ready to use" set of directories "bin" and "lib" under
llvm-interface which you can put in your PATH:

  PATH=$PWD/llvm-interface/bin:$PATH

- To run the compiler and produce a native object file:

  $ llvm-gcc -c file.adb

- To debug the compiler:

  $ gdb -args llvm-gnat1 -c file.adb

- To build a complete native executable:

  $ llvm-gnatmake main.adb

- To build a whole project:

  $ gprbuild --target=llvm -Pprj ...

- To generate LLVM byte code (will generate a .bc file):

  $ llvm-gcc -c -emit-llvm file.adb

- To generate LLVM assembly (will generate a .ll file):

  $ llvm-gcc -c -S -emit-llvm file.adb

- To generate native assembly file (will generate a .s file):

  $ llvm-gcc -S file.adb

