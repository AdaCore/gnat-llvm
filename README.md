GNAT LLVM
=========

Experimental Ada compiler based on LLVM

For more information on LLVM, see http://llvm.org
For more information on GNAT, see http://www.adacore.com

In order to build GNAT-LLVM from sources, you need to follow these steps:

- Get a check out of the GNAT sources under the llvm-interface directory:

  $ svn co svn://gcc.gnu.org/svn/gcc/trunk/gcc/ada llvm-interface/gnat_src

- Install (and put in your PATH) a recent GNAT, e.g GNAT Community 2019

- Build/install LLVM and Clang 8.0.x
  One possible way assuming you have cmake version >= 3.7.2 in your environment
  is to do:

  $ make llvm

  If you've installed LLVM and clang by other means (e.g. "brew install llvm"
  on Mac OS or "sudo apt-get install llvm-dev" on Ubuntu), make sure the
  llvm bin directory containing llvm-config and clang is in your PATH.

- Finally build GNAT-LLVM:

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

  $ gprbuild --target=llvm -P prj ...

- To generate LLVM byte code (will generate a .bc file):

  $ llvm-gcc -c -emit-llvm file.adb

- To generate native assembly file (will generate a .s file):

  $ llvm-gcc -S file.adb

