# Note that this Makefile is optional and mainly used by core GNAT LLVM
# developers: you can use an existing install of LLVM instead.
# llvm-interface/Makefile will work as long as llvm-config is found in your
# PATH.

PWD:=$(shell pwd)
LLVM_RELEASE=16.0.6
LLVM_INSTALL_DIR=$(PWD)/install
LLVM_SRC_DIR=llvm-project-$(LLVM_RELEASE).src
DL=wget
LLVM_SRC_FNAME=$(LLVM_SRC_DIR).tar.xz
LLVM_RELEASE_URL=https://github.com/llvm/llvm-project/releases/download/llvmorg-$(LLVM_RELEASE)
LLVM_SRC_LINK=$(LLVM_RELEASE_URL)/$(LLVM_SRC_FNAME)
LLVM_TARGETS=X86
# Linking LLVM libraries and binaries requires lots of RAM and is a common cause
# for out-of-memory errors. Restricting the build system to a single link job at
# a time doesn't affect the build time significantly but protects us from those
# memory issues.
CMAKE_FLAGS=-DLLVM_TARGETS_TO_BUILD="$(LLVM_TARGETS)" \
	-DBUILD_SHARED_LIBS=ON -DLLVM_INCLUDE_BENCHMARKS=OFF -DLLVM_ENABLE_PROJECTS=clang \
	-DLLVM_PARALLEL_LINK_JOBS=1 \
	-DLLVM_ENABLE_RUNTIMES="compiler-rt;libunwind" \
	-DCLANG_DEFAULT_RTLIB=compiler-rt -DCLANG_DEFAULT_UNWINDLIB=libunwind \
	-DLIBUNWIND_WEAK_PTHREAD_LIB=ON -DLIBUNWIND_USE_COMPILER_RT=ON
# LLVM 15 requires us to specify a CMake build type explicitly; we use Debug for
# the default target.
CMAKE_FLAGS_DEBUG=-DCMAKE_BUILD_TYPE=Debug
# No-debug mode is still good for GNAT-LLVM development: we enable assertions
# and debug info. But it's much faster than debug mode because the LLVM code is
# optimized during compilation.
CMAKE_FLAGS_NO_DEBUG=-DCMAKE_BUILD_TYPE=RelWithDebInfo -DLLVM_ENABLE_ASSERTIONS=ON
CMAKE_FLAGS_PROD=-DCMAKE_BUILD_TYPE=RelWithDebInfo

.PHONY: configure

all: setup llvm

CONFIGURE=cd llvm-obj && CXX=g++ CC=gcc CXXC=g++ \
  cmake -G "Unix Makefiles" -DCMAKE_INSTALL_PREFIX=$(LLVM_INSTALL_DIR) \
  $(CMAKE_FLAGS) ../$(LLVM_SRC_DIR)/llvm

setup: $(LLVM_SRC_DIR)

$(LLVM_SRC_DIR):
	$(DL) $(LLVM_SRC_LINK)
	tar xJf $(LLVM_SRC_FNAME)
	rm $(LLVM_SRC_FNAME)

llvm-obj/CMakeCache.txt:
	mkdir -p llvm-obj
	$(CONFIGURE) $(CMAKE_FLAGS_DEBUG)

configure-no-debug:
	mkdir -p llvm-obj
	$(CONFIGURE) $(CMAKE_FLAGS_NO_DEBUG)

configure-prod:
	mkdir -p llvm-obj
	$(CONFIGURE) $(CMAKE_FLAGS_PROD)

llvm: llvm-obj/CMakeCache.txt
	$(MAKE) -s -C llvm-obj

install: llvm
	cd llvm-obj && cmake -P cmake_install.cmake

clean:
	rm -rf llvm-obj $(LLVM_INSTALL_DIR)

distclean: clean
	rm -rf $(LLVM_SRC_DIR)
