# Note that this Makefile is optional and mainly used by core GNAT LLVM
# developers: you can use an existing install of LLVM instead.
# llvm-interface/Makefile will work as long as llvm-config is found in your
# PATH.

PWD:=$(shell pwd)
TMP_GEN_DIR=gen
OUT_DIR=out
LLVM_RELEASE=16.0.6
LLVM_INSTALL_DIR=$(PWD)/install
LLVM_SRC_DIR=llvm-project-$(LLVM_RELEASE).src
LLVM_INCLUDE_DIR=$(LLVM_INSTALL_DIR)/include
LLVM_INCLUDE_DIR_C=$(LLVM_INCLUDE_DIR)/llvm-c
CLANG_INCLUDE_DIR=$(LLVM_INSTALL_DIR)/include
CLANG_INCLUDE_DIR_C=$(CLANG_INCLUDE_DIR)/clang-c
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

.PHONY: configure clang-bindings

all: setup llvm

# Bindings generation works with GNAT versions starting from 22.0
# (in terms of date > 2021-06-16)

llvm-bindings: $(LLVM_SRC_DIR) $(OUT_DIR)/llvm-core.adb
	rm -rf llvm-bindings
	mv $(OUT_DIR) llvm-bindings

clang-bindings: $(LLVM_SRC_DIR) $(OUT_DIR)/clang-index.adb
	mv $(OUT_DIR)/* clang-bindings

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

# Note: in order to use py/common.py you need to first install libadalang
# and then run:
# pip install <prefix>/share/libadalang/python/Libadalang-*.whl

$(OUT_DIR)/llvm-core.adb: install py/common.py py/wrapper.py
	rm -rf $(TMP_GEN_DIR) $(OUT_DIR)
	mkdir -p $(TMP_GEN_DIR) $(OUT_DIR)
	cd $(TMP_GEN_DIR) && g++ -I$(LLVM_INCLUDE_DIR) -I$(PWD)/llvm-obj/include -DNDEBUG -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -U__cplusplus -c -D_Bool=bool -Dlto_api_version=lto_api_version_fun -fdump-ada-spec -C $(LLVM_INCLUDE_DIR_C)/*.h $(LLVM_INCLUDE_DIR_C)/*/*.h
	sed -e "s^$(PWD)/^^g" -i $(TMP_GEN_DIR)/llvm_c_*_h.ads
	sed -e 's/\(with Interfaces.C; use Interfaces.C;\)/pragma Warnings (Off); \1 pragma Warnings (On);/g' -i $(TMP_GEN_DIR)/llvm_c_*_h.ads
	echo -n "pragma Style_Checks (Off); package LLVM is end LLVM;" > $(OUT_DIR)/llvm.ads
	./py/common.py process_names_llvm gen/llvm_c_*_h.ads
	cd $(OUT_DIR) && rm x86_64_linux*.ads
	sed -e 's/x86_64_linux_gnu_bits_stdint_u*intn_h/stdint_h/g' -i $(OUT_DIR)/llvm-*.ads
	sed -e 's/x86_64_linux_gnu_sys_types_h/stddef_h/g' -i $(OUT_DIR)/llvm-*.ads
	./py/undupwiths.py out/llvm*.ads
	cp py/stddef_h.ads.proto $(OUT_DIR)/stddef_h.ads
	cp py/stdint_h.ads.proto $(OUT_DIR)/stdint_h.ads
	rm -rf gen
	mv out gen
	mkdir out
	./py/common.py generate_wrappers gen/llvm*.ads
	ls gen/*.ads | grep -v llvm | xargs -i cp {} out/
	rm -rf gen

$(OUT_DIR)/clang-index.adb: install py/common.py py/wrapper.py
	rm -rf $(TMP_GEN_DIR) $(OUT_DIR)
	mkdir -p $(TMP_GEN_DIR) $(OUT_DIR)
	cd $(TMP_GEN_DIR) && g++ -I$(LLVM_INCLUDE_DIR) -I$(PWD)/llvm-obj/include -DNDEBUG -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -U__cplusplus -c -D_Bool=bool -Dlto_api_version=lto_api_version_fun -fdump-ada-spec -C $(CLANG_INCLUDE_DIR_C)/*.h
	sed -e "s^$(PWD)/^^g" -i $(TMP_GEN_DIR)/clang_c_*_h.ads
	sed -e 's/\(with Interfaces.C; use Interfaces.C;\)/pragma Warnings (Off); \1 pragma Warnings (On);/g' -i $(TMP_GEN_DIR)/clang_c_*_h.ads
	echo -n "pragma Style_Checks (Off); package Clang is end Clang;" > $(OUT_DIR)/clang.ads
	./py/common.py process_names_clang gen/clang_c_*_h.ads
	cd $(OUT_DIR) && rm x86_64_linux*.ads
	sed -e 's/x86_64_linux_gnu_bits_types_time_t_h/time_h/g' -i $(OUT_DIR)/clang-*.ads
	./py/undupwiths.py out/clang*.ads
	cp py/stddef_h.ads.proto $(OUT_DIR)/stddef_h.ads
	cp py/time_h.ads.proto $(OUT_DIR)/time_h.ads
	rm -rf gen
	mv out gen
	mkdir out
	./py/common.py generate_wrappers gen/clang*.ads
	ls gen/*.ads | grep -v clang | xargs -i cp {} out/
	rm -rf gen

clean:
	rm -rf $(TMP_GEN_DIR) $(OUT_DIR) llvm-obj $(LLVM_INSTALL_DIR)

distclean: clean
	rm -rf $(LLVM_SRC_DIR)
