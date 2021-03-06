# Note that this Makefile is optional and mainly used by core GNAT LLVM
# developers: you can use an existing install of LLVM instead.
# llvm-interface/Makefile will work as long as llvm-config is found in your
# PATH.

PWD:=$(shell pwd)
TMP_GEN_DIR=gen
OUT_DIR=out
LLVM_RELEASE=12.0.0
LLVM_SRC_DIR=llvm-$(LLVM_RELEASE).src
LLVM_CLANG_EXP_NAME=clang-$(LLVM_RELEASE).src
LLVM_INCLUDE_DIR=$(PWD)/$(LLVM_SRC_DIR)/include
LLVM_INCLUDE_DIR_C=$(LLVM_INCLUDE_DIR)/llvm-c
CLANG_INCLUDE_DIR=$(PWD)/$(LLVM_SRC_DIR)/tools/clang/include
CLANG_INCLUDE_DIR_C=$(CLANG_INCLUDE_DIR)/clang-c
DL=wget
LLVM_SRC_FNAME=$(LLVM_SRC_DIR).tar.xz
LLVM_CLANG_SRC_FNAME=$(LLVM_CLANG_EXP_NAME).tar.xz
LLVM_RELEASE_URL=https://github.com/llvm/llvm-project/releases/download/llvmorg-$(LLVM_RELEASE)
LLVM_SRC_LINK=$(LLVM_RELEASE_URL)/$(LLVM_SRC_FNAME)
LLVM_CLANG_SRC_LINK=$(LLVM_RELEASE_URL)/$(LLVM_CLANG_SRC_FNAME)
LLVM_TARGETS=X86
CMAKE_FLAGS=-DLLVM_TARGETS_TO_BUILD="$(LLVM_TARGETS)" \
	-DBUILD_SHARED_LIBS=ON
CMAKE_FLAGS_NO_DEBUG=-DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_ASSERTIONS=ON
CMAKE_FLAGS_PROD=-DCMAKE_BUILD_TYPE=RelWithDebInfo

.PHONY: configure

all: setup llvm

# Bindings generation works with GNAT versions starting from 22.0
# (in terms of date > 2021-04-07)

llvm-bindings: $(OUT_DIR)/llvm-core.adb
	rm -rf llvm-bindings
	mv $(OUT_DIR) llvm-bindings

clang-bindings: $(OUT_DIR)/clang-index.adb
	rm -rf clang-bindings
	mv $(OUT_DIR) clang-bindings

CONFIGURE=cd llvm-obj && CXX=g++ CC=gcc CXXC=g++ \
  cmake -G "Unix Makefiles" -DCMAKE_INSTALL_PREFIX=$(PWD)/install \
  $(CMAKE_FLAGS) ../$(LLVM_SRC_DIR)

setup: $(LLVM_SRC_DIR) llvm-obj

$(LLVM_SRC_DIR):
	$(DL) $(LLVM_SRC_LINK)
	tar xJf $(LLVM_SRC_FNAME)
	rm $(LLVM_SRC_FNAME)
	$(DL) $(LLVM_CLANG_SRC_LINK)
	tar xJf $(LLVM_CLANG_SRC_FNAME)
	mv $(LLVM_CLANG_EXP_NAME) $(LLVM_SRC_DIR)/tools/clang
	rm $(LLVM_CLANG_SRC_FNAME)

llvm-obj:
	mkdir -p llvm-obj
	$(CONFIGURE)

configure:
	mkdir -p llvm-obj
	$(CONFIGURE)

configure-no-debug:
	mkdir -p llvm-obj
	$(CONFIGURE) $(CMAKE_FLAGS_NO_DEBUG)

configure-prod:
	mkdir -p llvm-obj
	$(CONFIGURE) $(CMAKE_FLAGS_PROD)

llvm:
	$(MAKE) -s -C llvm-obj

# Note: in order to use py/common.py you need to first install libadalang
# and then run:
# pip install <prefix>/share/libadalang/python/Libadalang-*.whl

$(OUT_DIR)/llvm-core.adb: $(LLVM_INCLUDE_DIR_C)/Core.h py/common.py py/wrapper.py
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

$(OUT_DIR)/clang-index.adb: $(CLANG_INCLUDE_DIR_C)/Index.h py/common.py py/wrapper.py
	rm -rf $(TMP_GEN_DIR) $(OUT_DIR)
	mkdir -p $(TMP_GEN_DIR) $(OUT_DIR)
	cd $(TMP_GEN_DIR) && g++ -I$(LLVM_INCLUDE_DIR) -I$(PWD)/llvm-obj/include -DNDEBUG -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -U__cplusplus -c -D_Bool=bool -Dlto_api_version=lto_api_version_fun -fdump-ada-spec -C $(CLANG_INCLUDE_DIR_C)/*.h
	sed -e 's/\(with Interfaces.C; use Interfaces.C;\)/pragma Warnings (Off); \1 pragma Warnings (On);/g' -i $(TMP_GEN_DIR)/clang_c_*_h.ads
	echo -n "pragma Style_Checks (Off); package Clang is end Clang;" > $(OUT_DIR)/clang.ads
	./py/common.py process_names_clang gen/clang_c_*_h.ads
	./py/undupwiths.py out/clang*.ads
	rm -rf gen
	mv out gen
	mkdir out
	./py/common.py generate_wrappers gen/clang*.ads
	ls gen/*.ads | grep -v clang | xargs -i cp {} out/
	rm -rf gen

clean:
	rm -rf $(TMP_GEN_DIR) $(OUT_DIR) llvm-obj

distclean: clean
	rm -rf $(LLVM_SRC_DIR)

