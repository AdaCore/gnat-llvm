PWD:=$(shell pwd)
TMP_GEN_DIR=gen
OUT_DIR=out
PARALLEL=24
LLVM_SRC_DIR=llvm-5.0.0.src
LLVM_INCLUDE_DIR=$(PWD)/$(LLVM_SRC_DIR)/include
LLVM_INCLUDE_DIR_C=$(LLVM_INCLUDE_DIR)/llvm-c
DL=wget
LLVM_SRC_FNAME=llvm-5.0.0.src.tar.xz
LLVM_SRC_LINK=http://releases.llvm.org/5.0.0/$(LLVM_SRC_FNAME)

all: setup llvm

binding: $(OUT_DIR)/llvm-core.adb

CONFIGURE=cd llvm-obj && CXX=g++ CC=gcc CXXC=g++ \
  cmake -G "Unix Makefiles" -DCMAKE_INSTALL_PREFIX=$(PWD)/install \
  -DCMAKE_EXE_LINKER_FLAGS="-static-libgcc -static-libstdc++" ../$(LLVM_SRC_DIR)
# -DCMAKE_BUILD_TYPE=RelWithDebInfo

setup:
	mkdir -p llvm-obj
	if [ ! -d $(LLVM_SRC_DIR) ]; then $(DL) $(LLVM_SRC_LINK) && tar xavf $(LLVM_SRC_FNAME) && rm $(LLVM_SRC_FNAME) && $(CONFIGURE) ; fi

configure:
	$(CONFIGURE)

llvm:
	$(MAKE) -s -j$(PARALLEL) -C llvm-obj

$(OUT_DIR)/llvm-core.adb: $(LLVM_INCLUDE_DIR_C)/Core.h
	mkdir -p $(TMP_GEN_DIR) $(OUT_DIR)
	cd $(TMP_GEN_DIR) && g++ -I$(LLVM_INCLUDE_DIR) -I$(PWD)/llvm-obj/include -DNDEBUG -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -U__cplusplus -c -D_Bool=bool -Dlto_api_version=lto_api_version_fun -fdump-ada-spec -C $(LLVM_INCLUDE_DIR_C)/*.h
	sed -e 's/pragma Import (CPP, \(.*\), ".*");/pragma Import (C, \1, "\1");/g' -e "s^$(PWD)/^^g" -i $(TMP_GEN_DIR)/llvm_c_*_h.ads
	sed -e 's/\(with Interfaces.C; use Interfaces.C;\)/pragma Warnings (Off); \1 pragma Warnings (On);/g' -i $(TMP_GEN_DIR)/llvm_c_*_h.ads
	cd $(TMP_GEN_DIR) && gnat2xml -q -mxml llvm_c_*_h.ads
	echo "pragma Style_Checks (Off); package LLVM is end LLVM;" > $(OUT_DIR)/llvm.ads
	./py/common.py process_names gen/llvm_c_*_h.ads
	cd $(OUT_DIR) && gnat2xml -q -mxml llvm*.ads
	rm -rf gen
	mv out gen
	mkdir out
	./py/common.py generate_wrappers gen/llvm*.ads
	ls gen/*.ads | grep -v llvm | xargs -i cp {} out/
	rm -rf gen

clean:
	rm -rf $(TMP_GEN_DIR) $(OUT_DIR)

distclean: clean
	rm -rf $(LLVM_SRC_DIR)

