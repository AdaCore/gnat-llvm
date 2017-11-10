TMP_GEN_DIR=gen
OUT_DIR=out
LLVM_INCLUDE_DIR=$(shell pwd)/llvm-3.3.src/include
LLVM_INCLUDE_DIR_C=$(LLVM_INCLUDE_DIR)/llvm-c
DL=wget
LLVM_SRC_FNAME=llvm-3.3.src.tar.gz
LLVM_SRC_LINK=http://llvm.org/releases/3.3/$(LLVM_SRC_FNAME)

all: setup $(OUT_DIR)/llvm-core.adb

CONFIGURE=cd llvm-3.3.src && ./configure
setup:
	mkdir -p $(TMP_GEN_DIR) $(OUT_DIR)
	if [ ! -d llvm-3.3.src ]; then $(DL) $(LLVM_SRC_LINK) && tar xvf $(LLVM_SRC_FNAME) && rm $(LLVM_SRC_FNAME) && $(CONFIGURE) ; fi

configure:
	$(CONFIGURE)

$(OUT_DIR)/llvm-core.adb: $(LLVM_INCLUDE_DIR_C)/Core.h
	cd $(TMP_GEN_DIR) && g++ -I/usr/include/x86_64-linux-gnu -I$(LLVM_INCLUDE_DIR) -DNDEBUG -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -U__cplusplus -c -D_Bool=bool -fdump-ada-spec $(LLVM_INCLUDE_DIR_C)/*.h
	sed -e 's/pragma Import (CPP, \(.*\), ".*");/pragma Import (C, \1, "\1");/g' -i $(TMP_GEN_DIR)/llvm_c_*_h.ads
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

clean:
	rm -rf $(TMP_GEN_DIR)
	rm -rf $(OUT_DIR)

distclean: clean
	rm -rf llvm-3.3.src

