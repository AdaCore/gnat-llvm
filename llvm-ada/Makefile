TMP_GEN_DIR=gen
OUT_DIR=out
LLVM_INCLUDE_DIR=$(shell pwd)/llvm-3.3.src/include
LLVM_INCLUDE_DIR_C=$(LLVM_INCLUDE_DIR)/llvm-c
DL=wget
LLVM_SRC_FNAME=llvm-3.3.src.tar.gz
LLVM_SRC_LINK=http://llvm.org/releases/3.3/$(LLVM_SRC_FNAME)

ifeq ($(wildcard out/llvm-core.adb),)
all: generate_bindings
else
all:
endif

init: init_gen init_llvm_src

init_gen:
	mkdir -p $(TMP_GEN_DIR) $(OUT_DIR)

init_llvm_src:
	if [ ! -d llvm-3.3.src ]; then $(DL) $(LLVM_SRC_LINK) && tar xvf $(LLVM_SRC_FNAME) && rm $(LLVM_SRC_FNAME) && cd llvm-3.3.src && ./configure ; fi

generate_fdump_bindings: init
	cd $(TMP_GEN_DIR) && g++ -I/usr/include/x86_64-linux-gnu -I$(LLVM_INCLUDE_DIR) -DNDEBUG -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -U__cplusplus -c -D_Bool=bool -fdump-ada-spec $(LLVM_INCLUDE_DIR_C)/*.h

clean_imports: generate_fdump_bindings
	sed -e 's/pragma Import (CPP, \(.*\), ".*");/pragma Import (C, \1, "\1");/g' -i $(TMP_GEN_DIR)/llvm_c_*_h.ads
	sed -e 's/\(with Interfaces.C; use Interfaces.C;\)/pragma Warnings (Off); \1 pragma Warnings (On);/g' -i $(TMP_GEN_DIR)/llvm_c_*_h.ads

generate_xml: clean_imports
	cd $(TMP_GEN_DIR) && gnat2xml -mxml llvm_c_*_h.ads

generate_xml_2: py_processing
	cd $(OUT_DIR) && gnat2xml -mxml llvm*.ads

create_base_package:
	echo "pragma Style_Checks (Off); package LLVM is end LLVM;" > $(OUT_DIR)/llvm.ads

py_processing: generate_xml create_base_package
	./py/common.py process_names gen/llvm_c_*_h.ads

generate_wrappers: generate_xml_2 
	rm gen -rf && mv out gen && mkdir out && ./py/common.py generate_wrappers gen/llvm*.ads
	ls gen/*.ads | grep -v llvm | xargs -i cp {} out/

generate_bindings: generate_wrappers

clean:
	rm $(TMP_GEN_DIR) -rf
	rm $(OUT_DIR) -rf

distclean: clean
	rm llvm-3.3.src -rf
