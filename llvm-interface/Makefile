# number of processors
PROCS=0

RM=rm -f
RMDIR=rm -rf
MV=mv -f
MKDIR=mkdir -p
CHMOD=chmod
AR=ar q
LN_S=ln -sf
CP_DIR=cp -pr

EXE=

GNATMAKE=gnatmake

GPROPT=-XBuild=Production

GNAT_SRC=$(pwd)/gnat_src
ADAINCLUDE=lib/rts-llvm/adainclude
ADALIB=lib/rts-llvm/adalib

pwd:=$(shell pwd)

LLVM_CONFIG ?= llvm-config

# If llvm-config belongs to an installation of LLVM, then its include directory
# is going to contain everything we need. Otherwise, if llvm-config belongs to a
# _build tree_, we need to add an extra directory because the Clang headers
# won't be in `llvm-config --includedir`.
CLANG_CXXFLAGS :=
CLANG_INCLUDE_DIR := $(abspath $(shell $(LLVM_CONFIG) --includedir)/../../clang/include)

ifneq ($(wildcard $(CLANG_INCLUDE_DIR)/.),)
  CLANG_CXXFLAGS := -I$(CLANG_INCLUDE_DIR) -I$(abspath $(shell $(LLVM_CONFIG) --obj-root)/tools/clang/include)
endif

LDFLAGS=$(shell $(LLVM_CONFIG) --libs all --ldflags --system-libs)
CXXFLAGS=
ALL_CXXFLAGS=$(shell $(LLVM_CONFIG) --cxxflags) $(CXXFLAGS) $(CLANG_CXXFLAGS) -I.
LLVM_INCLUDE_DIR=$(shell $(LLVM_CONFIG) --includedir)
LLVM_VERSION=$(shell $(LLVM_CONFIG) --version | cut -d. -f1)

GPRBUILD=gprbuild -v -n -XLLVM_Version=$(LLVM_VERSION) -x

ifeq ($(OS),Windows_NT)
  LN_S=cp -p
  LDFLAGS+=-Wl,--stack=0x2000000
  EXE=.exe
else
  # In order to enable pass plugins, the compiler binary needs to reexport LLVM
  # symbols to shared objects. We achieve this by building with -rdynamic (which
  # is equivalent to -Wl,--export-dynamic), just like Clang.
  LDFLAGS+=-rdynamic
endif

compare=cmp --ignore-initial=16

clang_path=$(dir $(shell which clang))

.PHONY: all setup force clean

all: setup build gnatlib

LOCAL_RT_FILES=../link.c s-pehage.adb s-pehage.ads s-rident.ads s-utf_32.adb \
  s-utf_32.ads \
  gnat.ads g-byorma.adb g-casuti.adb g-casuti.ads g-comlin.adb g-comlin.ads \
  g-ctrl_c.adb g-ctrl_c.ads g-dirope.adb g-dynhta.adb g-dynhta.ads \
  g-graphs.adb g-graphs.ads g-hesora.adb g-htable.adb g-io.adb g-io_aux.adb \
  g-io_aux.ads g-lists.adb g-lists.ads g-os_lib.adb g-os_lib.ads \
  g-regexp.ads g-sets.adb g-sets.ads g-spchge.adb g-speche.adb g-string.ads \
  g-u3spch.adb

GEN_IL_FILES = nmake.adb nmake.ads \
  seinfo.ads sinfo-nodes.ads sinfo-nodes.adb \
  einfo-entities.ads einfo-entities.adb

target := $(shell gcc -dumpmachine)
llvm_target := $(shell clang -dumpmachine)
target_clang := $(shell which $(target)-clang)

setup:
	$(MKDIR) obj obj-tools/libgnat bin gnat_src/vast
	for f in `cd $(GNAT_SRC); ls gen_il*.ad? xutil.ad? *-tmpl xoscons.adb xsnamest.adb`; \
	do \
	  cp -p $(GNAT_SRC)/$$f obj-tools; \
	done
	cd obj-tools && $(GNATMAKE) -q -j0 xoscons xsnamest && ./xsnamest && \
	$(MV) snames.ns ../obj/snames.ads && $(MV) snames.nb ../obj/snames.adb && \
	$(GNATMAKE) -g -q -j0 gen_il-main.adb -I../obj -Ilibgnat && ./gen_il-main && \
	$(MV) $(GEN_IL_FILES) ../obj
	for f in $(LOCAL_RT_FILES); do \
	  $(LN_S) $(GNAT_SRC)/libgnat/$$f obj; \
	done
	./check_for_LLVM_aliasing_bug.sh "$(LLVM_CONFIG)"
	./check_for_llvm_apis.sh "$(LLVM_CONFIG)"
	./configure_default_target.sh "$(llvm_target)"

build: setup force
	$(GPRBUILD) -Pgnat_llvm -j$(PROCS) \
	  -largs $(LDFLAGS) -cargs:c++ $(ALL_CXXFLAGS)
	$(GPRBUILD) -Ptools -j$(PROCS)

build-uninit: setup force
	$(GPRBUILD) -XBuild=Uninitialized -Pgnat_llvm -j$(PROCS) \
	  -largs $(LDFLAGS) -cargs:c++ $(ALL_CXXFLAGS)
	$(GPRBUILD) -Ptools -j$(PROCS)

build-opt: setup force
	$(GPRBUILD) $(GPROPT) -Pgnat_llvm -j$(PROCS) \
	  -largs $(LDFLAGS) -cargs:c++ $(ALL_CXXFLAGS)
	$(GPRBUILD) $(GPROPT) -Ptools -j$(PROCS)

# Target for building with coverage instrumentation.
build-cov: setup force
	$(RMDIR) obj/cov bin/cov
	gnatcov instrument -Pgnat_llvm --subdirs=cov --level=stmt --dump-trigger=atexit
	gnatcov instrument -Ptools --subdirs=cov --level=stmt --dump-trigger=atexit
	$(GPRBUILD) -Pgnat_llvm -j$(PROCS) -O \
	  --subdirs=cov --src-subdirs=gnatcov-instr \
	  --implicit-with=$(pwd)/../gnatcov_rts-build/install/share/gpr/gnatcov_rts_full.gpr \
	  -largs $(LDFLAGS) -cargs:c++ $(ALL_CXXFLAGS)
	$(GPRBUILD) -Ptools -j$(PROCS) -O \
	  --subdirs=cov --src-subdirs=gnatcov-instr \
	  --implicit-with=$(pwd)/../gnatcov_rts-build/install/share/gpr/gnatcov_rts_full.gpr \
	  -largs $(LDFLAGS)

cov-info:
	gnatcov coverage -Pgnat_llvm --level=stmt --save-checkpoint=acats.ckpt \
	  --subdirs=cov @$(pwd)/../acats/acats/gnat.traces
	gnatcov coverage -Pgnat_llvm --level=stmt \
	  --save-checkpoint=fixedbugs.ckpt \
	  --subdirs=cov @$(pwd)/../fixedbugs/gnat.traces
	gnatcov coverage --level=stmt --annotate=dhtml \
	  --checkpoint=acats.ckpt --checkpoint=fixedbugs.ckpt \
	  --output-dir=dhtml-report

# Bootstrap targets

# We want to use Clang for C and C++ compilation during bootstrap. However,
# since our LLVM libraries expect libstdc++ at the moment, we tell Clang to use
# our GCC's C++ runtime. Providing the path explicitly is important because
# otherwise Clang falls back to the system installation of GCC, if any; this
# can lead to crashes on some Linux distributions.

CXX_PREFIX=$(shell dirname $(dir $(shell which gcc)))
BOOT_DIRS=obj obj-tools bin lib
GCC_TOOLCHAIN=--gcc-toolchain="$(CXX_PREFIX)" --stdlib=libstdc++
BOOTSTRAP_GPRBUILD=$(GPRBUILD) -cargs:c++ $(GCC_TOOLCHAIN) -largs $(GCC_TOOLCHAIN) -margs

stage1:
	$(MAKE) build-opt gnatlib-automated
	$(RMDIR) stage1
	$(MKDIR) stage1
	$(MV) $(BOOT_DIRS) stage1

stage2: stage1
	PATH=$(clang_path):$(pwd)/stage1/bin:$$PATH $(MAKE) CXX_PREFIX=$(CXX_PREFIX) GNATMAKE=$(pwd)/stage1/bin/llvm-gnatmake GPRBUILD="$(BOOTSTRAP_GPRBUILD)" build-opt gnatlib-automated
	$(RMDIR) stage2
	$(MKDIR) stage2
	$(MV) $(BOOT_DIRS) stage2

stage3: stage2
	PATH=$(clang_path):$(pwd)/stage2/bin:$$PATH $(MAKE) CXX_PREFIX=$(CXX_PREFIX) GNATMAKE=$(pwd)/stage2/bin/llvm-gnatmake GPRBUILD="$(BOOTSTRAP_GPRBUILD)" build-opt gnatlib-automated
	@echo "Comparing stage2 and stage3 objects"
	$(MAKE) bootstrap-compare

bootstrap: stage3

unbootstrap:
	rm -rf $(BOOT_DIRS)
	mv stage1/* .
	rm -rf stage[12]

.PHONY: bootstrap-compare

bootstrap-compare:
	@rm -rf bootstrap-compare
	@mkdir -p bootstrap-compare
	@for f in `cd stage2/obj && echo *.o`; do \
	  objcopy --strip-debug stage2/obj/$$f bootstrap-compare/stage2-$$f; \
	  objcopy --strip-debug obj/$$f bootstrap-compare/stage3-$$f; \
	  $(compare) bootstrap-compare/stage?-$$f || exit 1; \
	done
	@rm -rf bootstrap-compare
	@echo "Comparison successful."

force:

clean:
	$(RMDIR) obj obj-tools lib libexec stage1 stage2 bootstrap-compare

# Full runtime

# Note some of these target(.*) variables are used in gnat_src's Makefiles.
# Renaming them will lead to runtime compilation errors.

target_list=$(filter-out gnu,$(subst -, ,$(target)))
target_cpu=$(word 1,$(target_list))

ifeq ($(words $(target_list)),2)
  target_vendor=
  target_os=$(word 2,$(target_list))
else
  target_vendor=$(word 2,$(target_list))
  target_os=$(word 3,$(target_list))
endif

RTSBASE=rts-native
RTS=$(pwd)/lib/gnat-llvm/$(llvm_target)/$(RTSBASE)
RTSDIR=$(RTS)/adainclude
RTSLIB=$(RTS)/adalib
RTSOBJ=$(RTS)/obj-static

LIBEXEC_DIR := $(pwd)/libexec/gnat-llvm/$(llvm_target)

ADAFLAGS=-gnatpg
CFLAGS=-O2
EXTRALIBFLAGS=
LIBRARY_KIND=static

# used in Makefile.rtl
ADAC=$(pwd)/bin/llvm-gcc
CROSSGCC=gcc

OSCONS_CC=$(CROSSGCC)
OSCONS_CPP=$(OSCONS_CC) -E -C -DTARGET=\"$(target)\" -iquote $(GNAT_SRC) \
  $(GNAT_SRC)/s-oscons-tmplt.c > s-oscons-tmplt.i
OSCONS_EXTRACT=$(OSCONS_CC) -S s-oscons-tmplt.i

objext=.o

include $(GNAT_SRC)/Makefile.rtl

automated: build-opt
ifeq ($(strip $(filter-out vxworks7r2,$(target_os))),)
	$(MAKE) gnatlib-automated RTSSWITCH=--RTS:C=kernel RTSBASE=rts-kernel THREAD_KIND=kernel-smp
	$(MAKE) gnatlib-automated RTSSWITCH=--RTS:C=rtp RTSBASE=rts-rtp THREAD_KIND=rtp-smp
else ifeq ($(strip $(filter-out linux,$(target_os))),)
	$(RMDIR) $(RTS)
	$(MAKE) quicklib LIBRARY_KIND=static-pic LN_S="cp -p"
	$(MV) $(RTSLIB)/libgnat_pic.a .
	$(MV) $(RTSLIB)/libgnarl_pic.a .
	$(RMDIR) $(RTS)
	$(MAKE) quicklib LIBRARY_KIND=dynamic GPRBUILD="$(GPRBUILD) --gpr=2" LN_S="cp -p"
	$(MV) $(RTSLIB)/libgna*.so .
	$(MAKE) gnatlib-automated
	$(MV) libgnat_pic.a $(RTSLIB)
	$(MV) libgnarl_pic.a $(RTSLIB)
	$(MV) libgna*.so $(RTSLIB)
else
	$(MAKE) gnatlib-automated
endif

gnatlib: build
	$(RMDIR) $(RTS)
	$(MAKE) CFLAGS="$(CFLAGS) -g" EXTRALIBFLAGS="$(EXTRALIBFLAGS)" quicklib

gnatlib-automated: build-opt
	$(RMDIR) $(RTS)
	$(MAKE) CFLAGS="$(CFLAGS)" \
	  EXTRALIBFLAGS="$(EXTRALIBFLAGS)" \
	  RTSSWITCH="$(RTSSWITCH)" \
	  LN_S="cp -p" quicklib
	$(RMDIR) $(RTSOBJ)

gnatlib-symcc-automated: setup
	$(RMDIR) $(RTS)
	$(MAKE) \
	  CFLAGS="-fpass-plugin=libsymcc.so $(CFLAGS)" \
	  EXTRALIBFLAGS="$(EXTRALIBFLAGS)" \
	  LN_S="cp -p" \
	  quicklib
	$(RMDIR) $(RTSOBJ)

gnatlib-shared:
	$(MAKE) CFLAGS="$(CFLAGS)" EXTRALIBFLAGS="$(EXTRALIBFLAGS)" \
	  LIBRARY_KIND=dynamic gnatlib

gnatlib-bc:
	PATH=$(pwd)/bin:$(clang_path):$$PATH \
	$(GPRBUILD) -c --subdirs=bc -j$(PROCS) \
	  -P$(RTSDIR)/libada \
	  -XCFLAGS="$(CFLAGS) -I../../adainclude -I../../include -emit-llvm"
	@echo "creating libgnat.bc"
	@cd $(RTSOBJ)/bc && llvm-link $(subst .o,.bc,$(GNATRTL_NONTASKING_OBJS) $(LIBGNAT_OBJS)) -o=libgnat.bc-tmp
	@echo "creating libgnarl.bc under $(RTSOBJ)/bc"
	@cd $(RTSOBJ)/bc && llvm-link $(subst .o,.bc,$(GNATRTL_TASKING_OBJS)) -o=libgnarl.bc-tmp
	@cd $(RTSOBJ)/bc && $(RM) *.bc && $(MV) libgnat.bc-tmp libgnat.bc && $(MV) libgnarl.bc-tmp libgnarl.bc

# Clang 16 complains about a missing function declaration in raise-gcc.c
# (eng/toolchain/gnat-llvm#5). Until we have a proper fix, disable the warning
# when building the runtime.
quicklib: $(RTSDIR) $(LIBEXEC_DIR)
	PATH=$(pwd)/bin:$(clang_path):$$PATH \
	$(GPRBUILD) -p --target=$(target) $(RTSSWITCH) -j$(PROCS) -P$(RTSDIR)/libada \
	  -XCFLAGS="$(CFLAGS) -Wno-implicit-function-declaration -I$(LLVM_INCLUDE_DIR)" \
	  -XEXTRALIBFLAGS="$(EXTRALIBFLAGS) -gdwarf-aranges" \
	  -XLIBRARY_KIND=$(LIBRARY_KIND) -XLIBRARY_VERSION=$(LIBRARY_VERSION)
	cd $(RTSLIB) && $(CHMOD) a-wx *.ali

$(RTSDIR):
	$(RMDIR) $(RTS)
	$(MKDIR) $(RTSLIB)
	$(MAKE) setup-rts
	$(CP_DIR) include $(RTS)
	cd $(RTSLIB) && $(OSCONS_CPP) && $(OSCONS_EXTRACT) && \
	  $(pwd)/obj-tools/xoscons s-oscons && \
	  $(MV) s-oscons.ads $(RTSDIR) && $(MV) s-oscons.h $(RTSDIR) && \
	  $(RM) s-oscons-tmplt.*

# llvm-gcc normally links with the Clang binary found on the PATH. If a
# different binary is needed (such as with our target-specific Clang binaries),
# we need to create a symlink for llvm-gcc.
$(LIBEXEC_DIR):
ifneq ($(target_clang),)
	$(MKDIR) "$(LIBEXEC_DIR)/bin"
	$(LN_S) "$(target_clang)" $(LIBEXEC_DIR)/bin/llvm-helper$(EXE)
	$(LN_S) "`dirname $(target_clang)`/ld.lld$(EXE)" $(LIBEXEC_DIR)/bin/ld.lld$(EXE)
	$(CP_DIR) "`dirname $(target_clang)`/../lib" $(LIBEXEC_DIR)/lib
endif

# ZFP runtime

COMMON_ZFP_SRCS= \
ada.ads a-assert.adb a-assert.ads a-unccon.ads a-uncdea.ads \
interfac.ads s-stoele.adb s-stoele.ads s-unstyp.ads

ZFPINCLUDE=lib/rts-zfp/adainclude
ZFPLIB=lib/rts-zfp/adalib

zfp-build:
	$(RMDIR) $(ZFPINCLUDE) $(ZFPLIB)
	$(MKDIR) $(ZFPINCLUDE) $(ZFPLIB)
	for f in $(COMMON_ZFP_SRCS); do \
	  cp -p $(GNAT_SRC)/libgnat/$$f $(ZFPINCLUDE); \
	done
	cp -p zfp/*.* $(ZFPINCLUDE)
	PATH=$(pwd)/bin:$(clang_path):$$PATH \
	$(GPRBUILD) -p --target=$(target) --RTS=zfp -j$(PROCS) -P$(ZFPINCLUDE)/zfp \
	  -XCFLAGS="$(CFLAGS)" \
	  -XEXTRALIBFLAGS="$(EXTRALIBFLAGS)" \
	  -XLIBRARY_KIND=$(LIBRARY_KIND) \
	  -XADAFLAGS=$(ADAFLAGS)
	cd $(ZFPLIB) && $(CHMOD) a-wx *.ali

zfp: build
	$(MAKE) CFLAGS="$(CFLAGS) -g" EXTRALIBFLAGS="$(EXTRALIBFLAGS)" zfp-build

zfp-opt: build-opt
	$(MAKE) CFLAGS="$(CFLAGS)" EXTRALIBFLAGS="$(EXTRALIBFLAGS)" zfp-build

# Optionally include sub makefile

-include Makefile.target
