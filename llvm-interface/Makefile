GPRBUILD=gprbuild -v -n

# number of processors
PROCS=0

RM=rm -f
RMDIR=rm -rf
MV=mv -f
MKDIR=mkdir -p
CHMOD=chmod
AR=ar q
LN_S=ln -sf

GNATMAKE=gnatmake

GPROPT=-XBuild=Production

GNAT_SRC=$(pwd)/gnat_src
ADAINCLUDE=lib/rts-llvm/adainclude
ADALIB=lib/rts-llvm/adalib

pwd:=$(shell pwd)

ifneq ($(wildcard $(pwd)/../llvm/llvm-obj/bin/llvm-config),)
  LLVM_CONFIG=$(pwd)/../llvm/llvm-obj/bin/llvm-config
else
  LLVM_CONFIG=llvm-config
endif

LDFLAGS=$(shell $(LLVM_CONFIG) --libs all --ldflags --system-libs) \
  -static-libstdc++
CXXFLAGS=
ALL_CXXFLAGS=$(shell $(LLVM_CONFIG) --cxxflags) $(CXXFLAGS)

ifeq ($(OS),Windows_NT)
  LN_S=cp -p
  LDFLAGS+=-Wl,--stack=0x2000000
endif

compare=cmp --ignore-initial=16

.PHONY: setup force clean

all: setup build
	$(MAKE) quicklib

LOCAL_RT_FILES=../link.c s-pehage.adb s-pehage.ads s-rident.ads s-utf_32.adb \
  s-utf_32.ads \
  gnat.ads g-byorma.adb g-casuti.adb g-casuti.ads g-comlin.adb g-comlin.ads \
  g-dirope.adb g-dynhta.adb g-dynhta.ads g-graphs.adb g-graphs.ads \
  g-hesora.adb g-htable.adb g-io.adb g-io_aux.adb g-io_aux.ads g-lists.adb \
  g-lists.ads g-os_lib.adb g-os_lib.ads g-regexp.ads g-sets.adb g-sets.ads \
  g-spchge.adb g-speche.adb g-string.ads g-u3spch.adb

setup:
	$(MKDIR) obj obj-tools bin gnat_src/vast
	for f in `cd $(GNAT_SRC); ls xtreeprs.adb xnmake.adb xutil.ad? *-tmpl xsnamest.adb sinfo.ads treeprs.adt nmake.adt xoscons.adb xutil.ads xutil.adb`; \
	do \
	  cp -p $(GNAT_SRC)/$$f obj-tools; \
	done
	cd obj-tools && $(GNATMAKE) -q -j0 xtreeprs xnmake xsnamest xoscons && \
	./xtreeprs && ./xnmake && ./xsnamest && \
	$(MV) nmake.ads nmake.adb treeprs.ads ../obj && \
	$(MV) snames.ns ../obj/snames.ads && $(MV) snames.nb ../obj/snames.adb
	for f in $(LOCAL_RT_FILES); do \
	  $(LN_S) $(GNAT_SRC)/libgnat/$$f obj; \
	done
	./check_for_LLVM_aliasing_bug.sh

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
	  --implicit-with=$(pwd)/../gnatcov_rts-build/install/share/gpr/gnatcov_rts_full.gpr

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

CXX_PREFIX=$(shell dirname $(dir $(shell which gcc)))
LD_PATH=$(shell dirname $(shell gcc --print-prog-name=cc1))
LIBSTDCXX_PATH=$(CXX_PREFIX)/lib64
BOOT_DIRS=obj obj-tools bin lib

stage1: 
	$(MAKE) GPRBUILD="gprbuild -v -gnatwns" build-opt gnatlib-automated
	$(RMDIR) stage1
	$(MKDIR) stage1
	$(MV) $(BOOT_DIRS) stage1

stage2: stage1
	PATH=$(pwd)/stage1/bin:$(LD_PATH):$$PATH $(MAKE) CXX_PREFIX=$(CXX_PREFIX) GNATMAKE=$(pwd)/stage1/bin/llvm-gnatmake GPRBUILD="gprbuild -v --target=llvm -largs -L$(LIBSTDCXX_PATH) -margs" build-opt gnatlib-automated
	$(RMDIR) stage2
	$(MKDIR) stage2
	$(MV) $(BOOT_DIRS) stage2

stage3: stage2
	PATH=$(pwd)/stage2/bin:$(LD_PATH):$$PATH $(MAKE) CXX_PREFIX=$(CXX_PREFIX) GNATMAKE=$(pwd)/stage2/bin/llvm-gnatmake GPRBUILD="gprbuild -v --target=llvm -largs -L$(LIBSTDCXX_PATH) -margs" build-opt gnatlib-automated
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
	$(RMDIR) obj obj-tools lib stage1 stage2 bootstrap-compare

# Full runtime

target=$(shell gcc -dumpmachine)

target_list=$(filter-out gnu,$(subst -, ,$(target)))
target_cpu=$(word 1,$(target_list))

ifeq ($(words $(target_list)),2)
  target_vendor=
  target_os=$(word 2,$(target_list))
else
  target_vendor=$(word 2,$(target_list))
  target_os=$(word 3,$(target_list))
endif

RTS=$(pwd)/lib/rts-native
RTSDIR=$(RTS)/adainclude
RTSLIB=$(RTS)/adalib
RTSOBJ=$(RTS)/obj-static

ADAFLAGS=-gnatpg
CFLAGS=-O2 -g
EXTRALIBFLAGS=
LIBRARY_KIND=static

ADAC=$(pwd)/bin/llvm-gcc
CROSSGCC=gcc

OSCONS_CC=$(CROSSGCC)
OSCONS_CPP=$(OSCONS_CC) -E -C -DTARGET=\"$(target)\" -iquote $(GNAT_SRC) \
  $(GNAT_SRC)/s-oscons-tmplt.c > s-oscons-tmplt.i
OSCONS_EXTRACT=$(OSCONS_CC) -S s-oscons-tmplt.i

objext=.o

include $(GNAT_SRC)/Makefile.rtl

# Disable runtime units that depend on 128bits support
export GNATRTL_128BIT_PAIRS=
export GNATRTL_128BIT_OBJS=

gnatlib: build
	$(RMDIR) $(RTS)
	$(MAKE) CFLAGS="$(CFLAGS)" EXTRALIBFLAGS="$(EXTRALIBFLAGS)" quicklib

gnatlib-automated: build-opt
	$(RMDIR) $(RTS)
	$(MAKE) CFLAGS="$(CFLAGS)" EXTRALIBFLAGS="$(EXTRALIBFLAGS)" \
	  LN_S="cp -p" quicklib
	$(RMDIR) $(RTSOBJ)

gnatlib-shared:
	$(MAKE) CFLAGS="$(CFLAGS)" EXTRALIBFLAGS="$(EXTRALIBFLAGS)" \
	  LIBRARY_KIND=dynamic gnatlib

gnatlib-bc:
	PATH=$(pwd)/bin:$$PATH \
	$(GPRBUILD) -c --subdirs=bc --target=llvm -j$(PROCS) \
	  -P$(RTSDIR)/libada \
	  -XCFLAGS="$(CFLAGS) -I../../adainclude -I../../include -emit-llvm"
	@echo "creating libgnat.bc"
	@cd $(RTSOBJ)/bc && llvm-link $(subst .o,.bc,$(GNATRTL_NONTASKING_OBJS) $(LIBGNAT_OBJS)) -o=libgnat.bc-tmp
	@echo "creating libgnarl.bc under $(RTSOBJ)/bc"
	@cd $(RTSOBJ)/bc && llvm-link $(subst .o,.bc,$(GNATRTL_TASKING_OBJS)) -o=libgnarl.bc-tmp
	@cd $(RTSOBJ)/bc && $(RM) *.bc && $(MV) libgnat.bc-tmp libgnat.bc && $(MV) libgnarl.bc-tmp libgnarl.bc

quicklib: $(RTSDIR)
	PATH=$(pwd)/bin:$$PATH \
	$(GPRBUILD) -p --target=llvm -j$(PROCS) -P$(RTSDIR)/libada \
	  -XCFLAGS="$(CFLAGS)" \
	  -XEXTRALIBFLAGS="$(EXTRALIBFLAGS) -gdwarf-aranges" \
	  -XLIBRARY_KIND=$(LIBRARY_KIND)
	cd $(RTSLIB) && $(CHMOD) a-wx *.ali

$(RTSDIR): bin/llvm-gnat1
	$(RMDIR) $(RTS)
	$(MKDIR) $(RTSLIB)
	$(MAKE) setup-rts
	cp -pr include $(RTS)
	cd $(RTSLIB) && $(OSCONS_CPP) && $(OSCONS_EXTRACT) && \
	  $(pwd)/obj-tools/xoscons s-oscons && \
	  $(MV) s-oscons.ads $(RTSDIR) && $(MV) s-oscons.h $(RTSDIR) && \
	  $(RM) s-oscons-tmplt.*

# Optionally include sub makefile

-include Makefile.target
