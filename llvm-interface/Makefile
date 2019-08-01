GPRBUILD=gprbuild -v

# number of processors
PROCS=0

uname=$(shell uname)

RM=rm -f
RMDIR=rm -rf
MV=mv -f
MKDIR=mkdir -p
CHMOD=chmod
AR=ar q
LN_S=ln -sf

ifeq ($(filter-out CYGWIN%,$(uname)),)
  LN_S=cp -p
endif

GNATMAKE=gnatmake

GPROPT=-XBuild=Production

GNAT_SRC=$(pwd)/gnat_src
ADAINCLUDE=lib/rts-llvm/adainclude
ADALIB=lib/rts-llvm/adalib

uname:=$(shell uname)
pwd:=$(shell pwd)

ifneq ($(wildcard $(pwd)/../llvm/llvm-obj/bin/llvm-config),)
  LLVM_CONFIG=$(pwd)/../llvm/llvm-obj/bin/llvm-config
else
  LLVM_CONFIG=llvm-config
endif

LDFLAGS=$(shell $(LLVM_CONFIG) --libs all --ldflags --system-libs) \
  -static-libstdc++
CXXFLAGS=$(shell $(LLVM_CONFIG) --cxxflags)

ifeq ($(filter-out CYGWIN%,$(uname)),)
  LDFLAGS+=-Wl,--stack=0x2000000
endif

compare=cmp --ignore-initial=16

.PHONY: setup force clean

all: setup build
	$(MAKE) quicklib

LOCAL_RT_FILES=s-rident.ads gnat.ads g-dynhta.ads g-dynhta.adb \
  g-sets.ads g-sets.adb g-lists.ads g-lists.adb g-sets.ads \
  g-graphs.ads g-graphs.adb g-htable.adb g-byorma.adb g-speche.adb \
  g-spchge.adb g-u3spch.adb g-comlin.ads g-comlin.adb g-dirope.adb \
  g-os_lib.ads g-os_lib.adb g-regexp.ads g-string.ads g-io.adb g-casuti.adb \
  g-hesora.adb g-io_aux.ads g-io_aux.adb

setup:
	$(MKDIR) obj obj-tools bin
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

build: setup force
	$(GPRBUILD) -Pgnat_llvm -j$(PROCS) \
	  -largs $(LDFLAGS) -cargs:c++ $(CXXFLAGS)
	$(GPRBUILD) -Ptools -j$(PROCS)
	$(MAKE) bugs-bins

build-uninit: setup force
	$(GPRBUILD) -XBuild=Uninitialized -Pgnat_llvm -j$(PROCS) \
	  -largs $(LDFLAGS) -cargs:c++ $(CXXFLAGS)
	$(GPRBUILD) -Ptools -j$(PROCS)
	$(MAKE) bugs-bins

build-opt: setup force
	$(GPRBUILD) $(GPROPT) -Pgnat_llvm -j$(PROCS) \
	  -largs $(LDFLAGS) -cargs:c++ $(CXXFLAGS)
	$(GPRBUILD) $(GPROPT) -Ptools -j$(PROCS)
	$(MAKE) bugs-bins

bugs-bins:
	$(RMDIR) bugs/bin
	$(MKDIR) bugs/bin
	for f in gcc gnat1 gnat gnatbind gnatchop gnatclean gnatkr gnatlink gnatls gnatmake gnatname gnatprep; do \
	  rm -f $$f; ln bin/llvm-$$f bugs/bin/$$f; \
	done
	echo "#!/bin/sh" > bugs/bin/gprbuild
	echo "`which gprbuild` --target=llvm "\$$\@ >> bugs/bin/gprbuild
	chmod +x bugs/bin/gprbuild

# Bootstrap targets

CXX_PREFIX=$(shell dirname $(dir $(shell which gcc)))
LD_PATH=$(shell dirname $(shell gcc --print-prog-name=cc1))
LIBSTDCXX_PATH=$(CXX_PREFIX)/lib64

stage1: 
	$(MAKE) GPRBUILD="gprbuild -v -gnatwns" build gnatlib
	$(RMDIR) stage1
	$(MKDIR) stage1
	$(MV) obj obj-tools bin lib stage1

stage2: stage1
	PATH=$(pwd)/stage1/bin:$(LD_PATH):$$PATH $(MAKE) CXX_PREFIX=$(CXX_PREFIX) GNATMAKE=$(pwd)/stage1/bin/llvm-gnatmake GPRBUILD="gprbuild -v --target=llvm -largs -L$(LIBSTDCXX_PATH) -margs" build gnatlib
	$(RMDIR) stage2
	$(MKDIR) stage2
	$(MV) obj obj-tools bin lib stage2

stage3: stage2
	PATH=$(pwd)/stage2/bin:$(LD_PATH):$$PATH $(MAKE) CXX_PREFIX=$(CXX_PREFIX) GNATMAKE=$(pwd)/stage2/bin/llvm-gnatmake GPRBUILD="gprbuild -v --target=llvm -largs -L$(LIBSTDCXX_PATH) -margs" build gnatlib
	@echo "Comparing stage2 and stage3 objects"
	$(MAKE) bootstrap-compare

bootstrap: stage3

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

fast:
	$(MAKE) DEBUG=0

force:

clean:
	$(RMDIR) obj obj-tools lib stage1 stage2 bootstrap-compare

# Full runtime

target=$(shell gcc -dumpmachine)

target_list=$(subst -, ,$(target))
target_cpu=$(word 1,$(target_list))
target_vendor=$(word 2,$(target_list))
target_os=$(word 3,$(target_list))

objext=.o
arext=.a
AR_FOR_TARGET=ar
AR_FLAGS=q
RANLIB_FOR_TARGET=ranlib
RTSDIR=$(pwd)/lib/rts-native/adainclude
RTSLIB=$(pwd)/lib/rts-native/adalib
RTSOBJ=$(pwd)/lib/rts-native/obj
ADAFLAGS=-gnatpg
ADA_INCLUDES=-nostdinc -I../adainclude
CFLAGS = -O2 -g
GNATLIBCFLAGS = -DIN_RTS=1 -fexceptions -DSTANDALONE
ALL_ADAFLAGS = $(CFLAGS) $(ADAFLAGS) $(ADA_INCLUDES)
INCLUDES = -I../adainclude -I../include

ADAC=$(pwd)/bin/llvm-gcc
CROSSC=clang
CROSSGCC=gcc
COMPILE=$(ADAC) -c -gnatpg -nostdinc -I../adainclude

FORCE_DEBUG_ADAFLAGS = -g
NO_INLINE_ADAFLAGS = -fno-inline
NO_OMIT_ADAFLAGS = -fno-omit-frame-pointer
NO_SIBLING_ADAFLAGS=-fno-optimize-sibling-calls
NO_REORDER_ADAFLAGS=-fno-toplevel-reorder

OSCONS_CC=$(CROSSGCC)
OSCONS_CPP=$(OSCONS_CC) -E -C -DTARGET=\"$(target)\" -iquote $(GNAT_SRC) \
  $(GNAT_SRC)/s-oscons-tmplt.c > s-oscons-tmplt.i
OSCONS_EXTRACT=$(OSCONS_CC) -S s-oscons-tmplt.i

vpath %.adb ../adainclude
vpath %.ads ../adainclude
vpath %.c   ../adainclude
vpath %.h   ../adainclude

include $(GNAT_SRC)/Makefile.rtl

.SUFFIXES: .adb .ads

.c.o:
	$(CC) -c $(CFLAGS) $(GNATLIBCFLAGS) $(INCLUDES) $<

.adb.o:
	$(ADAC) -c $(ALL_ADAFLAGS) $<

.ads.o:
	$(ADAC) -c $(ALL_ADAFLAGS) $<

gnatlib: build
	$(RMDIR) $(RTSLIB) $(RTSDIR) $(RTSOBJ)
	$(MAKE) quicklib

gnatlib-automated: build
	$(RMDIR) $(RTSLIB) $(RTSDIR) $(RTSOBJ)
	$(MAKE) quicklib LN_S="cp -p"
	$(RMDIR) $(RTSOBJ)

quicklib: $(RTSDIR)
	if [ -d $(RTSOBJ) ]; then \
	  $(MV) $(RTSOBJ)/*.o $(RTSLIB); \
	fi
	$(MAKE) GNAT_SRC=$(GNAT_SRC) -C $(RTSLIB) -f $(pwd)/Makefile \
	  ADAC=$(ADAC) CC=$(CROSSC) \
	  $(GNATRTL_OBJS) $(LIBGNAT_OBJS) $(EXTRA_ADALIB_OBJS)
# Create libgnat/libgnarl.a
	$(RM) $(RTSLIB)/libgnat$(arext) $(RTSLIB)/libgnarl$(arext)
	@$(AR_FOR_TARGET) $(AR_FLAGS) $(RTSLIB)/libgnat$(arext) \
	  $(addprefix $(RTSLIB)/,$(GNATRTL_NONTASKING_OBJS) $(LIBGNAT_OBJS))
	$(RANLIB_FOR_TARGET) $(RTSLIB)/libgnat$(arext)
	@$(AR_FOR_TARGET) $(AR_FLAGS) $(RTSLIB)/libgnarl$(arext) \
	  $(addprefix $(RTSLIB)/,$(GNATRTL_TASKING_OBJS))
	$(RANLIB_FOR_TARGET) $(RTSLIB)/libgnarl$(arext)
	$(CHMOD) a-wx $(RTSLIB)/*.ali
	$(RMDIR) $(RTSOBJ)
	$(MKDIR) $(RTSOBJ)
	$(MV) $(RTSLIB)/*.o $(RTSOBJ)

$(RTSDIR): bin/llvm-gnat1
	$(RMDIR) $(RTSLIB) $(RTSOBJ)
	$(MKDIR) $(RTSLIB)
	$(MAKE) setup-rts
	cp -pr include $(RTSLIB)/..
	cd $(RTSLIB) && $(OSCONS_CPP) && $(OSCONS_EXTRACT) && \
	  $(pwd)/obj-tools/xoscons s-oscons && \
	  $(MV) s-oscons.ads $(RTSDIR) && $(MV) s-oscons.h $(RTSDIR) && \
	  $(RM) s-oscons-tmplt.*
	$(MV) $(RTSDIR)/a-numaux.adb $(RTSDIR)/a-numaux.adb.orig
	sed -e's/%%/%/g' -e's/"=t"/"={st}"/' -e's/"u"/"{st(1)}"/' \
	  $(RTSDIR)/a-numaux.adb.orig > $(RTSDIR)/a-numaux.adb
	$(RM) a-numaux.adb.orig

tracebak.o: tracebak.c tb-gcc.c
	$(CC) -c $(CFLAGS) $(GNATLIBCFLAGS) \
	  $(ALL_CPPFLAGS) $(INCLUDES) $(NO_OMIT_ADAFLAGS) $<

