GPRBUILD=gprbuild -v

# number of processors
PROCS=0

# Set to 0 to disable debug mode, 1 to enable it
DEBUG=1

# Set to 1 to enable automated (as opposed to manual) builds
AUTOMATED=0

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

ifeq ($(AUTOMATED),1)
  GPRARGS=-XBuild=Production
endif

ifeq ($(DEBUG),0)
  GPRARGS=-XBuild=Production
endif

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

all: setup build gnatlib

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
	$(LN_S) $(GNAT_SRC)/libgnat/s-rident.ads obj

build: setup force
	$(GPRBUILD) $(GPRARGS) -Pgnat_llvm -j$(PROCS) \
	  -largs $(LDFLAGS) -cargs:c++ $(CXXFLAGS)
	$(GPRBUILD) $(GPRARGS) -Ptools -j$(PROCS)

stage1: build gnatlib
	$(RMDIR) stage1
	$(MKDIR) stage1
	$(MV) obj obj-tools bin lib stage1

stage2: stage1
	PATH=$(pwd)/stage1/bin:$$PATH $(MAKE) GNATMAKE=$(pwd)/stage1/bin/llvm-gnatmake GPRBUILD="gprbuild -v --target=llvm" build gnatlib
	$(RMDIR) stage2
	$(MKDIR) stage2
	$(MV) obj obj-tools bin lib stage2

stage3: stage2
	PATH=$(pwd)/stage2/bin:$$PATH $(MAKE) GNATMAKE=$(pwd)/stage2/bin/llvm-gnatmake GPRBUILD="gprbuild -v --target=llvm" build gnatlib
	# compare stage2 and stage3 objects
	@echo "Comparing stage2 and stage3 objects"
	@for f in `cd stage2/obj && echo *.o`; do \
	  $(compare) stage2/obj/$$f stage3/obj/$$f || exit 1; \
	done
	@echo "Comparison successful."

bootstrap: stage3

fast:
	$(MAKE) DEBUG=0

force:

clean:
	$(RMDIR) obj obj-tools

# Full runtime

target=x86_64-pc-linux-gnu

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
GCC_INCLUDE_PATH = $(word 2,$(shell gcc --print-search-dirs|grep install:))gcc-include
CFLAGS = -O2 -g
GNATLIBCFLAGS = -DIN_RTS=1 -DIN_GCC=1 -fexceptions
ALL_ADAFLAGS = $(CFLAGS) $(ADAFLAGS) $(ADA_INCLUDES)
INCLUDES = -I../adainclude -I$(GCC_INCLUDE_PATH)/include     \
  -I$(GCC_INCLUDE_PATH)/gcc/config -I$(GCC_INCLUDE_PATH)/gcc \
  -I$(GCC_INCLUDE_PATH)/gcc/ada    -I$(GCC_INCLUDE_PATH)     \
  -I$(GCC_INCLUDE_PATH)/libgcc

ADAC=$(pwd)/bin/llvm-gcc
CROSSC=gcc
COMPILE=$(ADAC) -c -gnatpg -nostdinc -I../adainclude

FORCE_DEBUG_ADAFLAGS = -g
NO_INLINE_ADAFLAGS = -fno-inline
NO_OMIT_ADAFLAGS = -fno-omit-frame-pointer
NO_SIBLING_ADAFLAGS=-fno-optimize-sibling-calls
NO_REORDER_ADAFLAGS=-fno-toplevel-reorder

OSCONS_CC=$(CROSSC)
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

gnatlib: $(RTSDIR)
	if [ -d $(RTSOBJ) ]; then \
	  $(MV) $(RTSOBJ)/*.o $(RTSLIB); \
	fi
# Ada files
	$(MAKE) GNAT_SRC=$(GNAT_SRC) -C $(RTSLIB) -f $(pwd)/Makefile \
	  ADAC=$(ADAC) CC=$(CROSSC) $(GNATRTL_OBJS)
# C files
	$(MAKE) GNAT_SRC=$(GNAT_SRC) -C $(RTSLIB) -f $(pwd)/Makefile \
	  CC=$(CROSSC) $(LIBGNAT_OBJS) $(EXTRA_ADALIB_OBJS)
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
	cd $(RTSLIB) && $(OSCONS_CPP) && $(OSCONS_EXTRACT) && \
	  $(pwd)/obj-tools/xoscons s-oscons && \
	  $(MV) s-oscons.ads $(RTSDIR) && $(MV) s-oscons.h $(RTSDIR)
	mv $(RTSDIR)/a-numaux.adb $(RTSDIR)/a-numaux.adb.orig
	sed -e's/%%/%/g' -e's/"=t"/"={st}"/' -e's/"u"/"{st(1)}"/' \
	  $(RTSDIR)/a-numaux.adb.orig > $(RTSDIR)/a-numaux.adb

bin/llvm-gnat1: build

tracebak.o: tracebak.c tb-gcc.c
	$(CC) -c $(CFLAGS) $(GNATLIBCFLAGS) \
	  $(ALL_CPPFLAGS) $(INCLUDES) $(NO_OMIT_ADAFLAGS) $<

