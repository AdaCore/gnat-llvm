GPRBUILD=gprbuild -v -k

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
LN_S=ln -s

ifeq ($(filter-out CYGWIN%,$(uname)),)
  LN_S=cp -p
endif

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

.PHONY: setup force clean

all: setup build zfp

setup:
	mkdir -p obj obj-tools bin
	for f in `cd $(GNAT_SRC); ls xtreeprs.adb xnmake.adb xutil.ad? *-tmpl xsnamest.adb sinfo.ads treeprs.adt nmake.adt xoscons.adb xutil.ads xutil.adb`; \
	do \
	  cp -p $(GNAT_SRC)/$$f obj-tools; \
	done
	cd obj-tools && gnatmake -q -j0 xtreeprs xnmake xsnamest xoscons && \
	./xtreeprs && ./xnmake && ./xsnamest && \
	mv nmake.ads nmake.adb treeprs.ads ../obj && \
	mv snames.ns ../obj/snames.ads && mv snames.nb ../obj/snames.adb
	cp -p $(GNAT_SRC)/libgnat/s-rident.ads obj

build: setup force
	$(GPRBUILD) $(GPRARGS) -Pgnat_llvm -j$(PROCS) \
	  -largs $(LDFLAGS) -cargs:c++ $(CXXFLAGS)
	$(GPRBUILD) $(GPRARGS) -Ptools -j$(PROCS)

fast:
	$(MAKE) DEBUG=0

force:

clean:
	$(RMDIR) obj obj-tools

COMMON_SRCS= \
a-assert.adb \
a-assert.ads \
ada.ads      \
a-unccon.ads \
a-uncdea.ads \
gnat.ads     \
g-souinf.ads \
i-cexten.ads \
interfac.ads \
s-addope.ads \
s-addope.adb \
s-atacco.adb \
s-atacco.ads \
s-arit64.ads \
s-arit64.adb \
s-boarop.ads \
s-casuti.ads \
s-casuti.adb \
s-exnint.ads \
s-exnint.adb \
s-exnllf.ads \
s-exnllf.adb \
s-expint.ads \
s-expint.adb \
s-explli.ads \
s-explli.adb \
s-expuns.ads \
s-expuns.adb \
s-fatflt.ads \
s-fatgen.adb \
s-fatgen.ads \
s-fatlfl.ads \
s-fatllf.ads \
s-fatsfl.ads \
s-flocon.ads \
s-fore.ads   \
s-fore.adb   \
s-geveop.ads \
s-geveop.adb \
s-imenne.adb \
s-imenne.ads \
s-imgboo.adb \
s-imgboo.ads \
s-imgcha.adb \
s-imgcha.ads \
s-imgenu.adb \
s-imgenu.ads \
s-imgint.adb \
s-imgint.ads \
s-imglli.adb \
s-imglli.ads \
s-imgllu.adb \
s-imgllu.ads \
s-imguns.adb \
s-imguns.ads \
s-imgwch.adb \
s-imgwch.ads \
s-maccod.ads \
s-powtab.ads \
s-stoele.adb \
s-stoele.ads \
s-unstyp.ads \
s-valboo.adb \
s-valboo.ads \
s-valcha.adb \
s-valcha.ads \
s-valenu.adb \
s-valenu.ads \
s-valint.adb \
s-valint.ads \
s-vallli.adb \
s-vallli.ads \
s-valllu.adb \
s-valllu.ads \
s-valuns.adb \
s-valuns.ads \
s-valuti.adb \
s-valuti.ads \
s-veboop.ads \
s-veboop.adb \
s-vector.ads \
s-wchcnv.adb \
s-wchcnv.ads \
s-wchcon.adb \
s-wchcon.ads \
s-wchjis.ads \
s-wchjis.adb \
s-widboo.ads \
s-widboo.adb \
s-widenu.ads \
s-widenu.adb \
s-widlli.ads \
s-widlli.adb \
machcode.ads \
text_io.ads  \
unchconv.ads \
unchdeal.ads

COMPILABLE_SPECS= \
ada.ads      \
a-unccon.ads \
a-uncdea.ads \
gnat.ads     \
g-souinf.ads \
i-c.ads      \
i-cexten.ads \
interfac.ads \
s-atacco.ads \
s-boarop.ads \
s-fatflt.ads \
s-fatsfl.ads \
s-fatllf.ads \
s-fatlfl.ads \
s-maccod.ads \
s-powtab.ads \
s-unstyp.ads \
s-parame.ads \
s-stalib.ads \
s-vector.ads \
system.ads   \
machcode.ads \
text_io.ads  \
unchconv.ads \
unchdeal.ads

ADAC=$(pwd)/bin/llvm-gcc
CROSSC=gcc
COMPILE=$(ADAC) -c -gnatpg -nostdinc -I../adainclude

# ZFP runtime

zfp: build
	$(RMDIR) $(ADAINCLUDE) $(ADALIB)
	$(MKDIR) $(ADAINCLUDE) $(ADALIB)
	for f in $(COMMON_SRCS); do \
	  cp -p $(GNAT_SRC)/libgnat/$$f $(ADAINCLUDE); \
	done
	cp -p $(GNAT_SRC)/hie/a-tags__c.ads $(ADAINCLUDE)/a-tags.ads
	cp -p $(GNAT_SRC)/hie/a-tags__c.adb $(ADAINCLUDE)/a-tags.adb
	cp -p $(GNAT_SRC)/hie/a-except__c.adb $(ADAINCLUDE)/a-except.adb
	cp -p $(GNAT_SRC)/hie/a-except__zfp.ads $(ADAINCLUDE)/a-except.ads
	cp -p $(GNAT_SRC)/hie/a-textio__c.adb $(ADAINCLUDE)/a-textio.adb
	cp -p $(GNAT_SRC)/hie/a-textio__c.ads $(ADAINCLUDE)/a-textio.ads
	cp -p $(GNAT_SRC)/hie/i-c__hie.ads $(ADAINCLUDE)/i-c.ads
	cp -p $(GNAT_SRC)/hie/s-assert__c.ads $(ADAINCLUDE)/s-assert.ads
	cp -p $(GNAT_SRC)/hie/s-assert__xi.adb $(ADAINCLUDE)/s-assert.adb
	cp -p $(GNAT_SRC)/hie/s-secsta__zfp.ads $(ADAINCLUDE)/s-secsta.ads
	cp -p $(GNAT_SRC)/hie/s-secsta__zfp.adb $(ADAINCLUDE)/s-secsta.adb
	cp -p $(GNAT_SRC)/hie/s-parame__zfp.ads $(ADAINCLUDE)/s-parame.ads
	cp -p $(GNAT_SRC)/hie/system-llvm.ads $(ADAINCLUDE)/system.ads
	cp -p $(GNAT_SRC)/hie/s-sssita.ad[sb] $(ADAINCLUDE)/
	cp -p $(GNAT_SRC)/hie/s-stalib__zfp.ads $(ADAINCLUDE)/s-stalib.ads
	cp -p $(GNAT_SRC)/libgnat/s-flocon__none.adb $(ADAINCLUDE)/s-flocon.adb
	cd $(ADALIB) && for f in $(COMPILABLE_SPECS); do \
	  $(COMPILE) ../adainclude/$$f || exit 1; \
	done
	cd $(ADALIB) && for f in ../adainclude/*.adb; do \
	  $(COMPILE) $$f || exit 1; \
	done
	cd $(ADALIB) && $(CHMOD) a-wx *.ali && $(AR) libgnat.a *.o && $(RM) *.o

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
ADAFLAGS=-gnatpg
ADA_INCLUDES=-nostdinc -I../adainclude
GCC_INCLUDE_PATH = $(word 2,$(shell gcc --print-search-dirs|grep install:))gcc-include
CFLAGS = -O2
GNATLIBCFLAGS = -DIN_RTS=1 -DIN_GCC=1 -fexceptions
ALL_ADAFLAGS = $(CFLAGS) $(ADAFLAGS) $(ADA_INCLUDES)
INCLUDES = -I../adainclude -I$(GCC_INCLUDE_PATH)/include     \
  -I$(GCC_INCLUDE_PATH)/gcc/config -I$(GCC_INCLUDE_PATH)/gcc \
  -I$(GCC_INCLUDE_PATH)/gcc/ada    -I$(GCC_INCLUDE_PATH)     \
  -I$(GCC_INCLUDE_PATH)/libgcc

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

gnatlib: build
	$(RMDIR) $(RTSLIB)
	$(MKDIR) $(RTSLIB)
	$(MAKE) setup-rts
	cd $(RTSLIB) && $(OSCONS_CPP) && $(OSCONS_EXTRACT) && \
	  $(pwd)/obj-tools/xoscons s-oscons && \
	  $(MV) s-oscons.ads $(RTSDIR) && $(MV) s-oscons.h $(RTSDIR)
# Ada files
	mv $(RTSDIR)/a-numaux.adb $(RTSDIR)/a-numaux.adb.orig
	sed -e's/%%/%/g' -e's/"=t"/"={st}"/' -e's/"u"/"{st(1)}"/' \
	  $(RTSDIR)/a-numaux.adb.orig > $(RTSDIR)/a-numaux.adb
	$(MAKE) GNAT_SRC=$(GNAT_SRC) -C $(RTSLIB) -f $(pwd)/Makefile \
	  ADAC=$(ADAC) CC=$(CROSSC) $(GNATRTL_OBJS)
# C files
	$(MAKE) GNAT_SRC=$(GNAT_SRC) -C $(RTSLIB) -f $(pwd)/Makefile \
	  CC=$(CROSSC) $(LIBGNAT_OBJS) $(EXTRA_ADALIB_OBJS)
# Create libgnat/libgnarl.a
	$(RM) $(RTSLIB)/libgnat$(arext) $(RTSLIB)/libgnarl$(arext)
	$(AR_FOR_TARGET) $(AR_FLAGS) $(RTSLIB)/libgnat$(arext) \
	  $(addprefix $(RTSLIB)/,$(GNATRTL_NONTASKING_OBJS) $(LIBGNAT_OBJS))
	$(RANLIB_FOR_TARGET) $(RTSLIB)/libgnat$(arext)
	$(AR_FOR_TARGET) $(AR_FLAGS) $(RTSLIB)/libgnarl$(arext) \
	  $(addprefix $(RTSLIB)/,$(GNATRTL_TASKING_OBJS))
	$(RANLIB_FOR_TARGET) $(RTSLIB)/libgnarl$(arext)
	$(CHMOD) a-wx $(RTSLIB)/*.ali
	$(RM) $(RTSLIB)/*.o

tracebak.o: tracebak.c tb-gcc.c
	$(CC) -c $(CFLAGS) $(GNATLIBCFLAGS) \
	  $(ALL_CPPFLAGS) $(INCLUDES) $(NO_OMIT_ADAFLAGS) $<

