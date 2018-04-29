GPRBUILD=gprbuild -v -k

# number of processors
PROCS=0

# Set to 0 to disable debug mode, 1 to enable it
DEBUG=1

# Set to 1 to enable automated (as opposed to manual) builds
AUTOMATED=0

RM=rm -f
RMDIR=rm -rf
MV=mv -f
MKDIR=mkdir -p
CHMOD=chmod
AR=ar q

ifeq ($(AUTOMATED),1)
  GPRARGS=-XBuild=Production
endif

ifeq ($(DEBUG),0)
  GPRARGS=-XBuild=Production
endif

GNAT_SRC=$(pwd)/gnat_src
ADAINCLUDE=lib/rts-llvm/adainclude
ADALIB=lib/rts-llvm/adalib
FULL_ADAINCLUDE=lib/rts-full/adainclude
FULL_ADALIB=lib/rts-full/adalib
GNAT_ROOT:=$(dir $(shell which gnatls))..
NATIVE_ADAINCLUDE:=$(strip $(shell gnatls -v | grep adainclude))

uname:=$(shell uname)
pwd:=$(shell pwd)

# LLVM_MODULES = Core Analysis BitWriter Target
# LDFLAGS=$(shell $(pwd)/../llvm/llvm-obj/bin/llvm-config --libs \
#   $(LLVM_MODULES) --ldflags --system-libs) -lstdc++
LDFLAGS=$(shell $(pwd)/../llvm/llvm-obj/bin/llvm-config --libs all \
  --ldflags --system-libs) -static-libstdc++
CXXFLAGS=$(shell $(pwd)/../llvm/llvm-obj/bin/llvm-config --cxxflags)

ifeq ($(filter-out CYGWIN%,$(uname)),)
  LDFLAGS+=-Wl,--stack=0x2000000
endif

.PHONY: setup force clean

all: setup build zfp

setup:
	mkdir -p obj obj-tools bin
	for f in `cd $(GNAT_SRC); ls xtreeprs.adb xnmake.adb xutil.ad? *-tmpl xsnamest.adb sinfo.ads treeprs.adt nmake.adt`; \
	do \
	  cp -p $(GNAT_SRC)/$$f obj-tools; \
	done
	cd obj-tools && gnatmake -q xtreeprs xnmake xsnamest && \
	./xtreeprs && ./xnmake && ./xsnamest && \
	mv nmake.ads nmake.adb treeprs.ads ../obj && \
	mv snames.ns ../obj/snames.ads && mv snames.nb ../obj/snames.adb
	cp -p $(GNAT_SRC)/ada_get_targ.adb obj/get_targ.adb
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
s-atacco.adb \
s-atacco.ads \
s-exnint.ads \
s-exnint.adb \
s-expint.ads \
s-expint.adb \
s-fatflt.ads \
s-fatgen.adb \
s-fatgen.ads \
s-fatlfl.ads \
s-fatllf.ads \
s-fatsfl.ads \
s-imgboo.adb \
s-imgboo.ads \
s-imenne.ads \
s-imenne.adb \
s-imgint.adb \
s-imgint.ads \
s-imglli.adb \
s-imglli.ads \
s-imgllu.adb \
s-imgllu.ads \
s-imguns.adb \
s-imguns.ads \
s-maccod.ads \
s-stoele.adb \
s-stoele.ads \
s-unstyp.ads \
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
s-fatflt.ads \
s-fatsfl.ads \
s-fatllf.ads \
s-fatlfl.ads \
s-maccod.ads \
s-unstyp.ads \
system.ads   \
text_io.ads  \
unchconv.ads \
unchdeal.ads

LLVM_GCC=$(pwd)/bin/llvm-gcc
COMPILE=$(LLVM_GCC) -c -gnatpg -nostdinc -I../adainclude

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
	cp -p $(GNAT_SRC)/hie/system-c.ads $(ADAINCLUDE)/system.ads
	cd $(ADALIB) && for f in $(COMPILABLE_SPECS); do \
	  $(COMPILE) ../adainclude/$$f || exit 1; \
	done
	cd $(ADALIB) && for f in ../adainclude/*.adb; do \
	  $(COMPILE) $$f || exit 1; \
	done
	cd $(ADALIB) && $(CHMOD) a-wx *.ali && $(AR) libgnat.a *.o && $(RM) *.o

rts-full: build
	$(RMDIR) $(FULL_ADAINCLUDE) $(FULL_ADALIB)
	$(MKDIR) $(FULL_ADAINCLUDE) $(FULL_ADALIB)
	$(MAKE) -f $(GNAT_SRC)/Makefile.adalib -C $(FULL_ADALIB) \
	  ROOT=$(GNAT_ROOT) ADAC=$(LLVM_GCC) \
	  ADAFLAGS="-gnatpg -nostdinc -I$(NATIVE_ADAINCLUDE)"
	cp -p $(NATIVE_ADAINCLUDE)/*.ad? $(FULL_ADAINCLUDE)

