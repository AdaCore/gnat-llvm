GPRBUILD=gprbuild -v -k

# number of processors
PROCS=0

# Set to 0 to disable debug mode, 1 to enable it
DEBUG=1

# Set to 1 to enable automated (as opposed to manual) builds
AUTOMATED=0

RM=rm -rf
MV=mv -f

adainclude:=$(subst \,/,$(strip $(shell gnatls -v | grep adainclude)))/
adalib:=$(subst \,/,$(strip $(shell gnatls -v | grep adalib)))/
gnat1dir:=$(shell echo $(adainclude) | sed -e 's^rts-.*^^' -e 's^adainclude^^')

ifeq ($(AUTOMATED),1)
  GPRARGS=-XBuild=Production
  prefix=$(shell echo $(adainclude) | sed -e 's!\(.*\)/lib/gcc/\(.*\)!\1!')/
else
  prefix:=@NOPREFIX@
endif

ifeq ($(DEBUG),0)
  GPRARGS=-XBuild=Production
endif

uname:=$(shell uname)
pwd:=$(shell pwd)

LLVM_MODULES = Core Analysis BitWriter Target
LDFLAGS=$(shell $(pwd)/../llvm-ada/llvm-obj/bin/llvm-config --libs \
  $(LLVM_MODULES) --ldflags --system-libs) -lstdc++

ifeq ($(filter-out CYGWIN%,$(uname)),)
  LDFLAGS+=-Wl,--stack=0x2000000
endif

.PHONY: setup force clean

all: setup build

setup:
	mkdir -p obj obj-tools bin
	sed -e "s^@ADAINCLUDE@^$(adainclude)^" \
	    -e "s^@ADALIB@^$(adalib)^" \
	    -e "s^@PREFIX@^$(prefix)^" \
	    -e "s^@GNAT1DIR@^$(gnat1dir)^" \
	    sdefault.adb.in > obj/sdefault.adb
	for f in `cd ../gnat_src; ls xtreeprs.adb xnmake.adb xutil.ad? *-tmpl xsnamest.adb sinfo.ads treeprs.adt nmake.adt`; \
	do \
	  cp -p ../gnat_src/$$f obj-tools; \
	done
	cd obj-tools && gnatmake -q xtreeprs xnmake xsnamest && \
	./xtreeprs && ./xnmake && ./xsnamest && \
	mv nmake.ads nmake.adb treeprs.ads ../obj && \
	mv snames.ns ../obj/snames.ads && mv snames.nb ../obj/snames.adb
	cp -p ../gnat_src/ada_get_targ.adb obj/get_targ.adb

build: setup force
	$(GPRBUILD) $(GPRARGS) -Pgnat_llvm -j$(PROCS) -largs $(LDFLAGS)
	$(GPRBUILD) $(GPRARGS) -Ptools -j$(PROCS)

fast:
        $(MAKE) DEBUG=0

force:

clean:
	$(RM) obj obj-tools

