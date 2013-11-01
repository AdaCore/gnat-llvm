# Makefile for GNATLLVM
# Copyright (C) 2006, AdaCore

TARGET=llvm
TARGETLANG=LLVM
DEF_TARGET=No_VM
LIBDIR=gnat2llvm
NON_ADA_RT_FILES=*.c
objext=.o

TOOLS_PREFIX=llvm-
GNATCOMPILE=llvm-gnatcompile
SHORT_GNATCOMPILE=gnatcompile
GNATCMD=gnat
EXTRA_TOOLS=

GNATRTL_COMMON_SRCS=\
   s-oscons.ads

GNATRTL_WHITELIST = \
	system.o

-include ../stubs/Makefile.common

sanity-check:
ifeq (,$(findstring $(TARGET),$(target)))
	@echo "You must run make in the src/$(TARGET) directory"
	@exit 1
endif
	@if [ ! -f $(GNAT_SRC)/Makefile.rtl ]; then \
	  echo "$(GNAT_SRC)/Makefile.rtl not found"; exit 1; \
	fi
libgnat: $(GNATRTL_OBJS)
