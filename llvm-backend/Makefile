# Makefile for GNATLLVM
# Copyright (C) 2006, AdaCore

TARGET=llvm
TARGETLANG=LLVM
LIBDIR=gnat-llvm
NON_ADA_RT_FILES=*.c
objext=.bc

TOOLS_PREFIX=llvm-
GNATCOMPILE=llvm-gcc
GNATCMD=gnat
EXTRA_TOOLS=

-include ../stubs/Makefile.common

sanity-check:
ifeq (,$(findstring $(TARGET),$(target)))
	@echo "You must run make in the src/$(TARGET) directory"
	@exit 1
endif
	@if [ ! -f $(GNAT_SRC)/Makefile.rtl ]; then \
	  echo "$(GNAT_SRC)/Makefile.rtl not found"; exit 1; \
	fi
