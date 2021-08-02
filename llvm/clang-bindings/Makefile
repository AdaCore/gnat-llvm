# Makefile targets
# ----------------
#
# Setup:                   make [VAR=VALUE] setup (see below)
# Build:                   make
# Install:                 make install

# Variables which can be set:
#
# General:
#
#   prefix        : root install directory
#   ENABLE_SHARED : yes / no (or empty)
#   PROCESSORS    : nb parallel compilations (0 to use all cores)
#   TARGET        : target triplet for cross-compilation
#

# helper programs
ECHO  := echo
WHICH := which

# check for out-of-tree build
SOURCE_DIR := $(dir $(MAKEFILE_LIST))
ifeq ($(SOURCE_DIR),./)
  RBD=
  LIBCLANG_GPR=libclang.gpr
  MAKEPREFIX=
else
  RBD=--relocate-build-tree
  LIBCLANG_GPR=$(SOURCE_DIR)/libclang.gpr
  MAKEPREFIX=$(SOURCE_DIR)/
endif

TARGET := $(shell gcc -dumpmachine)
NORMALIZED_TARGET := $(subst normalized_target:,,$(wordlist 6,6,$(shell gprconfig  --config=ada --target=$(TARGET) --mi-show-compilers)))
ifeq ($(NORMALIZED_TARGET),)
  $(error No toolchain found for target "$(TARGET)")
endif

prefix := $(dir $(shell $(WHICH) gnatls))..

PROCESSORS    = 0
BUILD_DIR     =
ENABLE_SHARED = yes

all: build

# Load current setup if any
-include makefile.setup

GTARGET=--target=$(NORMALIZED_TARGET)

ifeq ($(ENABLE_SHARED), yes)
   LIBRARY_TYPES=static relocatable static-pic
else
   LIBRARY_TYPES=static
endif


# Used to pass extra options to GPRBUILD, like -d for instance
GPRBUILD_OPTIONS=

BUILDER=gprbuild -p -m $(GTARGET) $(RBD) -j$(PROCESSORS) $(GPRBUILD_OPTIONS)
INSTALLER=gprinstall -p -f $(GTARGET) $(RBD) \
	--sources-subdir=include/libclang --prefix=$(prefix)$(integrated_install)
CLEANER=gprclean -q $(RBD) $(GTARGET)
UNINSTALLER=$(INSTALLER) -p -f --install-name=libclang --uninstall

#########
# build #
#########

build: $(LIBRARY_TYPES:%=build-%)

build-%:
	$(BUILDER) -XLIBRARY_TYPE=$* $(LIBCLANG_GPR)

###########
# Install #
###########

uninstall:
ifneq (,$(wildcard $(prefix)$(integrated_install)/share/gpr/manifests/libclang))
	$(UNINSTALLER) $(LIBCLANG_GPR)
endif

install: uninstall $(LIBRARY_TYPES:%=install-%)

install-%:
	$(INSTALLER) -XLIBRARY_TYPE=$* --build-name=$* \
		--build-var=LIBRARY_TYPE --build-var=LIBCLANG_BUILD \
		$(LIBCLANG_GPR)

###########
# Cleanup #
###########

clean: $(LIBRARY_TYPES:%=clean-%)

clean-%:
	-$(CLEANER) -XLIBRARY_TYPE=$*	$(LIBCLANG_GPR)

#########
# setup #
#########

.SILENT: setup

setup:
	$(ECHO) "prefix=$(prefix)" > makefile.setup
	$(ECHO) "ENABLE_SHARED=$(ENABLE_SHARED)" >> makefile.setup
	$(ECHO) "PROCESSORS=$(PROCESSORS)" >> makefile.setup
	$(ECHO) "TARGET=$(TARGET)" >> makefile.setup
	$(ECHO) "SOURCE_DIR=$(SOURCE_DIR)" >> makefile.setup

# Let gprbuild handle parallelisation. In general, we don't support parallel
# runs in this Makefile, as concurrent gprinstall processes may crash.
.NOTPARALLEL:
