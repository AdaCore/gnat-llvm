SVN_GNAT = "svn+ssh://svn.us.adacore.com/Dev/trunk/gnat"

DIR_GNAT_SRC = "gnat_src"
DIR_LLVM_BACKEND = "llvm-backend"

PARALLEL =

all: build

setup: setup-gnat
setup-gnat:
	if ! [ -e $(DIR_GNAT_SRC) ]; then \
		svn co $(SVN_ARGS) $(SVN_GNAT) $(DIR_GNAT_SRC); \
	fi

build: build-llvm-backend
build-llvm-backend: setup
	make -j$(PARALLEL) -C $(DIR_LLVM_BACKEND)
