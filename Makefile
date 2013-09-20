SVN_GNAT = "svn+ssh://svn.us.adacore.com/Dev/trunk/gnat"
GIT_LLVM_ADA = "chelles.eu.adacore.com:~derodat/share/llvm-ada.git"

DIR_GNAT_SRC = "gnat_src"
DIR_LLVM_ADA = "llvm-ada"
DIR_LLVM_BACKEND = "llvm-backend"

PARALLEL =

all: build

setup: setup-gnat setup-llvm-ada
setup-gnat:
	if ! [ -e $(DIR_GNAT_SRC) ]; then \
		svn co $(SVN_ARGS) $(SVN_GNAT) $(DIR_GNAT_SRC); \
	fi
setup-llvm-ada:
	if ! [ -e $(DIR_LLVM_ADA) ]; then \
		git checkout $(GIT_ARGS) $(GIT_LLVM_ADA) $(DIR_LLVM_ADA); \
	fi

build: build-llvm-backend
build-llvm-backend: setup
	make -j$(PARALLEL) -C $(DIR_LLVM_BACKEND)
build-llvm-ada: setup-llvm-ada
	make -C $(DIR_LLVM_ADA)
