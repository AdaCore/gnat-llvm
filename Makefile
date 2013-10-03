SVN_GNAT = "svn+ssh://svn.us.adacore.com/Dev/trunk/gnat"
GIT_LLVM_ADA = "chelles.eu.adacore.com:~derodat/share/llvm-ada.git"

DIR_GNAT_SRC = "gnat_src"
DIR_LLVM_ADA = "llvm-ada"
DIR_LLVM_BACKEND = "llvm-backend"

PARALLEL =8

all: build

setup: setup-gnat setup-llvm-ada
setup-gnat:
	if ! [ -e $(DIR_GNAT_SRC) ]; then \
		svn co $(SVN_ARGS) $(SVN_GNAT) $(DIR_GNAT_SRC); \
	fi
setup-llvm-ada:
	if ! [ -e $(DIR_LLVM_ADA) ]; then \
		git clone $(GIT_ARGS) $(GIT_LLVM_ADA) $(DIR_LLVM_ADA); \
	fi

build: build-llvm-backend
build-llvm-backend: build-llvm-ada setup
	make -j$(PARALLEL) -C $(DIR_LLVM_BACKEND)
build-llvm-ada: setup-llvm-ada
	make -C $(DIR_LLVM_ADA)
	make -j$(PARALLEL) -C $(DIR_LLVM_ADA)/llvm-3.3.src/

clean:
	make -C $(DIR_LLVM_BACKEND) clean

distclean: clean
	rm -rf gnat_src llvm-ada
