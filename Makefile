SVN_GNAT = "svn+ssh://svn.us.adacore.com/Dev/trunk/gnat"

DIR_GNAT_SRC = "gnat_src"
DIR_LLVM_ADA = "llvm-ada"
DIR_LLVM_BACKEND = "llvm-backend"

PARALLEL =24

all: build

setup:
	if ! [ -e $(DIR_GNAT_SRC) ]; then \
		svn co $(SVN_ARGS) $(SVN_GNAT) $(DIR_GNAT_SRC); \
	fi

build: setup build-llvm-ada build-be

build-be:
	make -j$(PARALLEL) -C $(DIR_LLVM_BACKEND) bin

build-llvm-ada:
	make -C $(DIR_LLVM_ADA)
	make -s -j$(PARALLEL) -C $(DIR_LLVM_ADA)/llvm-3.3.src/

clean:
	make -C $(DIR_LLVM_BACKEND) clean

distclean: clean
	rm -rf gnat_src
