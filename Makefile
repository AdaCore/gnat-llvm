all: build

sanity-check:
	@if ! [ -d llvm-interface/gnat_src ]; then \
          echo "error: directory llvm-interface/gnat_src not found"; exit 1; \
	fi

build: sanity-check build-be

build-be:
	make -C llvm-interface

llvm:
	make -C llvm

tests:
	make -C testsuite

clean:
	make -C llvm-interface clean

.PHONY: llvm
