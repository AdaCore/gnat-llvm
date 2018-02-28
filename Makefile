all: build

.PHONY: acats

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

acats:
	make -C acats

clean:
	make -C llvm-interface clean

.PHONY: llvm
