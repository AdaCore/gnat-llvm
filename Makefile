all: build

.PHONY: acats

sanity-check:
	@if ! [ -d llvm-interface/gnat_src ]; then \
          echo "error: directory llvm-interface/gnat_src not found"; exit 1; \
	fi

build: sanity-check build-be

build-opt: sanity-check build-be-opt

build-be:
	$(MAKE) -C llvm-interface build

build-be-opt:
	$(MAKE) -C llvm-interface build-opt

automated:
	$(MAKE) -C llvm-interface build
	$(MAKE) -C llvm-interface gnatlib-automated

llvm:
	$(MAKE) -C llvm

acats:
	$(MAKE) -C acats

fixed-bugs:
	$(MAKE) -C fixedbugs

clean:
	$(MAKE) -C llvm-interface clean

.PHONY: llvm
