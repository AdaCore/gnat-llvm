all: sanity-check
	$(MAKE) -C llvm-interface build gnatlib-automated

.PHONY: acats llvm clean distclean

sanity-check:
	@if ! [ -d llvm-interface/gnat_src ]; then \
          echo "error: directory llvm-interface/gnat_src not found"; exit 1; \
	fi

build: sanity-check
	$(MAKE) -C llvm-interface build

build-opt: sanity-check
	$(MAKE) -C llvm-interface build-opt

gnatlib: sanity-check
	$(MAKE) -C llvm-interface gnatlib

automated:
	$(MAKE) -C llvm-interface build
	$(MAKE) -C llvm-interface gnatlib-automated

llvm:
	$(MAKE) -j1 -C llvm setup
	$(MAKE) -C llvm llvm

acats:
	$(MAKE) -C acats

fixed-bugs:
	$(MAKE) -C fixedbugs

clean:
	$(MAKE) -C llvm-interface clean

distclean: clean
	$(MAKE) -C llvm clean

