all: sanity-check
	$(MAKE) -C llvm-interface build gnatlib-automated

.PHONY: acats ccg-acats fixed-bugs ccg-tests llvm clean distclean

sanity-check:
	@if ! [ -d llvm-interface/gnat_src ]; then \
          echo "error: directory llvm-interface/gnat_src not found"; exit 1; \
	fi

build build-opt clean gnatlib bootstrap automated zfp: sanity-check
	$(MAKE) -C llvm-interface $@

gnatlib%: sanity-check
	$(MAKE) -C llvm-interface $@

# Entry points for cross builds. Currently, it builds a cross compiler that
# isn't bootstrapped (i.e., we build it directly with native GNAT). The
# runtimes need to be built separately.
cross-automated:
	$(MAKE) -C llvm-interface build-opt

# Build the full runtime instrumented with SymCC. This target requires SymCC and
# a working GNAT-LLVM on the path, i.e., it builds only the runtime.
symcc-automated:
	$(MAKE) -C llvm-interface \
	  RTS="$(shell pwd)/llvm-interface/lib/rts-symcc" \
	  gnatlib-symcc-automated

llvm:
	$(MAKE) -j1 -C llvm setup
	$(MAKE) -C llvm llvm

acats:
	$(MAKE) -C acats tests

ccg-acats:
	$(MAKE) -C acats ccg

fixed-bugs:
	$(MAKE) -C fixedbugs

ccg-tests:
	$(MAKE) -C ccg-tests/tests

distclean: clean
	$(MAKE) -C llvm clean

