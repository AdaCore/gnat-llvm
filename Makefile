all: sanity-check
	$(MAKE) -C llvm-interface build gnatlib-automated

.PHONY: acats llvm clean distclean

sanity-check:
	@if ! [ -d llvm-interface/gnat_src ]; then \
          echo "error: directory llvm-interface/gnat_src not found"; exit 1; \
	fi

build build-opt clean gnatlib: sanity-check
	$(MAKE) -C llvm-interface $@

gnatlib%: sanity-check
	$(MAKE) -C llvm-interface $@

automated:
	$(MAKE) -C llvm-interface bootstrap

llvm:
	$(MAKE) -j1 -C llvm setup
	$(MAKE) -C llvm llvm

acats:
	$(MAKE) -C acats

fixed-bugs:
	$(MAKE) -C fixedbugs

distclean: clean
	$(MAKE) -C llvm clean

