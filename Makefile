PARALLEL=24

all: build

sanity-check:
	@if ! [ -d gnat_src ]; then \
          echo "error: directory gnat_src not found"; exit 1; \
	fi

build: sanity-check build-be

build-be:
	make -j$(PARALLEL) -C llvm-backend bin

llvm:
	make -C llvm-ada PARALLEL=$(PARALLEL)

clean:
	make -C llvm-backend clean

