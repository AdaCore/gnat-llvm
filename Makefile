PARALLEL=24

all: build

sanity-check:
	@if ! [ -d llvm-backend/gnat_src ]; then \
          echo "error: directory llvm-backend/gnat_src not found"; exit 1; \
	fi

build: sanity-check build-be

build-be:
	make -C llvm-backend

llvm:
	make -C llvm-ada PARALLEL=$(PARALLEL)

clean:
	make -C llvm-backend clean

