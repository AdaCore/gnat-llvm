JOBS=24

.PHONY: tests

all:	tests

tests:
	mkdir -p output/new
	rm -rf output/old
	mv output/new output/old
	./testsuite.py --jobs=$(JOBS) --output-dir=output/new \
	  --old-output-dir=output/old
