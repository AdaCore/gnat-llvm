.PHONEY: save-result tests

all: tests

save-results:
	if [ -d output/new ]; then mv output/new output/old; fi

tests:
	./run_acats_test.py --target=llvm -t tmp -o output/new --old-output-dir=output/old --acats-version=4 --disable-cleanup --jobs=24
