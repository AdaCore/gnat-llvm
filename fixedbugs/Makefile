.PHONEY: save-result run

all: run

save-results:
	if [ -d output/old ]; then rm -rf output/old; fi
	if [ -d output/new ]; then mv output/new output/old; fi

run:
	(cd support; ./testsuite.py -o ../output/new --old-output-dir=../output/old --jobs=12 \
  --tests-dir=../bugs --test-sections=fixed --max-consecutive-failures=30 \
  --target=llvm --discriminants=native,linux,Linux,x86_64-linux,x86_64)
