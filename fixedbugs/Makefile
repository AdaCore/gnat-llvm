.PHONEY: save-result run

all: run

save-results:
	if [ -d output/old ]; then rm -rf output/old; fi
	if [ -d output/new ]; then mv output/new output/old; fi

run:
	(cd support; PATH=$(PWD)/../llvm-interface/bugs/bin:$$PATH ./testsuite.py -o ../output/new --old-output-dir=../output/old --jobs=12 \
  --tests-dir=../bugs --test-sections=fixed --max-consecutive-failures=30 \
  --discriminants=llvm)
