.PHONEY: save-result save-ccg-results tests ccg

O=O0

all: ccg tests

save-results:
	if [ -d output/old ]; then rm -rf output/old; fi
	if [ -d output/new ]; then mv output/new output/old; fi

save-ccg-results:
	if [ -d output-ccg/old ]; then rm -rf output-ccg/old; fi
	if [ -d output-ccg/new ]; then mv output-ccg/new output-ccg/old; fi

tests:
	./run_acats_test.py --target=llvm -t tmp -o output/new --old-output-dir=output/old --acats-version=4 --disable-cleanup --jobs=48

ccg:
	CCG=1 OPTIMIZE=$(O) ./run_acats_test.py --target=ccg -t tmp-ccg -o output-ccg/new --old-output-dir=output-ccg/old --acats-version=4 --disable-cleanup --jobs=48
