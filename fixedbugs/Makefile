run:
	(cd support; PATH=$(PWD)/../llvm-interface/bugs/bin:$$PATH ./testsuite.py --output-dir=../output --keep-old-output-dir --jobs=48 \
  --tests-dir=../bugs --test-sections=fixed --max-consecutive-failures=30 \
  --discriminants=llvm)
