#!/bin/sh

mkdir -p output/new
rm -rf output/old
mv output/new output/old
./testsuite.py --jobs=24 --output-dir=output/new --old-output-dir=output/old
