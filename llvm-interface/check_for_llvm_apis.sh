#! /bin/bash
set -u

# Usage: check_for_llvm_api.sh "path/to/llvm-config"

llvm_config="$1"

cxxflags=$($llvm_config --cxxflags)
ldflags=$($llvm_config  --libs all --ldflags --system-libs)

# The output defines.
haves=

# Test for some specific LLVM API.
function api_test() {
    defname="$1"
    program="$2"
    filename=obj/test_${defname}.cpp

    # Just include whatever headers are required by any test.
    cat <<EOF > $filename
#include "llvm/IR/DIBuilder.h"
$program
EOF

    if gcc $cxxflags $ldflags --syntax-only $filename 2> /dev/null; then
	haves="$haves $defname"
    fi
}

api_test HAVE_SUBRANGE_TYPE "llvm::DISubrangeType *subrange_value = nullptr;"

for def in $haves; do
    echo "#define GNAT_LLVM_$def"
done > obj/gnat-llvm-config.h
