#! /bin/bash
set -u

# Usage: check_for_llvm_api.sh "path/to/llvm-config"

llvm_config="$1"

cxxflags=$($llvm_config --cxxflags)
ldflags=$($llvm_config  --libs all --ldflags --system-libs)

# The output defines.
haves=

# Test for some specific LLVM API.
api_test() {
    defname="$1"
    program="$2"
    filename=obj/test_${defname}.cpp

    # Just include whatever headers are required by any test.
    cat <<EOF > $filename
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfo.h"
using namespace llvm;
$program
EOF

    if gcc $cxxflags $ldflags --syntax-only $filename 2> /dev/null; then
	haves="$haves $defname"
    fi
}

api_test HAVE_SUBRANGE_TYPE "DISubrangeType *subrange_value = nullptr;"
# This checks for both the "name" patch and the "bit stride" patch.
api_test HAVE_ARRAY_NAME "MDNode *named(DIBuilder *builder) { return builder->createArrayType(nullptr, StringRef(), nullptr, 0, 32, 0, nullptr, {}, nullptr, nullptr, nullptr, nullptr, nullptr); }"
api_test HAVE_FIXED_POINT "DIFixedPointType *fp_type = nullptr;"

# This method's signature changed in the patch to allow types to have
# function scope.
api_test HAVE_TYPE_FN_SCOPE "void call(DebugInfoFinder *f, DILocalVariable *v) { f->processVariable(v); }"

# Test that checks if sizes and offsets can be dynamic.
api_test HAVE_DYNAMIC_OFFSETS "void call(DIBuilder *b) { b->createMemberType(nullptr, StringRef(), nullptr, 0, nullptr, 0, nullptr, DINode::FlagZero, nullptr); }"

for def in $haves; do
    echo "#define GNAT_LLVM_$def"
done > obj/tmp-gnat-llvm-config.h
./move-if-change obj/tmp-gnat-llvm-config.h obj/gnat-llvm-config.h
