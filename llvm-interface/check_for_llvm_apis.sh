#! /bin/bash
set -u

# Usage: check_for_llvm_api.sh "path/to/llvm-config"

llvm_config="$1"

cxxflags=$($llvm_config --cxxflags)
ldflags=$($llvm_config  --libs all --ldflags --system-libs)

# The output defines.
rm -f obj/def_*.h

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
	echo "#define GNAT_LLVM_$defname" >obj/def_${defname}.h
    fi

    rm $filename
}

# Test that invokes a DIBuilder method.
call_test() {
    defname="$1"
    program="$2"
    filename=obj/test_${defname}.cpp
    exe=obj/test_${defname}

    # Just include whatever headers are required by any test.
    cat <<EOF > $filename
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfo.h"
using namespace llvm;
int main ()
{
  LLVMContext context;
  Module mod ("test", context);
  DIBuilder builder (mod);
  $program
  return 0;
}
EOF

    if g++ $cxxflags $ldflags -o $exe $filename; then
	export LD_LIBRARY_PATH=$($llvm_config --libdir):$LD_LIBRARY_PATH
	if $exe ; then
	    echo "#define GNAT_LLVM_$defname" >obj/def_${defname}.h
	fi
	rm $exe
   fi

    rm $filename
}

# Test a ".ll" file to see if llvm-as parses it.
ll_test() {
    defname="$1"
    program="$2"
    filename=obj/test_${defname}.ll
    llvmas=$($llvm_config --bindir)/llvm-as

    echo "$program" > $filename
    if $llvmas < $filename > /dev/null 2>&1; then
	echo "#define GNAT_LLVM_$defname" >obj/def_${defname}.h
    fi
}

api_test HAVE_SUBRANGE_TYPE "DISubrangeType *subrange_value = nullptr;" &
# This checks for both the "name" patch and the "bit stride" patch.
api_test HAVE_ARRAY_NAME "MDNode *named(DIBuilder *builder) { return builder->createArrayType(nullptr, StringRef(), nullptr, 0, 32, 0, nullptr, {}, nullptr, nullptr, nullptr, nullptr, nullptr); }" &
api_test HAVE_FIXED_POINT "DIFixedPointType *fp_type = nullptr;" &

# This method's signature changed in the patch to allow types to have
# function scope.
api_test HAVE_TYPE_FN_SCOPE "void call(DebugInfoFinder *f, DILocalVariable *v) { f->processVariable(v); }" &

# Test that checks if sizes and offsets can be dynamic.
api_test HAVE_DYNAMIC_OFFSETS "void call(DIBuilder *b) { b->createMemberType(nullptr, StringRef(), nullptr, 0, nullptr, 0, nullptr, DINode::FlagZero, nullptr); }" &

# Test whether multiple members can be included in a variant.
api_test HAVE_MULTI_MEMBER_VARIANT "void call(DIBuilder *b) { b->createVariantMemberType(nullptr, DINodeArray(), (Constant*)nullptr, (DIType*)nullptr); }" &

# Test whether DISubrangeType can hold a DIDerivedType.  This was
# added after the initial DISubrangeType patch.
api_test HAVE_SUBRANGE_TYPE_EXTENSION "DIDerivedType *call(DISubrangeType::BoundType bound) { return bound.dyn_cast<DIDerivedType *>(); }" &

# Test whether DIExpression can handle DW_OP_rot, DW_OP_neg, and
# DW_OP_ops.  A single test is sufficient because these all landed in
# the same patch.
ll_test HAVE_DW_EXPRESSION_EXTENSIONS '!named = !{!DIExpression(DW_OP_push_object_address, DW_OP_lit0, DW_OP_lit0, DW_OP_neg, DW_OP_abs, DW_OP_rot, DW_OP_rot, DW_OP_rot, DW_OP_plus, DW_OP_plus)}' &

# Check that createBasicType will accept an empty name.  Note that
# this can't be checked via a '.ll' file, as LLVM accepted nameless
# types made that way.
call_test HAVE_NAMELESS_BASIC_TYPE 'builder.createBasicType("", 32, dwarf::DW_ATE_unsigned, DINode::FlagZero);'

wait

for def in obj/def_*.h; do
    cat $def
done > obj/tmp-gnat-llvm-config.h
./move-if-change obj/tmp-gnat-llvm-config.h obj/gnat-llvm-config.h

rm -f obj/def_*.h
