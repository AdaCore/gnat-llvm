from ctypes import *
from gnatllvm import build_and_load, Func

(Foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('_ada_foo', argtypes=[c_uint64], restype=c_uint32),
)

for a in (
    0x0000000000000000,
    0x0000000000000001,
    0x000000007fffffff,
    0x0000000080000000,
    0x00000000ffffffff,
):
    print 'Foo ({:#018x}) = {:#010x}'.format(a, Foo(a))
