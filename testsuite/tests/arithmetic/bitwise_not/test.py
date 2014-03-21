from ctypes import *
from gnatllvm import build_and_load, Func

(Foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('_ada_foo', argtypes=[c_uint32], restype=c_uint32),
)

for a in (
    0x00000000,
    0x00000001,
    0x7fffffff,
    0x80000000,
    0xffffffff,
):
    print 'Foo ({:#010x}) = {:#010x}'.format(a, Foo(a))
