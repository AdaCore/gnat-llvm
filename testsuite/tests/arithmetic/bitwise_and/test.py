from ctypes import *
from gnatllvm import build_and_load, Func

(Foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('_ada_foo', argtypes=[c_uint32, c_uint32], restype=c_uint32),
)

for a, b in (
    (0x00000000, 0x00000000),
    (0x00000001, 0x00000000),
    (0x00000001, 0x00000001),
    (0xaaaaaaaa, 0x00000000),
    (0xaaaaaaaa, 0x55555555),
    (0xaaaaaaaa, 0xffffffff),
    (0xffffffff, 0x00000000),
    (0xffffffff, 0x00000001),
    (0xffffffff, 0xffffffff),
):
    print 'Foo ({:#010x}, {:#010x}) = {:#010x}'.format(a, b, Foo(a, b))
