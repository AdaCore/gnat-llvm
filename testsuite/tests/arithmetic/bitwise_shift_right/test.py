from ctypes import *
from gnatllvm import build_and_load, Func

(Foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('_ada_foo', argtypes=[c_uint32, c_uint], restype=c_uint32),
)

for a, n in (
    (0x00000000, 0),
    (0x00000000, 1),
    (0x00000000, 32),

    (0x00000001, 0),
    (0x00000001, 1),
    (0x00000001, 31),
    (0x00000001, 32),

    (0x12345678, 0),
    (0x12345678, 4),
    (0x12345678, 1),
    (0x12345678, 31),
    (0x12345678, 32),
):
    print 'Foo ({:#010x}, {}) = {:#010x}'.format(a, n, Foo(a, n))
