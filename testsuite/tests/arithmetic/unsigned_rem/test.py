from ctypes import *
from gnatllvm import build_and_load, Func

(Foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('_ada_foo', argtypes=[c_uint, c_uint], restype=c_uint),
)

for a, b in (
    (0,  5),
    (1,  5),
    (4,  5),
    (5,  5),
    (6,  5),
):
    print 'Foo ({}, {}) = {}'.format(a, b, Foo(a, b))
