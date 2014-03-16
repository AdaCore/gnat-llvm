from ctypes import *
from gnatllvm import build_and_load, Func

(Foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('_ada_foo', argtypes=[c_int], restype=c_int),
)

for i, j in (
    (0, 2),
    (0, 10),
):
    print 'Foo ({}, {}) = {}'.format(i, j, Foo(i, j))
