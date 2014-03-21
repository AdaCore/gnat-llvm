from ctypes import *
from gnatllvm import build_and_load, Func

(Foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('_ada_foo', argtypes=[c_int, c_int], restype=c_int),
)

for a, b in (
    (-6, 5),
    (-5, 5),
    (-4, 5),
    (-1, 5),
    (0,  5),
    (1,  5),
    (4,  5),
    (5,  5),
    (6,  5),
    (-6, -5),
    (-5, -5),
    (-4, -5),
    (-1, -5),
    (0,  -5),
    (1,  -5),
    (4,  -5),
    (5,  -5),
    (6,  -5),
):
    print 'Foo ({}, {}) = {}'.format(a, b, Foo(a, b))
