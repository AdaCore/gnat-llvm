from ctypes import *
from gnatllvm import build_and_load, Func

(Foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('_ada_foo', argtypes=[c_int, c_bool], restype=c_int),
)

for a, cond in (
    (-1, False),
    (0,  False),
    (1,  False),
    (-1, True),
    (0,  True),
    (1,  True),
):
    print 'Foo ({}, {}) = {}'.format(a, cond, Foo(a, cond))
