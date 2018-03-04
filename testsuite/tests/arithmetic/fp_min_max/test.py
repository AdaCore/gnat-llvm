from ctypes import *
from gnatllvm import build_and_load, Func

(Foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('_ada_foo', argtypes=[c_bool, c_float, c_float], restype=c_float),
)

for compute_max, x, y in (
    (True,  -1, -1),
    (True,   1, -1),
    (True,  -1,  1),
    (False, -1, -1),
    (False,  1, -1),
    (False, -1,  1),
):
    print('Foo ({}, {}, {}) = {}'.format(
        compute_max, x, y,
        Foo(compute_max, x, y)
    ))
