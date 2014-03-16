from ctypes import *
from gnatllvm import build_and_load, Func

(Foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('_ada_foo', argtypes=[c_bool, c_uint32, c_uint32], restype=c_uint32),
)

uint32_max = 2 ** 32 - 1

for compute_max, x, y in (
    (True,  0, 0),
    (True,  0, 1),
    (True,  1, 0),
    (True,  0, uint32_max),
    (False, 0, 0),
    (False, 0, 1),
    (False, 1, 0),
    (False, 0, uint32_max),
):
    print('Foo ({}, {}, {}) = {}'.format(
        compute_max, x, y,
        Foo(compute_max, x, y)
    ))
