from ctypes import *
from gnatllvm import build_and_load, Func

(Foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('_ada_foo', argtypes=[c_int, c_bool], restype=c_int),
)

for a, cond1, cond2 in (
    # Then_Return
    (-1, True, False),
    (0,  True, False),
    (1,  True, False),

    # Elsif_Return
    (-1, False, True),
    (0,  False, True),
    (1,  False, True),

    # Else_Return
    (-1, False, False),
    (0,  False, False),
    (1,  False, False),
):
    print 'Foo ({}, {}, {}) = {}'.format(
        a, cond1, cond2,
        Foo(a, cond1, cond2)
    )
