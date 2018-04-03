from ctypes import *
from gnatllvm import build_and_load, Func

(Foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('_ada_foo', argtypes=[c_bool, c_bool, c_int], restype=c_int),
)

for modrem, sign, a in (
    (True, False, -6),
    (True, False, -5),
    (True, False, -4),
    (True, False, -1),
    (True, False, 0),
    (True, False, 1),
    (True, False, 4),
    (True, False, 5),
    (True, False, 6),
    (True, True, -6),
    (True, True, -5),
    (True, True, -4),
    (True, True, -1),
    (True, True, 0),
    (True, True, 1),
    (True, True, 4),
    (True, True, 5),
    (True, True, 6),
    (False, False, -6),
    (False, False, -5),
    (False, False, -4),
    (False, False, -1),
    (False, False, 0),
    (False, False, 1),
    (False, False, 4),
    (False, False, 5),
    (False, False, 6),
    (False, True, -6),
    (False, True, -5),
    (False, True, -4),
    (False, True, -1),
    (False, True, 0),
    (False, True, 1),
    (False, True, 4),
    (False, True, 5),
    (False, True, 6),
):
    print 'Foo ({}, {}, {}) = {}'.format(modrem, sign, a, Foo(modrem, sign, a))
