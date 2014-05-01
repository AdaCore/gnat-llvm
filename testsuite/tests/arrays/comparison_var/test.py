from ctypes import *
from gnatllvm import build_and_load, Func

(Foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func(
        '_ada_foo',
        argtypes=[c_int, c_int, c_int, c_int, c_bool],
        restype=c_bool
    ),
)

for a, b, c, d in (
    (1, 2, 1, 2),
    (1, 2, 1, 3),
    (1, 2, 3, 2),
):
    for equality in (False, True):
        print(
            'Foo(A => {}, B => {}, C => {}, D => {},'
            ' Equality => {}) = {}'.format(
                a, b, c, d, equality,
                Foo(a, b, c, d, equality)
            )
        )
    print('')
