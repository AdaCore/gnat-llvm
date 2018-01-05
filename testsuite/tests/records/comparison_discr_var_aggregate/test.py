from ctypes import *
from gnatllvm import build_and_load, Func

(Foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func(
        '_ada_foo',
        argtypes=[c_uint32, c_uint32, c_bool],
        restype=c_bool
    ),
)

for a, b, c, d, e, f in (
    (1, 2, 3, 1, 2, 3),
    (1, 2, 3, 4, 2, 3),
    (1, 2, 3, 1, 4, 3),
    (1, 2, 3, 1, 2, 4),
):
    # At the moment, passing more than 4 arguments seem to cause trouble. Not
    # sure if it's because of ctype or not.

    bits_per_arg = bpa = 4
    first = (a << bpa * 2) | (b << bpa) | c
    second = (d << bpa * 2) | (e << bpa) | f

    for equality in (False, True):
        print(
            'Foo(\n'
            '    A => {}, B => {}, C => {},\n'
            '  --> {:0x}\n'
            '    D => {}, E => {}, C => {},\n'
            '  --> {:0x}\n'
            '    Equality => {})\n'
            '  = {}'.format(
                a, b, c, first,
                d, e, f, second,
                equality,
                Foo (first, second, equality)
            )
        )
    print('')
