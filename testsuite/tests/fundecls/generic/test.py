from ctypes import *
from gnatllvm import build_and_load, Func

(Foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('_ada_foo', argtypes=[c_int], restype=c_int),
)

for i in (-1, 0, 1, 4, 8):
    print('Foo ({}) = {}'.format(i, Foo(i)))
