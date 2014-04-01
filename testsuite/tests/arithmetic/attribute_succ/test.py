from ctypes import *
from gnatllvm import build_and_load, Func

(Foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('_ada_foo', argtypes=[c_int], restype=c_int),
)

for a in (-10, -1, 0, 1, 10):
    print 'Foo ({}) = {}'.format(a, Foo(a))
