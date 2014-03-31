from ctypes import *
from gnatllvm import build_and_load, Func

(Foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('_ada_foo', argtypes=[c_bool], restype=c_bool),
)

for i in (False, True):
    print 'Foo ({}) = {}'.format(i, Foo(i))
