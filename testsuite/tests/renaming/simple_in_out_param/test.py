from ctypes import *
from gnatllvm import build_and_load, Func

c_int_p = POINTER(c_int)

(Foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('_ada_foo', argtypes=[c_int_p], restype=None),
)

for i in range(3):
    I = c_int(i)
    Foo(byref(I))
    print 'Foo ({}) = {}'.format(i, I.value)
