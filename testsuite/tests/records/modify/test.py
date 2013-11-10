from ctypes import *
from gnatllvm import build_and_load, Func

(foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('_ada_foo', argtypes=[c_int, c_int], restype=c_int),
)

print foo(1, 2)
print foo(4, 5)
