from ctypes import *
from gnatllvm import build_and_load, Func

(foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('_ada_foo', argtypes=[], restype=c_int),
)

print foo()
