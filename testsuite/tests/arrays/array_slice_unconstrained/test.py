from ctypes import *
from gnatllvm import build_and_load, Func

(baz, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('foo__baz', argtypes=[], restype=c_int),
)

print baz()
