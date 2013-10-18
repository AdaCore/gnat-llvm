from ctypes import *
from gnatllvm import build_and_load, Func

(mul, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('foo__mul', argtypes=[], restype=c_int),
)

assert mul(2) == 4
assert mul(12) == (12 * 2) % 17
