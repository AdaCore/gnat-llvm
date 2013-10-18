from ctypes import *
from gnatllvm import build_and_load, Func

(mul, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('foo__mul', argtypes=[], restype=c_int),
)

assert mul(2) == 6
assert mul(45) == (45* 3) % 64
