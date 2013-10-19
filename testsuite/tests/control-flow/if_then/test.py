from ctypes import *
from gnatllvm import build_and_load, Func

(absolute, ) = build_and_load(
    ['absolute.adb'], 'absolute',
    Func('_ada_absolute', argtypes=[c_int], restype=c_int),
)

assert absolute(0) == 0
assert absolute(1) == 1
assert absolute(-1) == 1
