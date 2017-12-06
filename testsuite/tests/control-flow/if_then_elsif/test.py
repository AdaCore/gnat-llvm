from ctypes import *
from gnatllvm import build_and_load, Func

(equal, ) = build_and_load(
    ['equal.adb'], 'equal',
    Func('_ada_equal', argtypes=[c_int, c_int], restype=c_int),
)

assert equal(1, 1) == 1
assert equal(1, 2) == 0
