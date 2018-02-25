from ctypes import *
from gnatllvm import build_and_load, Func

(test, ) = build_and_load(
    ['test.adb'], 'compare',
    Func('test__test', argtypes=[c_int], restype=c_int),
)

assert test(9) == 1
assert test(10) == 11
assert test(20) == 11
assert test(15) == 11
assert test(30) == 1

