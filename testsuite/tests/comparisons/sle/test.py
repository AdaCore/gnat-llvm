from ctypes import *
from gnatllvm import build_and_load, Func

(compare, ) = build_and_load(
    ['compare.adb'], 'compare',
    Func('_ada_compare', argtypes=[c_int, c_int], restype=c_bool),
)

assert compare(1,   2) == True
assert compare(1,   1) == True
assert compare(2,   1) == False

assert compare(-1,  1) == True
assert compare(-1, -1) == True
assert compare(1,  -1) == False
