from ctypes import *
from gnatllvm import build_and_load, Func

(or_else, ) = build_and_load(
    ['or_else.adb'], 'or_else',
    Func('or_else', argtypes=[c_uint], restype=c_uint),
)

assert or_else(0, 0) == 1
assert or_else(1, 0) == 1
assert or_else(0, 1) == 1
assert or_else(1, 1) == 0
