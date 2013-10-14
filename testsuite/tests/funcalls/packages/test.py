from ctypes import *
from gnatllvm import build_and_load, Func

(quadruple, ) = build_and_load(
    ['pak.adb', 'pak2.adb'], 'pak',
    Func('pak__quadruple', argtypes=[c_int], restype=c_int),
)

assert quadruple(1) == 4
assert quadruple(4) == 16
