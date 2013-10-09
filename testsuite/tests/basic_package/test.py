from ctypes import *
from gnatllvm import build_and_load, Func

(double, ) = build_and_load(
    ['pak.adb'], 'pak',
    Func('pak__double', argtypes=[c_int], restype=c_int),
)

assert double(2) == 4
