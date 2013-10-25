from ctypes import *
from gnatllvm import build_and_load, Func

(_sum, ) = build_and_load(
    ['sum.adb'], 'sum',
    Func('_ada_sum', argtypes=[c_int], restype=c_int),
)

print _sum(6)
