from ctypes import *
from gnatllvm import build_and_load, Func

(Call_A, Call_B, ) = build_and_load(
    ['a.adb', 'b.adb'], 'cross_references',
    Func('b__call_a', argtypes=[c_uint], restype=c_uint),
    Func('a__call_b', argtypes=[c_int], restype=c_int),
)

print 'Call_A ({}) = {}'.format(2, Call_A(2))
print 'Call_B ({}) = {}'.format(2, Call_B(2))
