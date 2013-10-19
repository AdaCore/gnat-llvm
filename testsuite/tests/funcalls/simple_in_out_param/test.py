from ctypes import *
from gnatllvm import build_and_load, Func

c_int_p = POINTER(c_int)

(absolute, ) = build_and_load(
    ['absolute.adb'], 'absolute',
    Func('_ada_absolute', argtypes=[c_int_p], restype=None),
)

def compute_abs(i):
    i = c_int(i)
    absolute(byref(i))
    return i.value

assert compute_abs(0) == 0
assert compute_abs(1) == 1
assert compute_abs(-1) == 1
