from ctypes import *
from gnatllvm import build_and_load, Func

c_uint_p = POINTER(c_uint)

(silly, ) = build_and_load(
    ['silly.adb'], 'silly',
    Func('_ada_silly', argtypes=[c_uint_p], restype=None),
    gargs=['-gnatws']
)

n = c_uint(1)
silly(byref(n))
assert n.value == 2
