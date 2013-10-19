from ctypes import *
from gnatllvm import build_and_load, Func

(inc, ) = build_and_load(
    ['inc.adb'], 'inc',
    Func('_ada_inc', argtypes=[POINTER(c_int)], restype=None),
)

c = c_int(1)
inc (c)
assert c.value == 2
