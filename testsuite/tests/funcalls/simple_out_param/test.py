from ctypes import *
from gnatllvm import build_and_load, Func

c_int_p = POINTER(c_int)

(set_one, ) = build_and_load(
    ['set_one.adb'], 'set_one',
    Func('_ada_set_one', argtypes=[c_int_p], restype=None),
)

i = c_int(0)
set_one(byref(i))
assert i.value == 1

i = c_int(-1)
set_one(byref(i))
assert i.value == 1
