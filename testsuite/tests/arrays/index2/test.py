from ctypes import *
from gnatllvm import build_and_load, Func

(index2, ) = build_and_load(
    ['index2.adb'], 'simple',
    Func('_ada_index2', argtypes=[c_int, c_int], restype=c_int),
)

print index2 (1, 1)
print index2 (1, 2)
print index2 (2, 1)
print index2 (2, 2)

