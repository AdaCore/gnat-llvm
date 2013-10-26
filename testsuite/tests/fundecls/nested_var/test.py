from ctypes import *
from gnatllvm import build_and_load, Func

(Abs_Sum, ) = build_and_load(
    ['abs_sum.adb'], 'abs_sum',
    Func('_ada_abs_sum', argtypes=[c_int], restype=c_int),
)

for i, j in (
   (-1, -1),
   (-1, 0),
   (-1, 1),
   (0, -1),
   (0, 0),
   (0, 1),
   (1, -1),
   (1, 0),
   (1, 1),
):
    print 'Abs_Sum ({}, {}) = {}'.format(i, j, Abs_Sum(i, j))
