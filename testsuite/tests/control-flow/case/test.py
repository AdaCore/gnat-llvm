from ctypes import *
from gnatllvm import build_and_load, Func

(case, ) = build_and_load(
    ['simple_case.adb'], 'simple_case',
    Func('_ada_simple_case', argtypes=[c_int], restype=c_int),
)

for i in (0, 1, 2, 3, 4):
    print 'case({}) = {}'.format(i, case(i))
