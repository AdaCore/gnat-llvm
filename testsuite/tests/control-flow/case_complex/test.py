from ctypes import *
from gnatllvm import build_and_load, Func

(case, ) = build_and_load(
    ['complex_case.adb'], 'complex_case',
    Func('_ada_complex_case', argtypes=[c_int], restype=c_int),
)

for i in (0, 10, 20, 32, 40):
    print 'case({}) = {}'.format(i, case(i))
