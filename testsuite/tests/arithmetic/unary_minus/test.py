from ctypes import *
from gnatllvm import build_and_load, Func

(opposite, ) = build_and_load(
    ['opposite.adb'], 'opposite',
    Func('_ada_opposite', argtypes=[c_int], restype=c_int),
)

for i in (-1, 0, 1):
    print 'opposite({}) = {}'.format(i, opposite(i))
