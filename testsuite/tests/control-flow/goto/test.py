from ctypes import *
from gnatllvm import build_and_load, Func

(absolute, ) = build_and_load(
    ['absolute.adb'], 'absolute',
    Func('absolute', argtypes=[c_int], restype=c_int),
)

for i in (-100, -2, -1, 0, 1, 2, 100):
    print 'absolute({}) = {}'.format(i, absolute(i))
