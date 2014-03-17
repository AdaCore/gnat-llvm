from ctypes import *
from gnatllvm import build_and_load, Func

(test, ) = build_and_load(
    ['test.adb'], 'test',
    Func('test__test', argtypes=[], restype=c_int),
)

print test()
