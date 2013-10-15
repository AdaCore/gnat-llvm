from ctypes import *
from gnatllvm import build_and_load, Func

(return_one, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('return_one', argtypes=[], restype=c_int),
)

assert return_one() == 1
