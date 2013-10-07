from ctypes import *
from gnatllvm import build_and_load, Func

(return_minus_one, ) = build_and_load(
    ['return_minus_one.adb'], 'return_minus_one',
    Func('return_minus_one', argtypes=[], restype=c_int),
)

assert return_minus_one() == -1
