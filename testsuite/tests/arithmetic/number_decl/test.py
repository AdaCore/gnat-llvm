from ctypes import *
from gnatllvm import build_and_load, Func

(Return_One, ) = build_and_load(
    ['return_one.adb'], 'return_one',
    Func('_ada_return_one', argtypes=[], restype=c_int),
)

print('Return_One = {}'.format(Return_One()))
