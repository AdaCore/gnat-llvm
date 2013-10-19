from ctypes import *
from gnatllvm import build_and_load, Func

(return_a_plus_b, ) = build_and_load(
    ['return_a_plus_b.adb'], 'return_a_plus_b',
    Func('_ada_return_a_plus_b', argtypes=[], restype=c_int),
)

assert return_a_plus_b(1, 2) == 3
