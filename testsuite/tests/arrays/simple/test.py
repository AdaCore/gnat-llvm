from ctypes import *
from gnatllvm import build_and_load, Func

(simple, ) = build_and_load(
    ['simple.adb'], 'simple',
    Func('_ada_simple', argtypes=[], restype=c_int),
)

print simple()
