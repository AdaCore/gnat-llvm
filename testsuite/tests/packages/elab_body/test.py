from ctypes import *
from gnatllvm import build_and_load, Func

(elabb, value, ) = build_and_load(
    ['pak.adb'], 'pak',
    Func('pak___elabb', argtypes=[], restype=None),
    Func('pak__value', argtypes=[], restype=c_int),
)

elabb()
assert value() == 42
