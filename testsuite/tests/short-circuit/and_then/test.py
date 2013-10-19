from ctypes import *
from gnatllvm import build_and_load, Func

(and_then, ) = build_and_load(
    ['and_then.adb'], 'and_then',
    Func('_ada_and_then', argtypes=[c_uint], restype=c_uint),
)

assert and_then(0, 0) == 1
assert and_then(1, 0) == 0
assert and_then(0, 1) == 0
