from ctypes import *
from gnatllvm import build_and_load, Func

(if_then_sclo, ) = build_and_load(
    ['if_then_sclo.adb'], 'if_then_sclo',
    Func('_ada_if_then_sclo', argtypes=[c_int], restype=c_int),
)

assert if_then_sclo(0) == 0
assert if_then_sclo(1) == 2
assert if_then_sclo(-1) == -1
