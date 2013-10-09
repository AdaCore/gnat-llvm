from ctypes import *
from gnatllvm import build_and_load, Func

(boolean_ops, ) = build_and_load(
    ['boolean_ops.adb'], 'boolean_ops',
    Func('boolean_ops', argtypes=[c_int, c_int], restype=c_int),
)

print boolean_ops(1, 2)
assert boolean_ops(1, 2) == 1
assert boolean_ops(1, 1) == 0
assert boolean_ops(-1, -2) == 0
assert boolean_ops(-2, -1) == 1
