from ctypes import *
from gnatllvm import build_and_load, Func

(fact, ) = build_and_load(
    ['fact.adb'], 'fact',
    Func('fact', argtypes=[c_uint], restype=c_uint),
)

assert fact(0) == 1
assert fact(1) == 1
assert fact(2) == 2
assert fact(4) == 24
assert fact(6) == 720
