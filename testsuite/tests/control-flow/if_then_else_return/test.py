from ctypes import *
from gnatllvm import build_and_load, Func

(alu, ) = build_and_load(
    ['alu.adb'], 'alu',
    Func('alu', argtypes=[c_bool, c_uint, c_uint], restype=c_uint),
)

assert alu(True, 2, 3) == 5
assert alu(False, 2, 3) == 6
