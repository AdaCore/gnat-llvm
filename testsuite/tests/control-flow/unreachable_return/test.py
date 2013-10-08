from ctypes import *
from gnatllvm import build_and_load, Func

(silly, ) = build_and_load(
    ['silly.adb'], 'silly',
    Func('silly', argtypes=[c_uint, c_uint], restype=c_uint),
    gargs=['-gnatws']
)

assert silly(1, 2) == 3
