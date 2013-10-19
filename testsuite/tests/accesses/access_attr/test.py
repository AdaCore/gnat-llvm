from ctypes import *
from gnatllvm import build_and_load, Func

(access_attr, ) = build_and_load(
    ['access_attr.adb'], 'access_attr',
    Func('_ada_access_attr', argtypes=[c_int], restype=c_int),
)

assert access_attr(3) == 4
