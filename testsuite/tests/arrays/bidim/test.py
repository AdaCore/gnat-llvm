from ctypes import *
from gnatllvm import build_and_load, Func

(bidim, ) = build_and_load(
    ['bidim.adb'], 'bidim',
    Func('_ada_bidim', argtypes=[], restype=c_int),
)

print bidim()
