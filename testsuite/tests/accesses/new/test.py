from ctypes import *
from gnatllvm import build_and_load, Func

(n, ) = build_and_load(
    ['n.adb'], 'n',
    Func('_ada_n', argtypes=[], restype=c_int),
)

print n()
