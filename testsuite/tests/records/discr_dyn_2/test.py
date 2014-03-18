from ctypes import *
from gnatllvm import build_and_load, Func

(f, ) = build_and_load(
    ['discr_dyn.adb'], 'discr_dyn',
    Func('discr_dyn__foo', argtypes=[], restype=c_int),
)

print f()
