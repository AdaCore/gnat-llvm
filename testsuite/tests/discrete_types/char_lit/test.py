from ctypes import *
from gnatllvm import build_and_load, Func

(charlit, ) = build_and_load(
    ['charlit.adb'], 'charlit',
    Func('_ada_charlit', argtypes=[], restype=c_char),
)

print charlit()
