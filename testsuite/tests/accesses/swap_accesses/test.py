from ctypes import *
from gnatllvm import build_and_load, Func

(foo, ) = build_and_load(
    ['swap_ptrs.adb'], 'swap_ptrs',
    Func('swap_ptrs__foo', argtypes=[], restype=c_int),
)

print foo()
