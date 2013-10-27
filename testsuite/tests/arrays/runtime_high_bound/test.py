from ctypes import *
from gnatllvm import build_and_load, Func

(runtime_size, ) = build_and_load(
    ['runtime_size.adb'], 'runtime_size',
    Func('_ada_runtime_size', argtypes=[], restype=c_int),
)

print runtime_size(25)
