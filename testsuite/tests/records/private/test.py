from ctypes import *
from gnatllvm import build_and_load, Func

(main, ) = build_and_load(
    ['main.adb', 'stack.adb'], 'main',
    Func('_ada_main', argtypes=[], restype=c_int),
)

print main()
