from ctypes import *
from gnatllvm import build_and_load, Func

(f, ) = build_and_load(
    ['out_param_record.adb'], 'out_param_record',
    Func('out_param_record__f', argtypes=[], restype=c_int),
)

assert f() == 13
