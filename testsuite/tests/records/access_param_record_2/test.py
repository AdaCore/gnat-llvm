from ctypes import *
from gnatllvm import build_and_load, Func

(f, ) = build_and_load(
    ['access_param_record.adb'], 'access_param_record',
    Func('access_param_record__f', argtypes=[], restype=c_int),
)

assert f() == 13
