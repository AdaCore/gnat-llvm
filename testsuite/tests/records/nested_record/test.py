from ctypes import *
from gnatllvm import build_and_load, Func

(nested_record, ) = build_and_load(
    ['nested_record.adb'], 'nested_record',
    Func('_ada_nested_record', argtypes=[], restype=c_int),
)

assert nested_record() == 12
