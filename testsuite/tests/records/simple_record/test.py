from ctypes import *
from gnatllvm import build_and_load, Func

(simple_record, ) = build_and_load(
    ['simple_record.adb'], 'simple_record',
    Func('simple_record', argtypes=[c_int, c_int], restype=c_int),
)

assert simple_record(1, 2) == 3
assert simple_record(4, 5) == 9
