from ctypes import *
from gnatllvm import build_and_load, Func

(seconds_per_days, ) = build_and_load(
    ['seconds_per_days.adb'], 'seconds_per_days',
    Func('seconds_per_days', argtypes=[c_uint], restype=c_uint),
)

assert seconds_per_days(3) == 3 * 24 * 60 * 60
